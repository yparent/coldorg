import os
import fitz  # PyMuPDF for PDF text extraction
import faiss
import numpy as np
from sentence_transformers import SentenceTransformer
from langchain.text_splitter import RecursiveCharacterTextSplitter
from tqdm import tqdm

from helpers import get_a_response  

class RAGPipeline:
    def __init__(self, pdf_path):
        """
        Initializes the RAG system with a French BERT model for embeddings,
        FAISS for retrieval, and an API call for response generation.
        """
        self.pdf_path = pdf_path  # Single PDF file
    
        # Load a French BERT model for embeddings
        self.bert_model = SentenceTransformer("dangvantuan/sentence-camembert-base")

        # Initialize FAISS Index
        self.index = None
        self.texts = []
        self.metadata = []  # Stores page numbers

    def extract_text_from_pdf(self):
        """Extracts text from a single long French PDF file."""
        doc = fitz.open(self.pdf_path)
        extracted_text = []
        metadata = []

        for page_num, page in enumerate(doc):
            text = page.get_text("text")
            if text.strip():
                extracted_text.append(text)
                metadata.append({"page": page_num + 1})  # Store page number

        return extracted_text, metadata

    def get_embedding(self, text):
        """Generates text embeddings using the French BERT model."""
        return self.bert_model.encode(text, convert_to_numpy=True)

    def build_vector_store(self):
        """Extracts text from a single PDF, splits it, generates embeddings, and stores them in FAISS."""
        text_splitter = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=100)

        extracted_text, page_metadata = self.extract_text_from_pdf()
        chunks = []
        chunk_metadata = []

        # Process text chunks
        for i, text in enumerate(extracted_text):
            split_chunks = text_splitter.split_text(text)
            chunks.extend(split_chunks)
            chunk_metadata.extend([{ "page": page_metadata[i]["page"] }] * len(split_chunks))

        # Store for retrieval
        self.texts = chunks
        self.metadata = chunk_metadata

        # Generate embeddings
        print("Generating embeddings ...")
        embeddings = np.array([self.get_embedding(chunk) for chunk in tqdm(self.texts)])

        # Create FAISS index
        dimension = embeddings.shape[1]
        self.index = faiss.IndexFlatL2(dimension)
        self.index.add(embeddings)
        faiss.write_index(self.index, "vector_store.index")

        # Save texts and metadata for future queries
        np.save("texts.npy", np.array(self.texts, dtype=object))
        np.save("metadata.npy", np.array(self.metadata, dtype=object))
        print("Texts and metadata saved successfully!")

    def load_texts_and_metadata(self):
        """Loads stored texts and metadata from disk after FAISS index is loaded."""
        if os.path.exists("texts.npy") and os.path.exists("metadata.npy"):
            print("Loading texts and metadata from disk...")
            self.texts = np.load("texts.npy", allow_pickle=True).tolist()
            self.metadata = np.load("metadata.npy", allow_pickle=True).tolist()
            print("Texts and metadata loaded successfully!")
        else:
            raise FileNotFoundError("Stored texts/metadata not found. Please run build_vector_store() first.")

    def search_faiss(self, query, top_k=5):
        """Retrieves relevant chunks from FAISS based on the query."""
        print("Starting the search")
        
        # Load FAISS index if not already in memory
        if self.index is None:
            if os.path.exists("vector_store.index"):
                self.index = faiss.read_index("vector_store.index")
                self.load_texts_and_metadata()
            else:
                self.build_vector_store()
                
                

        query_embedding = self.get_embedding(query).reshape(1, -1)
        distances, indices = self.index.search(query_embedding, top_k)

        results = []
        for idx in indices[0]:
            if idx < len(self.texts):  
                results.append({"text": self.texts[idx], "page": self.metadata[idx]["page"]})
        
        return results
    
    def generate_response(self, query, retrieved_docs, history=[]):
        """Generates a response using the Scaleway API based on retrieved chunks in French."""
        context = "\n\n".join([f"[Page {doc['page']}]: {doc['text']}" for doc in retrieved_docs])
        
        # prompt = (
        # f"Vous êtes un assistant dans la plomberie.\n"
        # f"Répondez en français à la question suivante en vous appuyant sur le contexte.\n\n"
        # f"Question: {query}\n\n"
        # f"Contexte:\n{context}\n\n"
        # f"Réponse:"
        # )

        prompt = (
        f"Tu es Coldbot, un assistant expert en systèmes de réfrigération et climatisation, doté d'une mémoire conversationnelle. Tu maintiens une conversation naturelle et contextualisée avec les techniciens\n"
        f"Réponds en français à la question suivante en vous appuyant sur le contexte.\n\n"
        f"Il est interdit de répondre sur les questions hors la réfrigération et climatisation \n\n"
        f"Merci de donner la réponse comme une liste si neccéssaire \n\n"
        f"Merci de considérer que les mots clés dans la question pour chercher dans le contexte\n\n"
        f"Question: {query}\n\n"
        f"Contexte:\n{context}\n\n"
        f"Réponse:"
        )

        response = get_a_response(prompt=prompt, history=history)
        return response

    def query_pipeline(self, user_query, history=[]):
        """Executes the full RAG pipeline for a given query."""
        retrieved_docs = self.search_faiss(user_query)
        response = self.generate_response(user_query, retrieved_docs, history=history)
        
        return {
            "retrieved_docs": [doc["page"] for doc in retrieved_docs],
            "response": response
        }

if __name__ == "__main__":
    pdf_path = "./data/merged/merged.pdf"
    rag_system = RAGPipeline(pdf_path)

    user_query = "Comment on calcule le sous-refroidissement ?"
    result = rag_system.query_pipeline(user_query)

    # print("\n Documents récupérés:")
    # for doc in result["retrieved_docs"]:
    #     print(f"- [Page {doc['page']}] {doc['text']}")

    print("\n Réponse de l'assistant:\n", result["response"])
    print("\n Les pages trouvées :\n", result["retrieved_docs"])










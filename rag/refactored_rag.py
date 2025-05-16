 # PyMuPDF for PDF text extraction
import numpy as np
from langchain.text_splitter import RecursiveCharacterTextSplitter
from tqdm import tqdm


from helpers import get_a_response

from embeddings import EmbeddingModel, CamemBERTEmbedding
from pdf_extractors import PDFExraction, PyMuPDFExtraction
from stores import VectorStore, FAISSVectorStore



class RAGPipeline:
    def __init__(self, extraction_strategy: PDFExraction, embedding_model: EmbeddingModel, vector_store: VectorStore):
        """
        Initializes the RAG system with modular embedding and vector storage strategies.
        """
        self.extraction_strategy = extraction_strategy
        self.embedding_model = embedding_model
        self.vector_store = vector_store


    def build_vector_store(self):
        """Extracts text, splits it, generates embeddings, and stores them in the vector store."""
        text_splitter = RecursiveCharacterTextSplitter(chunk_size=500, chunk_overlap=200)
        extracted_text, page_metadata = self.extraction_strategy.extract_text()

        chunks, chunk_metadata = [], []
        for i, text in enumerate(extracted_text):
            split_chunks = text_splitter.split_text(text)
            chunks.extend(split_chunks)
            chunk_metadata.extend([{ "page": page_metadata[i]["page"] }] * len(split_chunks))

        self.vector_store.build_index(chunks, chunk_metadata, self.embedding_model)

    def search(self, query, top_k=5):
        """Retrieves relevant chunks from the vector store."""
        if self.vector_store.index is None:
            self.vector_store.load_index()
        
        query_embedding = self.embedding_model.get_embedding(query).reshape(1, -1)
        indices = self.vector_store.search(query_embedding, top_k)
        
        return [{"text": self.vector_store.texts[idx], "page": self.vector_store.metadata[idx]["page"]} for idx in indices[0] if idx < len(self.vector_store.texts)]
    
    def generate_response(self, query, retrieved_docs, history=[]):
        """Generates a response using an API call based on retrieved chunks."""
        context = "\n\n".join([f"[Page {doc['page']}]: {doc['text']}" for doc in retrieved_docs])
        prompt = (
        f"Vous êtes un assistant dans la plomberie.\n"
        f"Répondez en français à la question suivante en vous appuyant sur le contexte.\n\n"
        f"Question: {query}\n\n"
        f"Contexte:\n{context}\n\n"
        f"Réponse:")

        return get_a_response(prompt, history=history)

    def query_pipeline(self, user_query, history=[]):
        """Executes the full RAG pipeline for a given query."""
        retrieved_docs = self.search(user_query)
        response = self.generate_response(user_query, retrieved_docs, history=history)
        
        return {
            "retrieved_docs": [doc["page"] for doc in retrieved_docs],
            "response": response
        }

if __name__ == "__main__":
    pdf_path = "./data/merged/merged.pdf"


    extraction_strategy = PyMuPDFExtraction(pdf_path= pdf_path)

    embedding_model = CamemBERTEmbedding()
    vector_store = FAISSVectorStore()
    
    rag_system = RAGPipeline(extraction_strategy=extraction_strategy, embedding_model= embedding_model, vector_store= vector_store)
    rag_system.build_vector_store()
    
    user_query = "j'ai une panne sur une vanne comment la régler ?"
    result = rag_system.query_pipeline(user_query)
    
    print("\nRéponse de l'assistant:\n", result["response"])

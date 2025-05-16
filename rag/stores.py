import os
import numpy as np
from abc import ABC, abstractmethod
import tqdm

# Interface for Vector Stores
class VectorStore(ABC):
    def __init__(self):
        self.index = None
        self.texts = []
        self.metadata = []

    @abstractmethod
    def build_index(self, texts, metadata, embedding_model):
        pass
    
    @abstractmethod
    def search(self, query_embedding, top_k):
        pass

class FAISSVectorStore(VectorStore):
    def build_index(self, texts, metadata, embedding_model):
        import faiss
        print("Generating embeddings...")
        embeddings = np.array([embedding_model.get_embedding(chunk) for chunk in tqdm(texts)])
        
        dimension = embeddings.shape[1]
        self.index = faiss.IndexFlatL2(dimension)
        self.index.add(embeddings)
        faiss.write_index(self.index, "vector_store.index")
        
        self.texts = texts
        self.metadata = metadata
        np.save("texts.npy", np.array(self.texts, dtype=object))
        np.save("metadata.npy", np.array(self.metadata, dtype=object))
        print("Vector store built and saved successfully!")

    def load_index(self):
        import faiss
        if os.path.exists("vector_store.index"):
            self.index = faiss.read_index("vector_store.index")
            self.texts = np.load("texts.npy", allow_pickle=True).tolist()
            self.metadata = np.load("metadata.npy", allow_pickle=True).tolist()
        else:
            raise FileNotFoundError("FAISS index not found. Please build the vector store first.")

    def search(self, query_embedding, top_k=5):
        distances, indices = self.index.search(query_embedding, top_k)
        return indices

from abc import ABC, abstractmethod
from sentence_transformers import SentenceTransformer


# Interface for Embedding Models
class EmbeddingModel(ABC):
    @abstractmethod
    def get_embedding(self, text):
        pass

class CamemBERTEmbedding(EmbeddingModel):
    def __init__(self):
        self.model = SentenceTransformer("dangvantuan/sentence-camembert-base")

    def get_embedding(self, text):
        return self.model.encode(text, convert_to_numpy=True)
    

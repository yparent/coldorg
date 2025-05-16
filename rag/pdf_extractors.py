from abc import ABC, abstractmethod

class PDFExraction(ABC):
    def __init__(self, pdf_path):
        self.pdf_path = pdf_path

    @abstractmethod
    def extract_text(self):
        pass


class PyMuPDFExtraction(PDFExraction):
      
      def extract_text(self):
          import fitz
          doc = fitz.open(self.pdf_path)
          extracted_text, metadata = [], []

          for page_num, page in enumerate(doc):
              text = page.get_text("text")
              if text.strip():
                 extracted_text.append(text)
                 metadata.append({"page": page_num + 1})

            
          return extracted_text, metadata
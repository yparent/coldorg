# library(dplyr)
# library(pdftools)
# library(tesseract)

#=================================
source("./R/pdf_helpers.R")
#=================================

lang <- "fra"

ocr_engine_infos <- tesseract::tesseract_info()
if(!lang %in% ocr_engine_infos$available){
  tesseract::tesseract_download(lang = "fra", model = "best")
}

pdf_file_path <- "./inst/rawdata/V7-2023 (1).pdf"


tictoc::tic()
pdf_text <- pdftools::pdf_ocr_text(
  pdf = pdf_file_path,
  pages = 1:20,
  dpi  = 1000,
  language = lang)
tictoc::toc()

bb <- clean_all_pdfs(ocr_pdf_results = list(pdf_text = pdf_text))

# cc <- pdftools::pdf_ocr_data(pdf = "../inst/rawdata/V7-2023 (1).pdf", pages = 5:8, language = "fra")
# bb <- metagear::PDF_extractImages(file = "../inst/rawdata/V7-2023 (1).pdf")





# ocr_pdf_results_cleaned <- clean_all_pdfs(ocr_pdf_results)

# aa <- metagear::PDF_extractImages(file = list_fo_pdfs[2])
# bb <- pdftools::pdf_text(pdf = list_fo_pdfs)

#tesseract::tesseract_download(lang = "fra", model = "best")

# pak::pak("sckott/pdfimager")

# pdfimager::pdimg_set_path()
# Sys.setenv(PDFIMAGER_PATH="C:/some/path/to/poppler/24/bin/pdfimages.exe")
# pdf_imgs <- pdfimager::pdimg_images(list_fo_pdfs)

convert_pdf_to_md <- function(ocr_dir = "./ocr_extract", raw_dir = "../inst/rawdata/"){
  # dir.create(ocr_dir, recursive = T)
  # setwd(ocr_dir)

  list.files(ocr_dir, pattern = ".png", full.names = T)%>%
    file.remove()
  list_fo_pdfs <- list.files(raw_dir, pattern = ".pdf", full.names = T)
  names(list_fo_pdfs) <- list.files(raw_dir, pattern = ".pdf", full.names = F)%>%gsub(".pdf","",.)

  ocr_pdf_results <- list_fo_pdfs%>%
    purrr::map(~{
      pdftools::pdf_ocr_text(pdf = .x, pages = NULL, dpi  = 1000,language = "fra")
    })
  saveRDS(ocr_pdf_results,"./ocr_pdf_results.rds")
}

clean_single_pdf <- function(target_pdf = NULL,ocr_pdf_results = NULL){
  # target_pdf <- names(ocr_pdf_results)[1]
  cleaned_md <- ocr_pdf_results[[target_pdf]]
  # [1]%>%
  #   heddlr::heddle(.,"’", "")%>%
  #   heddlr::heddle(.,">","*")

  add_md_page_header <- function(page_indx, md_doc){
    paste("\n# Page",page_indx,"\n",md_doc[[page_indx]])
  }

  cleaned_md <- sapply(1:length(cleaned_md), function(x)add_md_page_header(x,md_doc = cleaned_md))

  pdf_header <- heddlr::create_yaml_header(list(title = target_pdf))

  ocr_pdf_results_md <- heddlr::make_template(
    pdf_header,
    head(cleaned_md,6)
  )
  # save results
  cleand_docs_dir <- "./cleand_docs/"
  cleand_md_file <- paste0(cleand_docs_dir,target_pdf,".md")
  if(!dir.exists(cleand_docs_dir))dir.create(cleand_docs_dir,recursive = T)
  readr::write_lines(ocr_pdf_results_md, file = cleand_md_file)

  cleaned_md <- c(pdf_header,cleaned_md)
  names(cleaned_md) <- c("header",paste("Page",1:(length(cleaned_md)-1)))

  return(cleaned_md)
}

clean_all_pdfs <- function(ocr_pdf_results){
  names(ocr_pdf_results) <- gsub(".pdf","",names(ocr_pdf_results))
  ocr_pdf_results_cleaned <- names(ocr_pdf_results)%>%sapply(function(x)clean_single_pdf(target_pdf=x,ocr_pdf_results))
  return(ocr_pdf_results_cleaned)
}


create_source_pages <- function(pdf_file = system.file("rawdata/manuel_plombier.pdf", package = "th2coldorg"), source_dir = "./sources"){
  source_dir <- normalizePath(source_dir)
  if(!dir.exists(source_dir))dir.create(source_dir, recursive = TRUE)
  if(source_dir != getwd()) setwd(source_dir)
  pdftools::pdf_convert(pdf = pdf_file, format = "png", pages = NULL, dpi = 200)
}





#' mod_th2llm_ui
#' @export
mod_th2llm_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("llm_main_box"))
}

#' mod_th2llm_server
#' @export
mod_th2llm_server <- function(id, auth_details = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_admin <- auth_details$is_admin

    Sys.setenv(CURRENT_USER = auth_details$username)

    if (Sys.info()["sysname"] != "Windows") {
      llm_configs_file <- "/home/automl/llm_configs.rds"
      chat_repo <- "/home/chat"
      source_dir <- "/home/chat/sources"
    } else {
      chat_repo <- "./chat"
      source_dir <- "./sources"
      llm_configs_file <- "./llm_configs.rds"
    }
    if (!dir.exists(chat_repo)) dir.create(chat_repo, recursive = TRUE)
    if (!is.null(is_admin) && is_admin == TRUE) {

      output$html_files_select <- renderUI({
        req(is_admin)
        html_files <- list.files(chat_repo, pattern = "\\.html$", full.names = FALSE)
        users_html_files <- sapply(html_files, extract_name_from_filename)
        selectInput(inputId = ns("html_file"),
                    label = "Utilisateur",
                    choices = setNames(html_files, users_html_files))
      })

      output$download_html <- downloadHandler(
        filename = function() {
          paste0(input$html_file)
        },
        content = function(file) {
          selected_file <- file.path(chat_repo, input$html_file)
          file.copy(selected_file, file)
        }
      )

      output$download_html_btn <- renderUI({
        req(input$html_file)
        downloadButton(outputId = ns("download_html"), label = "Télécharger", style = th2utils::add_button_theme())
      })
    }
    llm_exchange_content_file <- paste0(chat_repo, "/llm_exchange_content_", th2utils::clean_object_name(auth_details$user), ".Rmd")

    if (!file.exists(llm_exchange_content_file)) {
      base_content <- readr::read_lines(file = system.file("llm/llm_exchange_content.Rmd", package = "th2coldorg"))
      readr::write_lines(base_content, file = llm_exchange_content_file)
    }

    llm_exchange_content <- reactiveFileReader(1000, session = session, filePath = llm_exchange_content_file, readFunc = readLines)

    llm_chat_objct <- reactiveVal()

    output$llm_main_box <- renderUI({
      uiOutput(ns("th2llm_chat"))
    })

    output$llm_engine <- renderUI({
      selectInput(inputId = ns("llm_engine"),
                  label = "Engine",
                  choices = c("MistralAI", "OpenAI", "Llama"))
    })

    output$llm_api_key <- renderUI({
      passwordInput(inputId = ns("llm_api_key"), label = "API Key", placeholder = "secret value")
    })

    output$llm_variant <- renderUI({
      selectInput(inputId = ns("llm_variant"),
                  label = "Variant",
                  choices = c("3.0", "4.0 turbo"))
    })

    output$save_configs <- renderUI({
      req(input$llm_api_key)
      actionButton(inputId = ns("save_configs"),
                   icon = icon("save"),
                   label = "Save Configs",
                   style = th2utils::add_button_theme())
    })

    output$llm_configs <- renderUI({
      fluidRow(
        column(width = 2, uiOutput(ns("llm_engine"))),
        column(width = 2, uiOutput(ns("llm_api_key"))),
        column(width = 2, uiOutput(ns("llm_variant"))),
        column(width = 2, br(), uiOutput(ns("save_configs")))
      )
    })

    observeEvent(input$save_configs, {
      llm_configs <- data.frame(llm_engine = input$llm_engine,
                                llm_api_key = th2product::encrypt_column(input$llm_api_key),
                                llm_variant = input$llm_variant)
      saveRDS(llm_configs, llm_configs_file)
      th2product::th_shinyalert(title = "LLM² configs", text = "stored successully", type = "success")
    })

    output$llm_question <- renderUI({
      req(llm_exchange_content())
      textAreaInput(inputId = ns("llm_question"),
                    placeholder = "C'est quoi la boucle de Tickelman ?",
                    width = "1200px",
                    label = "Question")
    })

    output$llm_chat_exchange <- renderUI({
      req(llm_exchange_content())
      llm_exchange_content_file2 <<- llm_exchange_content_file
      rendered_file <- rmarkdown::render(input = llm_exchange_content_file,
                                         output_dir = chat_repo,
                                         output_file = gsub(".Rmd", ".html", llm_exchange_content_file))
      includeHTML(rendered_file)
    })

    output$th2llm_chat <- renderUI({
      fluidPage(
          tags$script(HTML(sprintf("$(document).one('shiny:idle', function(){
      setTimeout(function(){
        window.scrollTo(0, document.body.scrollHeight);
      }, 5000);
    });
      $('#%s').on('keydown', function(e) {
        if(e.keyCode === 13 && !e.shiftKey) {
          e.preventDefault();

          Shiny.setInputValue('%s', $(this).val(), {priority: 'event'});
        }
      });
    ", ns("llm_question"), ns("submit")))
        ),
        waiter::use_waiter(),
        fluidRow(
          column(width = 2, uiOutput(ns("html_files_select"))),
          column(width = 2, br(), uiOutput(ns("download_html_btn"))),
          ),
        uiOutput(ns("llm_chat_exchange")),
        fluidRow(
          column(width = 3, uiOutput(ns("vide"))),
          column(width = 6, uiOutput(ns("llm_question"))),
          column(width = 1, br(), uiOutput(ns("ask_question")))
        ),
        fluidRow(
          column(width = 3, uiOutput(ns("vide"))),
          column(width = 3, uiOutput(ns("vide"))),
          column(width = 3, uiOutput(ns("vide"))),
        )
      )
    })

    output$ask_question <- renderUI({
      req(input$llm_question)
      actionButton(inputId = ns("ask_question"),
                   label = "",
                   icon = icon("paper-plane"),
                   style = th2utils::add_button_theme())
    })

    processLLMSubmission <- function(text, llm_exchange_file) {
      print(text)
      w <- waiter::Waiter$new(id = ns("th2llm_chat"), html = waiter::spin_circle(), color = "rgba(0, 0, 0, 0.5)")
      w$show()

      question_time <- paste0("* ", format(Sys.time(), "%Y-%B-%d %H:%M:%S"), "\n")
      question_title <- glue::glue("## <span style='color:blue'> *Question*</span>\n\n")
      question_content <- glue::glue("> {text}\n\n")
      response_title <- glue::glue("\n ## <span style='color:green'> *Réponse*</span>\n\n")

      if (TRUE) {
        llm_reponse <<- ask_coldorg_llm(
          main_url = "http://15.236.224.184:8000",
          user_mail = Sys.getenv("CURRENT_USER"),
          question = text
        )
        reponse_img_path <- find_page_source_path(page_id = llm_reponse$pages[[1]], source_dir = source_dir)
        reponse_details <- reponse_img_path %>% show_source_page_expansion(source_img_path = .)
        response_content <- glue::glue("\n > {llm_reponse$response}\n\n {reponse_details} \n \n")
      } else {
        response_content <- "\n > I don't know for the moment, please set your LLM engine"
        th2product::th_shinyalert(title = "LLM² engine is not",
                                  text = "please finalize your configuration",
                                  type = "error")
      }

      llm_exchange_content_new <- list(question_title, question_time, question_content, response_title, response_content)
      new_content <- c(llm_exchange_content(), llm_exchange_content_new)
      readr::write_lines(new_content, file = llm_exchange_content_file)

      w$hide()
    }
    observeEvent(input$ask_question, {
      print(input$llm_question)
      if (is.null(input$llm_question) || input$llm_question == "") {
        return()
      }
      processLLMSubmission(input$llm_question, llm_exchange_file = llm_exchange_content_file)
    })

    observeEvent(input$submit, {
      print(input$llm_question)
      if (is.null(input$llm_question) || input$llm_question == "") {
        return()
      }
      processLLMSubmission(input$llm_question, llm_exchange_file = llm_exchange_content_file)
    })

    observeEvent(input$download_llm_chat, {
      showModal(
        modalDialog(title = "Download LLM² Exchange", size = "xl",
                    downloadButton(outputId = ns("download_llm_chat_md"), style = th2utils::add_button_theme()))
      )
    })

    output$download_llm_chat_md <- downloadHandler(
      filename = function() {
        paste("thaink2_llm_exchange_", format(Sys.time(), "%Y_%B_%d_%H_%M"), ".html", sep = "")
      },
      content = function(file) {
        rendered_file <- gsub(".Rmd", ".html", llm_exchange_content_file)
        file.copy(rendered_file, file)
      }
    )

    observeEvent(input$save_llm_chat, {
      mod_save_llm_chat_id <- th2product::generateID("save_llm_chat")
      report_skeleton <- heddlr::import_draft(llm_exchange_content_file)
      mod_save_llm_report_server(mod_save_llm_chat_id, report_skeleton = report_skeleton)
      showModal(
        modalDialog(
          title = "Saving Report",
          size = "xl",
          mod_save_llm_report_ui(ns(mod_save_llm_chat_id))
        )
      )
    })
  })
}

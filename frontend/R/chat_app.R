# UI
cold_org_app_with_auth_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    th2shinymanager::auth_ui(
      id = ns("auth"),
      tags_top = tags$div(
        tags$h3("Cold Org", style = "text-align:center;"),
        tags$img(
          src = "https://media.licdn.com/dms/image/v2/D4E0BAQGqPdCzXUVtnA/company-logo_200_200/company-logo_200_200/0/1722848400017/cold_org_logo?e=1746057600&v=beta&t=-9uZsgx8-BbT7AcihNAT-lV5J3VGB72MvQFJwNousVI", width = 100
        )
      ),
      tags_bottom = tags$div(
        tags$p(
          "For any question, please contact ",
          tags$a(
            href = "mailto:someone@example.com?Subject=Shiny%20Manager",
            target = "_top", "administrator"
          )
        ),
        tags$p(
          "Vous n'avez pas encore un compte? ",
          uiOutput(ns("sign_up")),
          uiOutput(ns("sign_in_mod"))
        ),
      ),
      background = "linear-gradient(white,
                   rgba(0, 0, 255, 0.5)),
                   url('');",
      lan = th2shinymanager::use_language("fr")
    ),
    # Classic app UI
    mod_th2llm_ui(ns("th2llm"))
  )
}



# Server

cold_org_app_with_auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if(Sys.getenv("CURRENT_DB") == "postgresql"){
      user_infos <- th2product::fetch_data_from_db(table = "th2coldorg") %>%
        dplyr::distinct(username, .keep_all = T) %>%
        dplyr::rename(user = username)
        # dplyr::mutate(is_hashed_password = F) %>%
        # dplyr::mutate(password = th2product::decrypt_column(column = password))

    }else{
      user_infos <- data.frame(
        user = "farid",
        password = "farid",
        start = Sys.Date(),
        expire = Sys.Date() + 365,
        admin = TRUE,
        comment = "new user",
        is_hashed_password = FALSE
        # stringsAsFactors = FALSE
      )
    }


    # Authentication module
    auth <- callModule(
      module = th2shinymanager::auth_server,
      id = "auth",
      check_credentials = th2shinymanager::check_credentials(user_infos)
    )

    res_auth <- reactive({
      reactiveValuesToList(auth)
    })



    output$sign_in_mod <- renderUI({
      req(input$sign_up)
      # Sign-up module
      th2shinymanager::signup_server(
        id = "signup",
        parent_session = session,
        lan = th2shinymanager::use_language("fr")
      )
      th2shinymanager::signup_ui(id = ns("signup"))
    })
    # chat module
    observeEvent(res_auth()$user, {
      auth_details <<- res_auth()
      mod_th2llm_server("th2llm", auth_details = auth_details)
    })

    output$sign_up <- renderUI({
      actionButton(inputId = ns("sign_up"), label = "S'inscrire")
    })

  })
}

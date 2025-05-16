#' coldorg_ui_bs
#' @export
#' @import shinymanager
#' @importFrom shiny shinyApp
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem

cold_org_app_adminui <- function(id){
  shiny::fluidPage(
    th2coldorg::mod_th2llm_ui("th2llm")
  )
}

# Wrap your UI with secure_app
# cold_org_app_adminui <- th2shinymanager::secure_app(cold_org_app_adminui)

# Exemple d'UI commentée avec options supplémentaires
cold_org_app_adminui <- th2shinymanager::secure_app(cold_org_app_adminui, enable_admin = TRUE,
                                                 language = "fr",
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
                                                     uiOutput("sign_up"),
                                                     uiOutput("sign_in_mod")
                                                   )
                                                 ),
                                                 background = "linear-gradient(white,
                   rgba(0, 0, 255, 0.5)),
                   url('');")

#' cold_org_app_adminserver
#' @export
cold_org_app_adminserver <- function(input, output, session) {

  ns <- session$ns
 initialize_envs(working_mod = "dev")

  template_file <- system.file("sql_config/pg_template.yml", package = "th2coldorg")

  auth <- th2shinymanager::secure_server(
    check_credentials = th2shinymanager::check_credentials(db = template_file)
  )

  res_auth <- reactive({
    reactiveValuesToList(auth)
  })

  observeEvent(res_auth()$user, {
    auth_details <<- res_auth()
    th2coldorg::mod_th2llm_server("th2llm", auth_details = auth_details)
  })

  output$sign_up <- renderUI({
    actionButton(inputId = ns("sign_up"), label = "S'inscrire")
  })

  output$sign_in_mod <- renderUI({
    req(input$sign_up)


    # Appel du module sign-up
    th2shinymanager::signup_server(
      id = "signup",
      parent_session = session,
      lan = th2shinymanager::use_language("fr")
    )
    th2shinymanager::signup_ui(id = ns("signup"))
  })
}

#' cold_org_app_adminlauncher
#' @import echarts4r
#' @export
cold_org_app_adminlauncher <- function(target_port = 3838, target_host = '0.0.0.0'){
  shiny::shinyApp(
    ui = cold_org_app_adminui,
    server = cold_org_app_adminserver,
    options = list(
      launch.browser = TRUE,
      port = target_port,
      host = target_host,
      java.parameters = "-Xss3m"
    )
  )
}

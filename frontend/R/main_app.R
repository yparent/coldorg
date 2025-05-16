#' coldorg_ui_bs
#' @export
#' @importFrom shiny shinyApp
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem

coldorg_app_ui <- function(id){
  waiter::autoWaiter()
  bs4Dash::dashboardPage(
    scrollToTop = TRUE,
    header = th2coldorg::prepare_bi_app_header(app_title = "Cold Org"),
    dark = FALSE,
    # freshTheme = readRDS(system.file("app/www/fairml_theme.rds", package = "dive2ml")),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        id = "sidebarMenu",
        bs4Dash::menuItem(
          text = "Chat", icon = icon("lightbulb"),
          tabName = "coldorg_chat"
        )
      ),minified = TRUE, collapsed = TRUE),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = "coldorg_chat",
                         cold_org_app_with_auth_ui("chat_app")
        )
      )
    )
  )
}

#' coldorg_app_server
#' @export
coldorg_app_server <- function(input, output, session){
  initialize_envs(working_mod = "dev")
  cold_org_app_with_auth_server("chat_app")
}

#' coldorg_app_launcher
#' @import echarts4r
#' @export
coldorg_app_launcher <- function(target_port = 3838, target_host = '0.0.0.0'){
  shiny::shinyApp(ui = coldorg_app_ui, server = coldorg_app_server,
                  options = list(launch.browser = TRUE,
                                 port = target_port,
                                 host = target_host,
                                 java.parameters = "-Xss3m")
  )
}

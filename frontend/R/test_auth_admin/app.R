
library(shiny)
library(shinymanager)


# data.frame with credentials info
credentials <- data.frame(
    user = c("benoit.thieurmel@datastorm.fr", "victor", "benoit"),
    password = c("azerty", "12345", "azerty"),
    comment = c("alsace", "auvergne", "bretagne"),
    applications = c("app1;shiny-sqlite", "app1", "shiny-sqlite"),
    age = c(14, 20, 30),
    expire = as.Date(c(NA, "2019-12-31", "2019-12-31")),
    admin = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
)

# if (!file.exists("credentials.sqlite")) {
create_db(credentials_data = credentials, sqlite_path = "credentials.sqlite", passphrase = "supersecret")
# }
ui <- fluidPage(

    # classic app
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', names(iris)),
        selectInput('ycol', 'Y Variable', names(iris),
                    selected=names(iris)[[2]]),
        numericInput('clusters', 'Cluster count', 3,
                     min = 1, max = 9)
    ),
    mainPanel(
        plotOutput('plot1'),
        verbatimTextOutput("res_auth")
    )

)

secure_app(ui, enable_admin = TRUE, theme = shinythemes::shinytheme("sandstone"))


server <- function(input, output, session) {
    auth_out <- secure_server(
        check_credentials = check_credentials("credentials.sqlite", passphrase = "supersecret"),
        timeout = 1
    )

    observe({
        print(input$shinymanager_where)
    })

    output$res_auth <- renderPrint({
        reactiveValuesToList(auth_out)
    })

    # classic app
    selectedData <- reactive({
        iris[, c(input$xcol, input$ycol)]
    })

    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })

    output$plot1 <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })


}

shinyApp(ui, server)



#' @importFrom DT DTOutput
#' @importFrom htmltools tags singleton tagList
#' @importFrom shiny NS fluidRow column actionButton icon
#' @export
admin_ui <- function(id, lan = NULL) {

  ns <- NS(id)

  if(is.null(lan)){
    lan <- use_language()
  }

  tagList(
    singleton(tags$head(
      tags$link(href="shinymanager/styles-admin.css", rel="stylesheet"),
      tags$script(src = "shinymanager/shiny-utils.js"),
      tags$script(src = "shinymanager/timeout.js")
    )),
    fluidRow(
      column(
        width = 10, offset = 1,
        # tags$h2(lan$get("Administrator mode")),
        # tags$br(), tags$br(),

        tags$h3(icon("users"), lan$get("Users"), class = "text-primary"),
        tags$hr(),

        actionButton(
          inputId = ns("add_user"),
          label = lan$get("Add a user"),
          icon = icon("plus"),
          width = "100%",
          class = "btn-primary"
        ),
        tags$br(), tags$br(), tags$br(),
        DTOutput(outputId = ns("table_users")),

        tags$br(),

        actionButton(
          inputId = ns("select_all_users"),
          # label = lan$get("Select all shown users"),
          label = "",
          class = "btn-secondary pull-right",
          style = "margin-left: 5px",
          icon = icon("square-check")
        ),

        actionButton(
          inputId = ns("edit_selected_users"),
          label = lan$get("Edit selected users"),
          class = "btn-primary pull-right disabled",
          style = "margin-left: 5px",
          icon = icon("pen-to-square")
        ),

        actionButton(
          inputId = ns("remove_selected_users"),
          label = lan$get("Remove selected users"),
          class = "btn-danger pull-right disabled",
          icon = icon("trash-can")
        ),

        tags$br(),

        if("users" %in% get_download()){
          list(
            tags$br(),tags$br(), tags$br(),

            downloadButton(
              outputId = ns("download_users_database"),
              label = lan$get("Download Users file"),
              class = "btn-primary center-block",
              icon = icon("download")
            )
          )
        },

        tags$h3(icon("key"), lan$get("Passwords"), class = "text-primary"),
        tags$hr(),

        DTOutput(outputId = ns("table_pwds")),

        tags$br(),

        actionButton(
          inputId = ns("change_selected_allusers"),
          # label = lan$get("Select all shown users"),
          label = "",
          class = "btn-secondary pull-right",
          style = "margin-left: 5px",
          icon = icon("square-check")
        ),

        actionButton(
          inputId = ns("change_selected_pwds"),
          label = lan$get("Force selected users to change password"),
          class = "btn-primary pull-right disabled",
          icon = icon("key")
        ),

        if("db" %in% get_download()){
          conditionalPanel(
            condition = paste0("output['",ns("is_sqlite"),"']"),
            list(
              tags$br(),tags$br(), tags$br(), tags$hr(),

              downloadButton(
                outputId = ns("download_sql_database"),
                label = lan$get("Download SQL database"),
                class = "btn-primary center-block",
                icon = icon("download")
              )
            )
          )
        },

        tags$br(),tags$br()

      )
    )
  )
}

#' @importFrom DT renderDT datatable JS
#' @importFrom shiny reactive observeEvent isolate showModal modalDialog reactiveFileReader
#'  removeUI insertUI reactiveValues showNotification callModule req updateCheckboxInput reactiveTimer
#' @importFrom DBI dbConnect SQL
#' @importFrom RSQLite SQLite
#' @export
#' @export
admin_server <- function(input, output, session, lan, inputs_list = NULL, max_users = NULL) {

  ns <- session$ns
  jns <- function(x) { paste0("#", ns(x)) }
  token_start <- isolate(getToken(session = session))
  update_read_db <- reactiveValues(x = NULL)

  ## Lecture de la table "credentials" depuis PostgreSQL
  users <- reactiveVal(NULL)
  observe({
    req(update_read_db$x)
    db_data <- try({
      conn <- th2product::connect_to_database()
      on.exit(DBI::dbDisconnect(conn))
      th2product::fetch_data_from_db(table = "credentials")
    }, silent = TRUE)
    if (inherits(db_data, "try-error")) {
      showModal(modalDialog("Une erreur est survenue lors de la lecture de la table 'credentials' dans PostgreSQL."))
      users(NULL)
    } else {
      users(db_data)
    }
  })

  auto_sql_reader <- reactiveTimer(1000) # rafraîchissement toutes les 1 seconde
  observeEvent(auto_sql_reader(), {
    if ("add_user" %in% names(session$input)) {
      conn <- th2product::connect_to_database()
      tmp <- th2product::fetch_data_from_db(table = "credentials")
      DBI::dbDisconnect(conn)
      if (!is.null(tmp)) users(tmp)
    }
  })

  ## Lecture de la table "pwd_mngt" depuis PostgreSQL
  pwds <- reactiveVal(NULL)
  observe({
    req(update_read_db$x)
    db_data <- try({
      conn <- th2product::connect_to_database()
      on.exit(DBI::dbDisconnect(conn))
      th2product::fetch_data_from_db(table = "pwd_mngt")
    }, silent = TRUE)
    if (inherits(db_data, "try-error")) {
      showModal(modalDialog("Une erreur est survenue lors de la lecture de la table 'pwd_mngt' dans PostgreSQL."))
      pwds(NULL)
    } else {
      pwds(db_data)
    }
  })

  observeEvent(auto_sql_reader(), {
    if ("add_user" %in% names(session$input)) {
      conn <- th2product::connect_to_database()
      tmp <- th2product::fetch_data_from_db(table = "pwd_mngt")
      DBI::dbDisconnect(conn)
      if (!is.null(tmp)) pwds(tmp)
    }
  })

  ## Affichage de la table "credentials" via DT
  output$table_users <- renderDT({
    req(users())
    unbindDT(ns("table_users"))
    my_users <- users()
    # Supprime les colonnes sensibles
    my_users <- my_users[, setdiff(names(my_users), c("password", "is_hashed_password")), drop = FALSE]
    my_users$Edit <- input_btns(ns("edit_user"), my_users$user, "Edit user", icon("pen-to-square"), status = "primary", lan = lan())
    my_users$Remove <- input_btns(ns("remove_user"), my_users$user, "Delete user", icon("trash-can"), status = "danger", lan = lan())
    my_users$Select <- input_checkbox_ui(ns("select_mult_users"), my_users$user, session = session)
    names_lan <- sapply(names(my_users), function(x) lan()$get(x))
    change <- as.logical(my_users$admin)
    my_users[change, "admin"] <- lan()$get("Yes")
    my_users[!change, "admin"] <- lan()$get("No")
    datatable(
      data = my_users,
      colnames = make_title(names_lan),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      style = "bootstrap",
      options = list(
        scrollY = if (nrow(my_users) > 10) "500px",
        lengthChange = FALSE,
        paging = FALSE,
        language = lan()$get_DT(),
        drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
        scrollX = TRUE,
        columnDefs = list(
          list(width = "50px", targets = (ncol(my_users)-3):(ncol(my_users)-1))
        )
      )
    )
  }, server = FALSE)

  ## Affichage de la table "pwd_mngt" via DT
  output$table_pwds <- renderDT({
    req(pwds())
    unbindDT(ns("table_pwds"))
    my_pwds <- pwds()
    if("n_wrong_pwd" %in% colnames(my_pwds)) {
      my_pwds$n_wrong_pwd <- NULL
    }
    my_pwds$`Change password` <- input_btns(ns("change_pwd"), my_pwds$user, "Ask to change password", icon("key"), status = "primary", lan = lan())
    my_pwds$`Reset password` <- input_btns(ns("reset_pwd"), my_pwds$user, "Reset password", icon("arrow-rotate-left"), status = "warning", lan = lan())
    my_pwds$Select <- input_checkbox_ui(ns("change_mult_pwds"), my_pwds$user, session = session)
    names_lan <- sapply(names(my_pwds), function(x) lan()$get(x))
    change <- as.logical(my_pwds$must_change)
    my_pwds[change, "must_change"] <- lan()$get("Yes")
    my_pwds[!change, "must_change"] <- lan()$get("No")
    change <- as.logical(my_pwds$have_changed)
    my_pwds[change, "have_changed"] <- lan()$get("Yes")
    my_pwds[!change, "have_changed"] <- lan()$get("No")
    datatable(
      data = my_pwds,
      colnames = make_title(names_lan),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      style = "bootstrap",
      options = list(
        scrollY = if (nrow(my_pwds) > 10) "500px",
        lengthChange = FALSE,
        paging = FALSE,
        language = lan()$get_DT(),
        drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
        scrollX = TRUE,
        columnDefs = list(
          list(width = "50px", targets = (ncol(my_pwds)-3):(ncol(my_pwds)-1))
        )
      )
    )
  }, server = FALSE)

  ## Exemple d'opération de mise à jour d'un utilisateur
  observeEvent(input$edited_user, {
    my_users <- users()
    my_pwds <- pwds()
    newval <- value_edited$user
    showModal(modalDialog(
      title = NULL, footer = NULL, size = "s", easyClose = FALSE,
      div(img(src = "shinymanager/1497.gif", style = "height:50px"), align = "center")
    ))
    res_edit <- try({
      conn <- th2product::connect_to_database()
      on.exit(DBI::dbDisconnect(conn))
      # update_user_pg est une fonction que vous devez définir pour mettre à jour la table "credentials"
      th2product::update_data_in_db(table = "credentials", updates = newval, filter = list(user = input$edited_user))
      update_user_pg(newval, input$edit_user)
    }, silent = FALSE)
    removeModal()
    if (inherits(res_edit, "try-error")) {
      showNotification(ui = lan()$get("Fail to update user"), type = "error")
    } else {
      showNotification(ui = lan()$get("User successfully updated"), type = "message")
      update_read_db$x <- Sys.time()
    }
  })

  ## Téléchargement de la table "credentials"
  output$download_users_database <- downloadHandler(
    filename = function() {
      paste('shinymanager-users-', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      my_users <- th2product::fetch_data_from_db(table = "credentials")
      my_users$password <- NULL
      my_users$is_hashed_password <- NULL
      write.table(my_users, con, sep = ";", row.names = FALSE, na = '')
    }
  )

  output$is_sqlite <- reactive({ FALSE })
  outputOptions(output, "is_sqlite", suspendWhenHidden = FALSE)
}



#' @importFrom htmltools HTML tags tagList
#' @importFrom shiny showModal modalDialog modalButton actionButton
remove_modal <- function(inputId, user, lan) {
  showModal(modalDialog(
    tags$p(HTML(sprintf(
      lan$get("Are you sure to remove user(s): %s from the database ?"), tags$b(paste(user, collapse = ", "))
    ))),
    fade = FALSE,
    footer = tagList(
      actionButton(
        inputId = inputId,
        label = lan$get("Delete user(s)"),
        class = "btn-danger",
        `data-dismiss` = "modal"
      ),
      modalButton(lan$get("Cancel"))
    )
  ))
}

change_pwd_modal <- function(inputId, user, lan) {
  showModal(modalDialog(
    title = lan$get("Ask to change password"),
    tags$p(HTML(
      sprintf(lan$get("Ask %s to change password on next connection?"), tags$b(paste(user, collapse = ", ")))
    )),
    footer = tagList(
      modalButton(lan$get("Cancel")),
      actionButton(
        inputId = inputId,
        label = lan$get("Confirm"),
        class = "btn-primary",
        `data-dismiss` = "modal"
      )
    )
  ))
}

reset_pwd_modal <- function(inputId, user, lan) {
  showModal(modalDialog(
    title = lan$get("Reset password"),
    tags$p(HTML(
      sprintf(lan$get("Reset password for %s?"), tags$b(paste(user, collapse = ", ")))
    )),
    footer = tagList(
      modalButton(lan$get("Cancel")),
      actionButton(
        inputId = inputId,
        label = lan$get("Confirm"),
        class = "btn-primary",
        `data-dismiss` = "modal"
      )
    )
  ))
}

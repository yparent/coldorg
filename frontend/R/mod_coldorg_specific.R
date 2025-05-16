prepare_calendr_data <- function(user_services = NULL){
  # user_services <- user_meta2$user_services
  coldorg_pal <- get_coldorg_col_pallette()
  user_services <- user_services%>%
    dplyr::filter(!animation_service %in% c("no service","Closed"))%>%
    dplyr::mutate(calendarId = 2)%>%
    dplyr::rename(title = company,  start = next_appoint)%>%
    dplyr::mutate(location = country, end = start, body = animation_service,
                  backgroundColor = dplyr::case_when(animation_service == 'Meeting planned' ~ coldorg_pal[1],
                                                     animation_service =='Meeting expired' ~ coldorg_pal[4],
                                                     animation_service =='Stand By' ~ coldorg_pal[3],
                                                     animation_service =='Plan first meeting' ~ coldorg_pal[2],
                                                     animation_service =='Plan meeting' ~ coldorg_pal[2],.default = NA))%>%
    dplyr::select(calendarId, title, body,backgroundColor, start, end, location)%>%
    dplyr::filter(!is.na(start))
  return(user_services)
}

wid_service_calndr_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("calendar_title")),
    toastui::calendarOutput(ns("my_calendar"))
  )
}

wid_service_calndr_server <-function(id, user_meta = list()) {
  moduleServer(id, function(input, output, session) {
  ns <- session$ns

  output$calendar_title <- renderUI({
    user <- user_meta$user_services%>%
      dplyr::pull(user)%>%unique()
    tags$h2(paste(user,"Calendrier"))
  })
  output$my_calendar <- toastui::renderCalendar({
    # user_meta2 <<- user_meta
    user_meta$user_services%>%
      prepare_calendr_data()%>%
    toastui::calendar(.,navigation = TRUE) %>%
      toastui::cal_props(
        list(
          id = 1,
          name = "PERSO",
          color = "white",
          bgColor = "firebrick",
          borderColor = "firebrick"
        ),
        list(
          id = 2,
          name = "WORK",
          color = "white",
          bgColor = "forestgreen",
          borderColor = "forestgreen"
        )
      )
  })

  output$dates <- renderPrint({
    input$my_calendar_dates
  })

  output$click <- renderPrint({
    input$my_calendar_click
  })

  })
}


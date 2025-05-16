#' Prepare the Application Header
#' Creates a custom header for a Shiny application, including the application logo and links.
#' @export
prepare_bi_app_header <- function(app_title = "BI & Reporting", app_logo = NULL, app_logo_circle = NULL,logo_href = NULL) {
  if(is.null(app_logo_circle))app_logo_circle <- "https://raw.githubusercontent.com/thaink2/thaink2publicimages/main/thaink2_logo_circle.png"
  if(is.null(app_logo))app_logo <- "https://static.wixstatic.com/media/9aacb8_9886b3a143c2470d96ec76a181e67e49~mv2.png/v1/fill/w_195,h_46,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/thaink2-logo-blue-big.png"
  if(is.null(logo_href))logo_href <- "https://www.thaink2.com/en"
  bs4Dash::bs4DashNavbar(
    skin = "light",
    title = bs4Dash::bs4DashBrand(title = app_title, image = app_logo_circle),
    rightUi = shiny::tagList(
      shiny::tags$li(
        class = "nav-item dropdown",
        shiny::tags$a(
          href = logo_href,
          target = "_blank",
          class = "nav-link",
          shiny::tags$img(src = app_logo, style = "height: 30px")
        )
      )
    )
  )
}


chart_bar_stack <- function(input_data, target_var){

  bar_chart <- input_data |>
    e_charts_(target_var)
  for(ds in sort(colnames(input_data)[-1])){
    bar_chart <- bar_chart%>%
      e_bar_(ds, stack = "grp")
  }
  bar_chart <- bar_chart%>%
    e_color(color = get_coldorg_col_pallette())%>%
    e_tooltip()
  return(bar_chart)
}

get_coldorg_col_pallette <- function(target_pal = 1){
 if(target_pal ==1) return(c("lightgreen","#013DFF","yellow","orange","maroon", "red"))
 # if(target_pal ==2) c("#543005", "#8C510A" ,"#BF812D" ,"#DFC27D" ,"#F6E8C3", "#C7EAE5" ,"#80CDC1" ,"#35978F", "#01665E" ,"#003C30")
}


get_user_services_overview_dt <- function(user_services_data){
  coldorg_pal <- get_coldorg_col_pallette()
  user_services_overview_dt <- user_services_data%>%
    DT::datatable(selection  = "single")

  if('Meeting expired' %in% colnames(user_services_data)){
    user_services_overview_dt <- user_services_overview_dt%>%
      DT::formatStyle(
        'Meeting expired',
        backgroundColor = coldorg_pal[4])
  }
  if('Meeting planned' %in% colnames(user_services_data)){
    user_services_overview_dt <- user_services_overview_dt%>%
      DT::formatStyle(
        'Meeting planned',
        backgroundColor = coldorg_pal[1])
  }
  if('Stand By' %in% colnames(user_services_data)){
    user_services_overview_dt <- user_services_overview_dt%>%
      DT::formatStyle(
      'Stand By',
      backgroundColor = coldorg_pal[3])
  }
  if('Plan first meeting' %in% colnames(user_services_data)){
    user_services_overview_dt <- user_services_overview_dt%>%
      DT::formatStyle(
        'Plan first meeting',
        backgroundColor = coldorg_pal[2])
  }
  if('Plan meeting' %in% colnames(user_services_data)){
    user_services_overview_dt <- user_services_overview_dt%>%
      DT::formatStyle(
        'Plan meeting',
        backgroundColor = coldorg_pal[2])
  }
  return(user_services_overview_dt)
}


show_source_page_expansion <- function(source_img_path = "C:/TEMP/th2customers/th2coldorg/V7-2023 (1)_5.png"){
  reponse_details <- "<details style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;'>
    <summary style='font-weight: bold; cursor: pointer; color: #007bff;'> Source </summary>
      <p style='margin-top: 10px;'>Cette a vraissembalement été utilisée pour génerer la réponse.</p>
    <img src='{source_img_path}' alt='Description of Image' width='1000' />
</details>"
  reponse_details <- glue::glue(reponse_details)
}


find_page_source_path <- function(page_id, source_stem = "manuel_plombier", source_dir ){
  # page_source_path <- glue::glue("\"{source_dir}/{source_stem}_{page_id}.png\"")
  paste0(source_dir,"/",source_stem,"_", page_id,".png")%>%
    normalizePath()
}

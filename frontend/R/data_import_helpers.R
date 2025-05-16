

import_coldorg_data <- function(data_source = "xlsx",
                             working_mode = "dev",
                             file_path = "./input_data.xlsx",
                             sheet_name = "Exploitation"){
  if(Sys.getenv("WORK_MODE") != "")working_mode <- Sys.getenv("WORK_MODE")
  if(TRUE){
    coldorg_data <- th2coldorg::template_data
  }else if(file.exists(file_path)){
    coldorg_data <- file_path%>%
      readxl::read_excel(path = ., sheet = sheet_name, skip = 4)%>%
      janitor::clean_names()
  }else{
    s3_bucket <- th2utils::get_company_name()
    if(Sys.getenv("WORK_MODE") == "")s3_bucket <- "thaink2"
    s3_prefix <- "uffiren/"
    uffiren_board <- th2ml::get_s3_ml_board(s3_bucket = s3_bucket, s3_prefix = s3_prefix)
    ms_connection <- uffiren_board%>%pins::pin_read(name = "ms_connection")
    coldorg_data <- th2blender::th2_connect_to_onedrive(ms_token = ms_connection$token,
                                                     item_id = "0175T5FPRCILKO5JZ325B2MXN54IBG7TJG")%>%
      readxl::read_excel(path = ., sheet = sheet_name, skip = 4)%>%
        janitor::clean_names()
  }
  # if(length(na.omit(coldorg_data$company))==0)coldorg_data$company <- paste0("company_",1:nrow(coldorg_data))

  if(sheet_name == "Exploitation"){
    coldorg_data <- coldorg_data%>%
      dplyr::mutate(next_appoint = dplyr::case_when(next_appoint %in% c("x","X") ~ NA, .default = next_appoint))%>%
      dplyr::mutate(next_appoint = lubridate::as_date(as.numeric(next_appoint), origin = "1900-01-01") - 2)
  }

  return(coldorg_data)
}


calculate_coldorg_stats_suivi <- function(coldorg_raw_data ,user , country, field, ref_date_adh = Sys.Date()){
  library(lubridate)
  coldorg_stats_suivi <- coldorg_raw_data%>%
    dplyr::filter(user %in% !!user  & field %in% !!field & country %in% !!country)
  if(nrow(coldorg_stats_suivi)== 0)return(NULL)
  coldorg_stats_suivi <- coldorg_stats_suivi%>%
    dplyr::mutate(prochaine_date_renouvellemnt = start_date %m+% years(1) )%>%
    dplyr::mutate(adhere_depuis_jours = difftime(prochaine_date_renouvellemnt , !!ref_date_adh , units  = "days"),
                  adhere_depuis_sem = difftime(prochaine_date_renouvellemnt , !!ref_date_adh, units  = "weeks"))%>%
    dplyr::mutate(default_statement = dplyr::case_when(adhere_depuis_jours > 90 ~ "1_normal",
                                                       adhere_depuis_jours < 90 & adhere_depuis_jours > 0 ~ "2_reminder",
                                                       adhere_depuis_sem > -6 ~ "3_warning",
                                                       adhere_depuis_sem > -12 ~ "4_strong_warning",
                                                       adhere_depuis_sem < -12 ~ "5_red", .default = "1_normal"))%>%
    dplyr::select(adhere_depuis_sem, user, company, country,
                  start_date, prochaine_date_renouvellemnt,
                  field, price,
                  default_statement, adhere_depuis_jours,next_appoint,
                  adhere_depuis_sem, expired, animation_service)
  default_stats <- coldorg_stats_suivi |>
    dplyr::group_by(default_statement)|>
    dplyr::summarise(stats = dplyr::n(), ca = sum(price))%>%
    dplyr::arrange(default_statement)

  expired_stats <- coldorg_stats_suivi |>
    dplyr::group_by(.data[["user"]],.data[["expired"]]) |>
    dplyr::summarise(nbr_service = dplyr::n(), .groups = "drop")%>%
    tidyr::pivot_wider(names_from = expired,
                       values_from = nbr_service)

  services_per_user_stats <- coldorg_stats_suivi |>
    dplyr::group_by(.data[["user"]],.data[["animation_service"]]) |>
    dplyr::summarise(nbr_service = dplyr::n(), .groups = "drop")%>%
    dplyr::filter(!animation_service %in% c("no service","Closed"))%>%
    tidyr::pivot_wider(names_from = animation_service,
                       values_from = nbr_service)

  default_by_user_montant <- coldorg_stats_suivi |>
    dplyr::group_by(.data[["user"]],.data[["default_statement"]]) |>
    dplyr::summarise(price = sum(price), .groups = "drop")%>%
    tidyr::pivot_wider(names_from = default_statement,
                       values_from = price)


  #-------- key values ----------------
  # coldorg_stats_suivi2 <<- coldorg_stats_suivi
  coldorg_key_values <- coldorg_stats_suivi |>
    dplyr::group_by(expired)%>%
    dplyr::summarise(ca_total = sum(price), nbre_adh = dplyr::n())

  #-------- return output--------------
  output_list <- list(coldorg_stats_suivi = coldorg_stats_suivi,
                      default_by_user_montant = default_by_user_montant,
                      expired_stats = expired_stats,
                      coldorg_key_values = coldorg_key_values,
                      services_per_user_stats = services_per_user_stats,
                      default_stats = default_stats)
  return(output_list)
}

#' ask_coldorg_llm
#' @author Lotfi Mayouf <lotif.mayouf@thaink2.com>
#' @description
#' module d'interaction avec LLM en utilisant API
#' @param main_url l'url principale ou le serveur est exposé
#' @export
ask_coldorg_llm <- function(main_url = "http://0.0.0.0:8000",target_endpoint = "chat", user_mail, question){
  chat_body <- list(email = user_mail,
                    query = question)
  chat_req <- httr2::request(main_url)%>%
    httr2::req_url_path(target_endpoint)%>%
    httr2::req_headers("Accept" = "application/json")%>%
    httr2::req_body_json(chat_body)
  chat_resp <- chat_req%>%
    httr2::req_perform()
  chat_resp <- chat_resp%>%
    httr2::resp_body_json()
  return(chat_resp)
}

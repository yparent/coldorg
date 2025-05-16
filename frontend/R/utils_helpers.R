#' @export
initialize_envs <- function(cluster = "scaleway", working_mode = "dev"){
  if (file.exists("C:/temp/Repos/temp_vars/initializer.R")){
    source("C:/temp/Repos/temp_vars/initializer.R")
    init_envs_cluster(cluster, working_mode)
  }
}

# Function to add new users
#' @export
add_user <- function(new_user, new_password) {
  current_data <- th2product::fetch_data_from_db(table = "th2coldorg")

  if (new_user %in% current_data$username) {
    return(list(result = FALSE, message = "Username already exists"))
  }
  new_data <- rbind(
    current_data,
    data.frame(
      username = new_user,
      password = new_password,
      start = Sys.Date(),
      expire = Sys.Date() + 365,
      admin = TRUE,
      comment = "new user",
      stringsAsFactors = FALSE
    )
  )
  th2product::add_entry_to_table(new_entry = new_data, target_table = "th2coldorg")
  return(list(result = TRUE, message = "Account created successfully"))
}

#' @export
extract_name_from_filename <- function(filename) {
  # Retirer le préfixe et l'extension
  name_part <- sub("^llm_exchange_content_", "", filename)
  name_part <- sub("\\..*$", "", name_part)

  # Découper la chaîne en segments
  parts <- strsplit(name_part, "_")[[1]]

  # Si on a au moins 3 segments, on considère que les deux derniers correspondent au domaine
  if (length(parts) >= 3) {
    local_parts <- parts[1:(length(parts)-2)]
  } else {
    local_parts <- parts
  }

  # Mettre chaque segment en Title case et les combiner avec un espace
  display_name <- paste(stringr::str_to_title(local_parts), collapse = " ")
  return(display_name)
}

#' @export
initialize_envs <- function(cluster = "scaleway", working_mod = "prod"){
  if (file.exists("C:/temp/Repos/temp_vars/initializer.R")){
    source("C:/temp/Repos/temp_vars/initializer.R")
    init_envs_cluster(cluster, WORKING_MODE = working_mod)
  }
}

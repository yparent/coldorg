template_data <- readxl::read_excel("./template_data.xlsx", sheet = 4, skip = 4)
template_data <- template_data|>
  janitor::clean_names()%>%
  dplyr::rename(user = owner,
                country = pays,
                price = montant_dadhesion,
                start_date = date_dadhesion,
                steps = etape,
                field = secteur,
                expired = overdue,
                next_appoint = prochain_rdv,
                )%>%
  dplyr::select(user,country, price, start_date, steps,expired,next_appoint,field)%>%
  dplyr::mutate(price = price - round(rnorm(nrow(template_data),15, 15),0))%>%
  dplyr::mutate(country = dplyr::case_when(country == "Belgique" ~ "Germany", .default = "France"))%>%
  dplyr::mutate(expired = dplyr::case_when(expired == "not expired" ~ "No",
                                           expired == "On hold" ~ "Stand By",
                                           .default = "Closed"))%>%
  dplyr::mutate(steps = dplyr::case_when(steps == "2) Phase d'intégration" ~ "Onboarding",
                                         steps == "On-hold" ~ "Stand By",
                                         steps == "4) 2ème année et +" ~ "Ongoing",
                                         steps %in% c("Fini","fini") ~ "Closed",
                                         steps == "3) RDV 9 mois" ~ "Soon",
                                           .default = "New User"))%>%
  dplyr::mutate(user = dplyr::case_when(user == "Reynald" ~ "Farid",
                                         user == "Jolle" ~ "David",
                                         user == "Gilles" ~ "Léa",
                                         user == "Gérald" ~ "Anis",
                                           .default = "Mike"))%>%
  dplyr::mutate(field = dplyr::case_when(field == "Construction" ~ "Manifacturing",
                                         field == "Commerce" ~ "Finance",
                                         field == "Service" ~"Consulting",
                                         .default = "Automotive"))%>%
  dplyr::mutate(next_appoint =janitor::excel_numeric_to_date(as.numeric(next_appoint)))%>%
  dplyr::mutate(company = paste0("company_",1:nrow(template_data)))


usethis::use_data(template_data, overwrite = T)

# readr::write_csv(template_data2, "./data/data_template.csv")
# usethis::use_data(template_data2, template_data2)



## ---IMPORTS

library(tidyr)
library(dplyr)
library(stringr)
library(stringi)



#----------------------------FUNCTIONS----------------------------------------------
#-----------------------------------------------------------------------------------

#************************IMPORTING-DATA************************************

import_data_viral_circulation <- function(report_data = NULL,
                                          header = FALSE,
                                          skip = 3,
                                          col_names = FALSE,
                                          sheet) {
  viral_circulation_data <- data.frame()
  for (data_path in report_data) {
    file_extension <- tools::file_ext(data_path)
    if (!is.null(file_extension)) {
      temp_data <- switch(
        file_extension,
        "xlsx" = readxl::read_excel(data_path,
                                    col_names = col_names,
                                    skip = skip,
                                    sheet = sheet),
        "csv" = utils::read.csv(data_path, header = header,
                                skip = if (header) 0 else 3)
      )
      if (!header) {
        temp_data <- row_to_header(data = temp_data)
        viral_circulation_data <- rbind(viral_circulation_data, temp_data)
      } else {
        viral_circulation_data <- temp_data
      }
    }
  }
  return(viral_circulation_data)
}
#' @title Convertir una fila a un encabezado
#' @keywords internal
row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) {
    names(data) <- as.character(unlist(data[row_num, ]))
    data[-row_num, ]
  }
}

#**************************CLEANING-DATA****************************

clean_data_other_viruses <- function(report_data) {
  names(report_data) <- epitrix::clean_labels(names(report_data), sep = "")
  report_data$resultadonuevocoronavirussarscov2vegeneral <-
    epitrix::clean_labels(
      report_data$resultadonuevocoronavirussarscov2vegeneral)
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(
    report_data$rangodeedadvegeneral)
  report_data$clasificacionvegeneral <- epitrix::clean_labels(
    report_data$clasificacionvegeneral)
  report_data$inmunofluorescenciarealizadavegeneral <- epitrix::clean_labels(
    report_data$inmunofluorescenciarealizadavegeneral)
  report_data$eventovegeneral <- epitrix::clean_labels(
    report_data$eventovegeneral)
  report_data$virusdetectadosvegeneral <- epitrix::clean_labels(
    report_data$virusdetectadosvegeneral)
  report_data$rangodeedadvegeneral <- epitrix::clean_labels(
    report_data$rangodeedadvegeneral)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_clean_epiweek <- config::get(file = config_path,
                                    "other_viruses")$epiweek$cols_clean
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  index_col <- which(cols_clean_epiweek %in% names(report_data))
  if (length(index_col) > 0) {
    col_clean <- cols_clean_epiweek[index_col]
    names(report_data)[names(report_data) == col_clean] <- col_epiweek
    report_data[[col_epiweek]] <- as.numeric(report_data[[col_epiweek]])
  }
  report_data$influenzaaporrtpcrvegeneral <- epitrix::clean_labels(
    report_data$influenzaaporrtpcrvegeneral)
  report_data$influenzabporrtpcrvegeneral <- epitrix::clean_labels(
    report_data$influenzabporrtpcrvegeneral)
  report_data$virussincitialrespiratoriovsrporrtpcrvegeneral <-
    epitrix::clean_labels(
      report_data$virussincitialrespiratoriovsrporrtpcrvegeneral)
  report_data$adenovirusadvporrtpcrvegeneral <-
    epitrix::clean_labels(
      report_data$adenovirusadvporrtpcrvegeneral)
  report_data <- clean_sars_data(report_data = report_data)
  return(report_data)
}

clean_sars_data <- function(report_data) {
  names(report_data) <-
    epitrix::clean_labels(names(report_data), sep = "")
  report_data$resultadonuevocoronavirussarscov2vegeneral <-
    epitrix::clean_labels(
      report_data$resultadonuevocoronavirussarscov2vegeneral)
  report_data$rangodeedadvegeneral <-
    epitrix::clean_labels(report_data$rangodeedadvegeneral)
  report_data$virusdetectadosvegeneral <-
    epitrix::clean_labels(report_data$virusdetectadosvegeneral)
  return(report_data)
}

get_cases_other_viruses <- function(report_data,
                                    epiweek = NULL,
                                    age_groups = FALSE,
                                    vrs_influenza = NULL) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_viruses <- config::get(file = config_path, "viruses")
  invalid_results <- config::get(file = config_path,
                                 "other_viruses")$invalid_results
  positive_cases <- data.frame()
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  if (!is.null(epiweek) && epiweek != "all") {
    report_data <- report_data %>%
      dplyr::filter(!!dplyr::sym(col_epiweek) == as.numeric(epiweek))
  }
  if (!is.null(vrs_influenza)) {
    cols_viruses <-
      get_influenza_viruses(viruses = cols_viruses,
                            events = vrs_influenza)
  }
  for (virus in cols_viruses) {
    other_vrs <- virus$other_viruses
    if (other_vrs$col_name %in% names(report_data) &&
        !("" %in% other_vrs$values)) {
      cases_virus <- report_data
      if ("valid_result" %in% names(other_vrs)) {
        for (value in invalid_results$values) {
          if (value != "") {
            cases_virus <- cases_virus[which(!stringr::str_detect(
              invalid_results$col_name, value)), ]
          }
        }
      }
      if ("col_subtypes" %in% names(other_vrs)) {
        subtypes_values <-
          get_subtypes_values(viruses = cols_viruses,
                              subtypes = virus$subtypes)
        other_vrs$values <- subtypes_values
        cases_virus <- cases_virus[
          which(stringr::str_detect(cases_virus[[other_vrs$col_name]],
                                    other_vrs$original_value)), ]
      }
      positive_cases_virus <- data.frame()
      for (value in other_vrs$values) {
        if (value != "") {
          if ("col_subtypes" %in% names(other_vrs)) {
            cases_virus <- cases_virus[which(!stringr::str_detect(
              cases_virus[[other_vrs$col_name]], value)), ]
          } else {
            cases_virus <- cases_virus[which(stringr::str_detect(
              cases_virus[[other_vrs$col_name]], value)), ]
          }
          if (!is.null(epiweek)) { 
            cases_virus <- group_columns_total(disease_data = cases_virus,
                                               event_name = virus$name,
                                               col_names = col_epiweek,
                                               event_label = virus$label)  
          }
          positive_cases_virus <- rbind(positive_cases_virus, cases_virus)
        }
      }
      if (age_groups) {
        cases_age_groups <-
          generate_age_groups_viruses(report_data = cases_virus,
                                      event_name = virus$name,
                                      wt_percentage = TRUE,
                                      total_cases = nrow(cases_virus),
                                      event_label = virus$label)
        cases_age_groups$total_casos <- nrow(cases_virus)
        positive_cases <- rbind(positive_cases, cases_age_groups)
      } else {
        positive_cases_virus$total_casos <- nrow(cases_virus)
        positive_cases <- rbind(positive_cases, positive_cases_virus)
      }
    }
  }
  return(positive_cases)
}


get_subtypes_values <- function(viruses, subtypes) {
  subtypes_values <- NULL
  for (virus in viruses) {
    if (virus$name %in% subtypes) {
      other_vrs <- virus$other_viruses
      subtypes_values <- c(subtypes_values, other_vrs$values)
    }
  }
  return(subtypes_values)
}

generate_age_groups_viruses <- function(report_data,
                                        event_name = "adenovirus",
                                        wt_percentage = FALSE,
                                        total_cases = 0,
                                        event_label) {
  data_grouped  <- report_data %>% dplyr::group_by(
    dplyr::across(
      dplyr::all_of("rangodeedadvegeneral"))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  third_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_5_y_9_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_10_y_14_anos"][1], 
        na.rm = TRUE)
  four_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_15_y_19_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_20_y_29_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_30_y_39_anos"][1],
        na.rm = TRUE)
  five_group_age <- 
    sum(data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_40_y_49_anos"][1],
        data_grouped$casos[data_grouped$rangodeedadvegeneral ==
                             "entre_50_y_59_anos"][1], 
        na.rm = TRUE)
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "1_ano"]  <- "< 2 años"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_1_y_4_anos"]  <- "2 a 4 años"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "60_y_mas_anos"]  <- "60 y más"
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_5_y_9_anos"]  <- "5 a 14 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "5 a 14 años"] <-
    third_group_age
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_15_y_19_anos"]  <- "15 a 39 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "15 a 39 años"] <-
    four_group_age
  
  data_grouped$rangodeedadvegeneral[data_grouped$rangodeedadvegeneral ==
                                      "entre_40_y_49_anos"]  <- "40 a 59 años"
  data_grouped$casos[data_grouped$rangodeedadvegeneral == "40 a 59 años"] <-
    five_group_age
  
  if (length(which(stringr::str_detect(data_grouped$rangodeedadvegeneral,
                                       "_"))) > 0) {
    data_grouped <-
      data_grouped[-which(stringr::str_detect(data_grouped$rangodeedadvegeneral,
                                              "_")), ]
  }
  if (length(which(is.na(data_grouped$rangodeedadvegeneral))) > 0) {
    data_grouped <-
      data_grouped[-which(is.na(data_grouped$rangodeedadvegeneral)), ]
  }
  colnames(data_grouped)[colnames(data_grouped) == "rangodeedadvegeneral"] <-
    "grupo_edad"
  if (total_cases > 0) {
    data_grouped  <-  data_grouped %>% dplyr::mutate(
      porcentaje = round((data_grouped$casos/total_cases)*100, 1))
  } else {
    data_grouped  <-  data_grouped %>% dplyr::mutate(
      porcentaje = 0.0)
  }
  data_grouped  <-  data_grouped %>%
    dplyr::mutate(evento = event_name, etiqueta = event_label)
  data_grouped <-
    complete_age_categories(data_grouped = data_grouped,
                            event_name = event_name,
                            event_label = event_label)
  
  return(data_grouped)
  
}

complete_age_categories <- function(data_grouped,
                                    event_name,
                                    event_label) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  categorie_labels <- config::get(file = config_path,
                                  "age_categories")$age_categories
  for (label in categorie_labels) {
    if (!any(data_grouped == label) || is.na(any(data_grouped == label))) {
      new_row <- data.frame(grupo_edad = label,
                            casos = 0,
                            porcentaje = 0,
                            evento = event_name,
                            etiqueta = event_label)
      data_grouped <- rbind(data_grouped, new_row)
    }
  }
  return(data_grouped)
}

#*************************************** PIPELINE****************************************
#****************************************************************************************


url_data <- "C:/Users/willi/Documents/labrep-import/dataCopied/datos1/OTROSVIRUS_SE 23.xlsx"


other_viruses_data <- import_data_viral_circulation(report_data = url_data,skip = 0,sheet = 1) #PRE DATA 2
other_viruses_cleaned_data  <- clean_data_other_viruses(other_viruses_data) # PRE DATA 2
other_viruses_age_groups <- get_cases_other_viruses(report_data = other_viruses_cleaned_data,age_groups = TRUE) #DATA 2 (?)


#Inspecting Data

head(other_viruses_age_groups)
str(other_viruses_age_groups)
unique(other_viruses_age_groups$grupo_edad)
hist(other_viruses_age_groups$total_casos)

#Inspecting Cleaned Data

colnames(other_viruses_cleaned_data)
View(other_viruses_cleaned_data)




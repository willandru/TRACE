

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





clean_filmarray_data <- function(filmarray_data) {
  data_clean <- filmarray_data
  names(data_clean) <-
    epitrix::clean_labels(names(data_clean))
  data_clean <- clean_filmarray_age(data_clean)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  col_epiweek <- config::get(file = config_path,
                             "filmarray_data")$epiweek$col_valid
  index_col <- which(names(data_clean) %in% col_epiweek)
  if (length(index_col) > 0) {
    data_clean[[col_epiweek]] <- as.numeric(data_clean[[col_epiweek]])
  }
  return(data_clean)
}


clean_filmarray_age <- function(filmarray_data,
                                col_age = "edad") {
  data_age_clean <- filmarray_data
  data_age_clean[[col_age]] <- tolower(data_age_clean[[col_age]])
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_clean_age_groups <-
    config::get(file = config_path, "filmarray_data")$age_groups$cols_clean
  col_age_groups <- config::get(file = config_path,
                                "filmarray_data")$age_groups$col_valid
  index_col <- which(cols_clean_age_groups %in% names(data_age_clean))
  if (length(index_col) > 0) {
    col_clean <- cols_clean_age_groups[index_col]
    names(data_age_clean)[names(data_age_clean) == col_clean] <- col_age_groups
    data_age_clean[[col_age_groups]] <-
      tolower(data_age_clean[[col_age_groups]])
  }
  if (col_age %in% colnames(data_age_clean)) {
    patterns <- c(" año", " mes", " dia", " día")
    space_indexes <- NULL
    for (pattern in patterns) {
      aux <- which(stringr::str_detect(data_age_clean[[col_age]],
                                       stringr::fixed(pattern)))
      if (length(aux) > 0) {
        space_indexes <-
          c(space_indexes, aux)
      }
    }
    if (length(space_indexes) != nrow(data_age_clean)) {
      total_indexes <- seq_len(nrow(data_age_clean))
      missing_indexes <- setdiff(total_indexes, space_indexes)
      data_age_clean[[col_age]][missing_indexes] <-
        sapply(data_age_clean[[col_age]][missing_indexes],
               add_character, char = " ")
    }
    data_age_clean[[col_age_groups]][data_age_clean[[col_age_groups]]
                                     == "60 y mas"] <- "60 y más"
  }
  return(data_age_clean)
}

add_character <- function(value, char) {
  init_pos <- regexpr("años|año|mes|meses|día|días|dias", value)[1]
  if (init_pos > -1) {
    value <- paste0(substring(value, 1, init_pos - 1),
                    char,
                    substring(value, init_pos))
  }
  return(value)
}


generate_age_categories <- function(dataset) {
  if (!any(names(dataset) == "grupo_edad")) {
    data_ages <- cbind(dataset, grupo_edad = NA)
    data_ages[, ncol(data_ages)] <- sapply(data_ages$edad,
                                           define_age_category)
    return(data_ages)
  } else {
    return(dataset)
  }
}


get_cases_filmarray <- function(report_data,
                                positive_value = "DETECTADO",
                                age_groups = TRUE,
                                epiweek = NULL,
                                vrs_influenza = NULL) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  cols_viruses <- config::get(file = config_path, "viruses")
  col_age_groups <- config::get(file = config_path,
                                "filmarray_data")$age_groups$col_valid
  col_epiweek <- config::get(file = config_path,
                             "filmarray_data")$epiweek$col_valid
  if (!is.null(vrs_influenza)) {
    cols_viruses <-
      get_influenza_viruses(viruses = cols_viruses,
                            events = vrs_influenza)
  }
  if (!is.null(epiweek)  && epiweek != "all") {
    col_epiweek <- config::get(file = config_path,
                               "filmarray_data")$epiweek$col_valid
    report_data <-
      report_data[which(report_data[[col_epiweek]]
                        == as.numeric(epiweek)), ]
  }
  viruses_age_group <- data.frame()
  for (virus in cols_viruses) {
    filmarray_vrs <- virus$filmarray
    if (all(filmarray_vrs$col_name %in% names(report_data)) &&
        !("" %in% filmarray_vrs$values)) {
      for (col_name in filmarray_vrs$col_name) {
        positive_cases <-
          report_data[report_data[[col_name]]
                      == filmarray_vrs$values, ]
        if (!is.null(epiweek)) {
          positive_cases <- group_columns_total(disease_data = positive_cases,
                                                event_name = virus$name,
                                                col_names = col_epiweek,
                                                event_label = virus$label) 
        }
        if (age_groups) {
          positive_cases_age_group <-
            group_columns_total(positive_cases,
                                col_age_groups,
                                event_name = virus$name,
                                wt_percentage = TRUE,
                                total_cases = nrow(positive_cases),
                                event_label = virus$label)
          positive_cases_age_group <-
            complete_age_categories(data_grouped = positive_cases_age_group,
                                    event_name = virus$name,
                                    event_label = virus$label)
          positive_cases_age_group$total_casos <-
            nrow(positive_cases)
          viruses_age_group <-
            rbind(viruses_age_group, positive_cases_age_group)
        } else {
          positive_cases$total_casos <-
            nrow(positive_cases)
          viruses_age_group <-
            rbind(viruses_age_group, positive_cases)
        }
      }
    }
  }
  na_values <- which(is.na(
    viruses_age_group[[col_age_groups]]))
  if (length(na_values) > 0) {
    viruses_age_group <-
      viruses_age_group[-na_values, ]
  }
  sd_values <- which(viruses_age_group[[col_age_groups]]
                     == "SD")
  if (length(sd_values) > 0) {
    viruses_age_group <-
      viruses_age_group[-sd_values, ]
  }
  return(viruses_age_group)
}


group_columns_total <- function(disease_data,
                                event_name = "adenovirus",
                                col_names,
                                wt_percentage = FALSE,
                                total_cases = 0,
                                event_label = NULL,
                                sum_cases = FALSE,
                                col_order = NULL,
                                etiqueta = TRUE) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  categorie_labels <- config::get(file = config_path,
                                  "age_categorie_labels")
  if (!sum_cases) {
    disease_data_grouped  <- disease_data %>% dplyr::group_by(
      dplyr::across(dplyr::all_of(col_names))) %>%
      dplyr::summarise(casos = dplyr::n(), .groups = "drop")   
  } else {
    disease_data_grouped <-
      disease_data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>%
      dplyr::summarise(casos = sum(.data$casos), .groups = "drop")
  }
  if (wt_percentage) {
    if (total_cases == 0) {
      total_cases <- sum(disease_data_grouped$casos)
    }
    disease_data_grouped  <-  disease_data_grouped %>%
      dplyr::mutate(porcentaje =
                      round((disease_data_grouped$casos / total_cases) * 100,
                            1))
  }
  if (!("etiqueta" %in% col_names) && etiqueta) {
    disease_data_grouped  <-  disease_data_grouped %>%
      dplyr::mutate(evento = event_name, etiqueta = event_label) 
  }
  if (is.null(event_label)) {
    for (label in categorie_labels) {
      if (!any(disease_data_grouped == label) ||
          is.na(any(disease_data_grouped == label))) {
        new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0,
                              evento = event_name, etiqueta = event_label)
        disease_data_grouped <- rbind(disease_data_grouped, new_row)
      }
    }
  }
  if ("semanaepidemiologicavegeneral" %in% col_names) {
    for (i in 1:52) {
      if (!any(disease_data_grouped$semanaepidemiologicavegeneral == i)
          || is.na(any(disease_data_grouped$semanaepidemiologicavegeneral
                       == i))) {
        if ("porcentaje" %in% col_names) {
          new_row <- data.frame(semanaepidemiologicavegeneral = i,
                                casos = 0,
                                porcentaje  = 0,
                                evento = event_name,
                                etiqueta = event_label)
        } else {
          new_row <- data.frame(semanaepidemiologicavegeneral = i,
                                casos = 0,
                                evento = event_name,
                                etiqueta = event_label)
        }
        disease_data_grouped <- rbind(disease_data_grouped, new_row)
      }
    }
  }
  if (!is.null(col_order)) {
    disease_data_grouped <- disease_data_grouped %>%
      dplyr::arrange(dplyr::desc(!!dplyr::sym(col_order)))
  }
  return(disease_data_grouped)
}

get_dist_fci_other_vrs <- function(fci_data, vrs_data) {
  dist_fci_other_vrs <- rbind(fci_data, vrs_data)
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("evento", "grupo_edad","etiqueta")))) %>%
    dplyr::summarise(casos = sum(casos),
                     total_casos = sum(total_casos),
                     .groups = "drop")
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::mutate(porcentaje =
                    round((.data$casos * 100)/.data$total_casos))
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::select(.data$grupo_edad,
                  .data$casos,
                  .data$porcentaje,
                  .data$evento,
                  .data$etiqueta)
  return(dist_fci_other_vrs)
}



plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  plot <- ggplot2::ggplot(report_data,
                          ggplot2::aes_string(x =
                                                factor(report_data[[var_x]],
                                                       levels =
                                                         category_labels),
                                              y = var_y,
                                              fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
    }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 11),
                   axis.title = ggplot2::element_text(face = "bold"),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  return(plot)
}



#*************************************** PIPELINE****************************************
#****************************************************************************************


url_data_otherviruses <- "C:/Users/willi/Documents/labrep-import/dataCopied/datos1/OTROSVIRUS_SE 23.xlsx"


other_viruses_data <- import_data_viral_circulation(report_data = url_data_otherviruses,skip = 0,sheet = 1) #PRE DATA 2
other_viruses_cleaned_data  <- clean_data_other_viruses(other_viruses_data) # PRE DATA 2
other_viruses_age_groups <- get_cases_other_viruses(report_data = other_viruses_cleaned_data,age_groups = TRUE) #DATA 2 (?)


#Inspecting Data

#head(other_viruses_age_groups)
#str(other_viruses_age_groups)
#unique(other_viruses_age_groups$grupo_edad)
#hist(other_viruses_age_groups$total_casos)

#Inspecting Cleaned Data

#colnames(other_viruses_cleaned_data)
#View(other_viruses_cleaned_data)



filmarray_data_url <- "C:/Users/willi/Documents/labrep-import/dataCopied/datos1/BASE FCI SE 23.xlsx"

filmarray_data <- import_data_viral_circulation(report_data = filmarray_data_url,sheet = 1,skip = 3) # PRE DATA1
filmarray_data_cleaned <- clean_filmarray_data(filmarray_data = filmarray_data) # PRE DATA1
filmarray_data_cleaned <- generate_age_categories(dataset = filmarray_data_cleaned) #PRE DATA1
filmarray_age_groups <- get_cases_filmarray(report_data = filmarray_data_cleaned,age_groups = TRUE) #DATA 1 (?)



## FINAL DATASETS
head(other_viruses_age_groups)
head(filmarray_age_groups)

viruses_age_groups <- get_dist_fci_other_vrs(fci_data = filmarray_age_groups, vrs_data = other_viruses_age_groups)  # DATA1 + DATA2 =  PLOT, TABLE, PARAGRAPH (?)


#**************************PLOTTING***********************************
#*********************************************************************

convert_age_groups_as_cols <- function(dataset) {
config_path <- system.file("extdata", "config.yml", package = "labrep")
category_labels <-
  config::get(file = config_path,
              "age_categories")$age_categories
category_labels <- c("etiqueta", category_labels)
data_groups <- dataset %>%
  dplyr::select(.data$etiqueta, .data$grupo_edad, .data$casos) %>% # Seleccionar columnas relevantes
  tidyr::pivot_wider(
    names_from = .data$grupo_edad, # Columna que se convierte en encabezados
    values_from = .data$casos      # Valores que llenan la tabla
  )
cols_order <- factor(colnames(data_groups),
                     levels = category_labels)
data_groups <- data_groups %>%
  dplyr::select(dplyr::all_of(levels(cols_order)))
return(data_groups)
}

plot_table_legend <- function(report_data,
                              include_sars = FALSE) {
  report_data$cs <- ""
  report_data <- report_data %>%
    dplyr::arrange(.data$etiqueta) %>%
    dplyr::relocate(.data$cs, .after = .data$etiqueta)
  colors <- get_colors_age_groups(order = TRUE,
                                  hex_cods = TRUE,
                                  include_sars = include_sars)
  col_names <- names(report_data)
  table <- knitr::kable(report_data,
                        col.names = NULL,
                        align = "c",
                        longtable = TRUE) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      latex_options = c("bordered", "hold_position"),
      font_size = 9
    )  %>%
    kableExtra::column_spec(2, background = colors) %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(length(col_names), border_right = TRUE) %>%
    kableExtra::column_spec(column = seq(3, length(col_names)),
                            width = "1.6cm") %>%
    kableExtra::column_spec(length(col_names), border_right = TRUE,
                            width = "1.7cm")
  return(table)
}
trans_viruses_age_groups <- convert_age_groups_as_cols(dataset = viruses_age_groups) #PREPARE DATA FOR TABLE
plot_table_legend(report_data = trans_viruses_age_groups) #TABLE

plot_age_group_distribution(viruses_age_groups)

#** mine **
#*

library(ggplot2)
library(rlang)
library(scales)
library(config)

library(ggplot2)
library(gridExtra)
library(grid)
library(rlang)
library(config)
library(scales)


plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  plot <- ggplot2::ggplot(report_data,
                          ggplot2::aes_string(x =
                                                factor(report_data[[var_x]],
                                                       levels =
                                                         category_labels),
                                              y = var_y,
                                              fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format(),
                                  expand = ggplot2::expansion(mult = c(0.05, 0.1))) # Espacio en Y
    }
    } +
    ggplot2::geom_segment(
      aes(x = 0.5, xend = length(category_labels) + 0.5, # Desde el inicio al final del eje X
          y = -0.1, yend = -0.1),                       # Línea horizontal debajo del gráfico
      color = "black"
    ) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(family = "Montserrat", size = 11),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1, 1, 5, 5), "lines") # Margen inferior e izquierdo aumentados
    ) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios") +
    ggplot2::coord_cartesian(ylim = c(-0.2, 1)) # Fijar límites en Y
  return(plot)
}

library(ggplot2)
library(patchwork)

plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  
  # Gráfico principal
  main_plot <- ggplot2::ggplot(report_data,
                               ggplot2::aes_string(x =
                                                     factor(report_data[[var_x]],
                                                            levels =
                                                              category_labels),
                                                   y = var_y,
                                                   fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
    }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(family = "Montserrat", size = 11),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines")
    ) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  
  # Línea horizontal como gráfico separado
  line_plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(aes(x = 0.5, xend = length(category_labels) + 0.5,
                              y = 0, yend = 0),
                          color = "black", size = 0.5) +
    ggplot2::theme_void() + # Eliminar todo excepto la línea
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(0, 1, 1, 1), "lines") # Márgenes ajustados
    )
  
  # Combinar el gráfico principal con la línea horizontal
  combined_plot <- main_plot / line_plot +
    patchwork::plot_layout(heights = c(10, 1)) # Ajustar proporciones
  return(combined_plot)
}


library(ggplot2)
library(patchwork)
library(gridExtra)

# Define the function to create a plot
plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  
  # Main bar plot
  main_plot <- ggplot2::ggplot(report_data,
                               ggplot2::aes_string(x =
                                                     factor(report_data[[var_x]],
                                                            levels =
                                                              category_labels),
                                                   y = var_y,
                                                   fill = var_fill)) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
    }} +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(family = "Montserrat", size = 11),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines")
    ) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  
  return(main_plot)
}

# Define the function to create a table as a grob (table graphic object)
plot_table_legend <- function(report_data,
                              include_sars = FALSE) {
  report_data$cs <- ""
  report_data <- report_data %>%
    dplyr::arrange(.data$etiqueta) %>%
    dplyr::relocate(.data$cs, .after = .data$etiqueta)
  colors <- get_colors_age_groups(order = TRUE,
                                  hex_cods = TRUE,
                                  include_sars = include_sars)
  
  # Create a table grob
  table_grob <- gridExtra::tableGrob(report_data,
                                     rows = NULL, # Remove row names
                                     theme = gridExtra::ttheme_default(
                                       core = list(bg_params = list(fill = colors)),
                                       colhead = list(fg_params = list(fontface = "bold"))
                                     ))
  return(table_grob)
}

# Combine the plot and the table
combine_plot_and_table <- function(plot, table) {
  combined <- plot / table + patchwork::plot_layout(heights = c(3, 1)) # Adjust the heights
  return(combined)
}

# Example usage
# Assuming `viruses_age_groups` is your dataset
plot <- plot_age_group_distribution(viruses_age_groups)
table <- plot_table_legend(viruses_age_groups)
combined <- combine_plot_and_table(plot, table)
print(combined)


library(ggplot2)
library(gridExtra)
library(patchwork)

plot_age_group_distribution <- function(report_data,
                                        var_x = "grupo_edad",
                                        var_y = "porcentaje",
                                        var_fill = "etiqueta",
                                        stacked_percentage = TRUE,
                                        include_sars = FALSE) {
  # Colors and category labels
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  
  # Main bar plot
  main_plot <- ggplot2::ggplot(report_data,
                               ggplot2::aes_string(x =
                                                     factor(report_data[[var_x]],
                                                            levels =
                                                              category_labels),
                                                   y = var_y,
                                                   fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
    }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(family = "Montserrat", size = 11),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines")
    ) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  
  # Generate table using your plot_table_legend function
  table_grob <- plot_table_legend(report_data, include_sars)
  
  # Combine plot and table
  combined_plot <- main_plot / table_grob +
    patchwork::plot_layout(heights = c(10, 3)) # Adjust height proportions
  
  return(combined_plot)
}



plot_age_group_distribution(viruses_age_groups)



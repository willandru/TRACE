#Begin

### IMPORTING DATA

import_data_viral_circulation <- function(report_data = NULL,header = FALSE,skip = 3,col_names = FALSE,sheet) {
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
row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) {
    names(data) <- as.character(unlist(data[row_num, ]))
    data[-row_num, ]
  }
}
fci_data <- 'BASE FCI SE 23.xlsx'

filmarray_data <- import_data_viral_circulation(report_data = fci_data, sheet = 1,skip = 3)

#--OK (Se importó la tabla)

str(filmarray_data)

# -- OK (número de Filas/columnas y tiene headers)

### CLEANING DATA


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
clean_filmarray_age <- function(filmarray_data,col_age = "edad") {
  data_age_clean <- filmarray_data
  data_age_clean[[col_age]] <- tolower(data_age_clean[[col_age]])
  
  
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  
  cols_clean_age_groups <- config::get(file = config_path, "filmarray_data")$age_groups$cols_clean
  
  col_age_groups <- config::get(file = config_path,  "filmarray_data")$age_groups$col_valid
  
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
      
      aux <- which(stringr::str_detect(data_age_clean[[col_age]], stringr::fixed(pattern)))
      
      if (length(aux) > 0) {
        space_indexes <- c(space_indexes, aux)
      }
      
    }
    if (length(space_indexes) != nrow(data_age_clean)) {
      
      total_indexes <- seq_len(nrow(data_age_clean))
      
      missing_indexes <- setdiff(total_indexes, space_indexes)
      
      data_age_clean[[col_age]][missing_indexes] <- sapply(data_age_clean[[col_age]][missing_indexes],add_character, char = " ")
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

filmarray_data_cleaned <- clean_filmarray_data(filmarray_data = filmarray_data)

# -- OK (running)

colnames(filmarray_data)
colnames(filmarray_data_cleaned)


# -- OK (colnames)

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

filmarray_data_cleaned <-  generate_age_categories(dataset = filmarray_data_cleaned)
filmarray_data_cleaned_age_categories <-  generate_age_categories(dataset = filmarray_data_cleaned)

# -- OK running

str(filmarray_data_cleaned)
str(filmarray_data_cleaned_age_categories)

# -- ALERT - No veo la diferencia entre las dos tablas


get_cases_filmarray <- function(report_data, positive_value = "DETECTADO",age_groups = TRUE,epiweek = NULL,vrs_influenza = NULL) {
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
group_columns_total <- function(disease_data, event_name = "adenovirus",col_names,wt_percentage = FALSE,total_cases = 0,event_label = NULL,sum_cases = FALSE,col_order = NULL, etiqueta = TRUE) {
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
library('dplyr')
complete_age_categories <- function(data_grouped,event_name,event_label) {
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

filmarray_age_groups <- get_cases_filmarray(report_data = filmarray_data_cleaned,age_groups = TRUE)

# -- OK running





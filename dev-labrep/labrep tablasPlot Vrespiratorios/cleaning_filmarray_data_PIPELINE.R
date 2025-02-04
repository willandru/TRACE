url_fci <- "C:/Users/willi/Documents/labrep-import/dataCopied/datos1/BASE FCI SE 23.xlsx"

## ---IMPORTS

library(tidyr)
library(dplyr)
library(stringr)
library(stringi)
library(janitor)

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

#************************CHECKING_DATA*****************************
#*******************************************************************
# Función que verifica si una columna puede ser usada como columna ID
es_columna_id <- function(df, columna) {
  unique_values <- length(unique(df[[columna]]))  # Cuenta los valores únicos en la columna
  total_rows <- nrow(df)  # Cuenta el número total de filas en el dataframe
  
  # Retorna TRUE si los valores únicos son iguales al número de filas, de lo contrario FALSE
  return(unique_values == total_rows)
}


#**************************CLEANING-DATA*****************************
#********************************************************************

fix_colnames <- function(df) {
  df <- df %>% clean_names()
  return(df)
}


seleccionar_columnas <- function(df, colnames_lista) {
  df_seleccionado <- df[, colnames_lista, drop = FALSE]
  return(df_seleccionado)
}

replace_string_in_column <- function(df, column_name, search_string, replace_string) {
  df[[column_name]] <- df[[column_name]] %>%
    # Si hay coincidencia con el search_string, reemplaza toda la fila por replace_string
    ifelse(str_detect(., search_string), replace_string, .)
  return(df)
}

#*****************************UTILS************************************
#**********************************************************************
#Cantidad de valores unicos para cada columna
count_unique_values <- function(df) {
  result <- data.frame(
    col_name = names(df),
    Unicos = sapply(df, function(x) length(unique(x))),
    row.names = NULL
  )
  print(result)
}


#**************************PIPELINE***********************************
#*********************************************************************

filmarray_data <- import_data_viral_circulation(report_data = url_fci ,sheet = 1,skip = 3)

fillmarray_clean <- fix_colnames(filmarray_data)

colnames(fillmarray_clean)

COLUMNAS <- c("se",
              "n_de_orden",
              "edad",
              "sexo",
              "grupo_de_edad",
              "adenovirus",
              "coronavirus_229e",
              "coronavirus_hku1",
              "coronavirus_nl63",
              "coronavirus_oc43",
              "severe_acute_respiratoriy_syndrome_coronavir",
              "metapneumovirus_humano",
              "rinovirus_enterovirus_humano",
              "influenza_a",
              "influenza_a_h1",
              "influenza_a_h1_2009",
              "influenza_a_h3",
              "influenza_b",
              "virus_parainfluenza_1",
              "virus_parainfluenza_2",
              "virus_parainfluenza_3",
              "virus_parainfluenza_4",
              "virus_sincitial_respiratorio",
              "bordetella_pertussis",
              "chlamydophila_pneumoniae",
              "mycoplasma_pneumoniae",
              "bordetella_parapertussis")

data_cl <- seleccionar_columnas(fillmarray_clean, COLUMNAS)

count_unique_values(data_cl)

unique(data_cl$edad)

data_cl <- replace_string_in_column(data_cl, "sexo", "f","F")
data_cl <- replace_string_in_column(data_cl, "sexo", "m","M")

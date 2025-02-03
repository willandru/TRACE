url_fci <- "C:/Users/willi/Documents/labrep-import/dataCopied/datos1/BASE FCI SE 23.xlsx"


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


#**************************PIPELINE***********************************
#*********************************************************************

filmarray_data <- import_data_viral_circulation(report_data = url_fci ,sheet = 1,skip = 3)

fillmarray_clean <- fix_colnames(filmarray_data)

colnames(fillmarray_clean)

COLUMNAS <- c("radicado",
              "tipo_edad_ve_general",
              "rango_de_edad_ve_general",
              "genero_ve_general",
              "influenza_a_por_rt_pcr_ve_general",
              "influenza_b_por_rt_pcr_ve_general",
              "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general",
              "adenovirus_ad_v_por_rt_pcr_ve_general",
              "virus_detectados_ve_general",
              "resultado_nuevo_coronavirus_sars_co_v_2_ve_general"
)

data_cl <- seleccionar_columnas(fillmarray_clean, COLUMNAS)


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

#**************************CLEANING-DATA****************************

#CREAR UN DF CON 2 COLUMNAS: ID Y VIRUS_DETECTADO
crear_df_con_id <- function(df, nombre_columna) {
  # Verificamos si la columna existe en el dataframe
  if (!nombre_columna %in% colnames(df)) {
    stop("La columna no existe en el dataframe")
  }
  
  # Creamos el nuevo dataframe con la columna ID y la columna seleccionada
  nuevo_df <- data.frame(
    ID = seq_along(1:nrow(df)),  # Generamos los IDs en función del número de filas
    Valor = df[[nombre_columna]],  # Extraemos la columna por nombre
    stringsAsFactors = FALSE  # Aseguramos que no se convierta en factor
  )
  
  return(nuevo_df)
}
# SPLIT LOS VALORES DE VIRUS_DETECTADO USANDO ', / -'
split_virus_data <- function(df, columna_virus, columna_id) {
  # Crear una lista vacía para almacenar los resultados
  result <- list()
  
  # Recorrer las filas del data.frame
  for (i in 1:nrow(df)) {
    # Split del texto de la columna especificada por ',', '-', o '/'
    viruses <- strsplit(df[[columna_virus]][i], ",|/|-")[[1]]
    
    # Obtener el ID de la fila especificada
    id_value <- df[[columna_id]][i]
    
    # Agregar los resultados a la lista con el ID respectivo
    result[[i]] <- data.frame(ID = rep(id_value, length(viruses)), Valor = viruses, stringsAsFactors = FALSE)
  }
  
  # Unir todos los data.frames individuales en uno solo
  final_df <- do.call(rbind, result)
  
  return(final_df)
}

# Definición de la función
# Definición de la función
split_df <- function(df, columna_a_split, columna_con_ID) {
  
  # Crear una lista vacía para almacenar los resultados
  result <- list()
  
  # Recorrer las filas del data.frame
  for (i in 1:nrow(df)) {
    # Split del texto de la columna especificada por ',', '-', o '/'
    values_split <- strsplit(df[[columna_a_split]][i], ",|/|-")[[1]]
    
    # Obtener los valores de la fila actual
    row_data <- df[i, , drop = FALSE]  # Mantener la fila como un dataframe
    
    # Para cada valor del split, crear una fila duplicada con el valor correspondiente
    for (val in values_split) {
      row_data[[columna_a_split]] <- val  # Asignar el valor correspondiente al split
      result[[length(result) + 1]] <- row_data  # Agregar la nueva fila a la lista
    }
  }
  
  # Unir todos los data.frames individuales en uno solo
  final_df <- do.call(rbind, result)
  
  # Retornar el dataframe final
  return(final_df)
}





#LIMPIEZA DE LOS VALORES DE LA COLUMNA: Elimina espacios en blanco al incio y
#al final, convierte los valores a minusculas, elimina dobles espacios.
clean_column <- function(df, column_name) {
  df[[column_name]] <- df[[column_name]] %>%
    str_to_lower() %>%  # Convertir todo a minúsculas
    str_trim() %>%      # Eliminar espacios al inicio y al final
    str_squish() %>%    # Reemplazar múltiples espacios con uno solo
    stri_replace_all_regex("\\s+$", "") %>%  # Eliminar espacios al final
    stri_replace_all_regex("^\\s+", "")  # Eliminar espacios al inicio
  
  return(df)
}



replace_string_in_column <- function(df, column_name, search_string, replace_string) {
  df[[column_name]] <- df[[column_name]] %>%
    # Si hay coincidencia con el search_string, reemplaza toda la fila por replace_string
    ifelse(str_detect(., search_string), replace_string, .)
  return(df)
}


# Función para reemplazar valores exactos en una columna de un dataframe
replace_exact_match <- function(df, column_name, search_string, replace_string) {
  df[[column_name]] <- ifelse(df[[column_name]] == search_string, replace_string, df[[column_name]])
  return(df)
}



fix_colnames <- function(df) {
  df <- df %>% clean_names()
  return(df)
}


seleccionar_columnas <- function(df, colnames_lista) {
  df_seleccionado <- df[, colnames_lista, drop = FALSE]
  return(df_seleccionado)
}


#***************VISUALIZING******************************************************
#**********************************************************************************

#IMPRIME UNA TABLA CONTANDO LA FRECUENCIA DE LOS VALORES DE UNA COLUMNA
count_values <- function(df, column_name) {
  # Verificar si la columna existe en el data frame
  if (!column_name %in% colnames(df)) {
    stop("La columna especificada no existe en el data frame")
  }
  
  # Contar las ocurrencias de cada valor único y ordenar por frecuencia descendente
  df_counts <- df %>%
    count(!!sym(column_name)) %>%
    arrange(desc(n))
  
  return(knitr::kable(df_counts, col.names = c("Virus", "Frecuencia")))
}
count_values_2 <- function(df, column_name, column_id) {
  # Verificar si las columnas existen en el data frame
  if (!column_name %in% colnames(df)) {
    stop("La columna especificada para los valores no existe en el data frame")
  }
  if (!column_id %in% colnames(df)) {
    stop("La columna especificada para el ID no existe en el data frame")
  }
  
  # Contar las ocurrencias de cada valor único y ordenar por frecuencia descendente
  df_counts <- df %>%
    dplyr::group_by(!!rlang::sym(column_name)) %>%               # Agrupar por el valor de la columna
    dplyr::summarise(Frecuencia = n(),                           # Contar ocurrencias
                     Primer_ID = first(!!rlang::sym(column_id))) %>% # Extraer el primer ID de cada grupo
    dplyr::arrange(dplyr::desc(Frecuencia))                      # Ordenar por frecuencia descendente
  
  # Retornar el data frame con las frecuencias y primer ID
  return(df_counts)
}




#*************************************** PIPELINE****************************************
url_data <- "C:/Users/willi/Documents/labrep-import/dataCopied/datos1/OTROSVIRUS_SE 23.xlsx"
url_config <- "C:/Users/willi/Desktop/labrep tablasPlot Vrespiratorios/config2.yml"
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


data_otherVirus <- import_data_viral_circulation(report_data = url_data,skip = 0,sheet = 1) 

data_cl <- fix_colnames(data_otherVirus)
data_cl <- seleccionar_columnas(data_cl, COLUMNAS)


spl_df <- split_df(data_cl, "virus_detectados_ve_general", "radicado")

clean_Df <- clean_column(spl_df, "virus_detectados_ve_general")

unique(clean_Df$virus_detectados_ve_general)

#ADVERTENCIA: Algunos registros (filas) del dataframe continene mas de 2 virus detectados. 
# A continuación se limpiaran todos los registros del dataframe, pero porfavor tenga en cuenta 
#que se van a perder algunos casos detectados, pues no se manejo el problema de tener 2 o más 
#virus por regiustro. En este caso el primer virus detectado será el que se asigne a dicho ID.

replace_strings_df <- replace_string_in_column(clean_Df, "virus_detectados_ve_general", "movi", "Metaneumovirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "ade", "Adenovirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "rin", "Rinovirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "rhinovirus", "Rinovirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "riovirus", "Rinovirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "rivo", "Rinovirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "parain", "Parainfluenza")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "vsr", "VSR")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "sinc", "VSR")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "neg", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "ficiente", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "no se", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "no cu", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "no apt", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "boca", "Bocavirus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "pdm09", "H1N1 2009")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "h3", "H3N2")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "h1n1", "H1N1")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "oc43", "Coronavirus oc43")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "oc", "Coronavirus oc43")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "influenza b", "Influenza b")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "influenza a", "Influenza a")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "entero", "Otros Virus")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_detectados_ve_general", "nl63", "Otros Virus")
replace_strings_df <- replace_exact_match(replace_strings_df, "virus_detectados_ve_general", "1", "Negativo")
replace_strings_df <- replace_exact_match(replace_strings_df, "virus_detectados_ve_general", "2", "Negativo")
replace_strings_df <- replace_exact_match(replace_strings_df, "virus_detectados_ve_general", "3", "Negativo")
replace_strings_df <- replace_exact_match(replace_strings_df, "virus_detectados_ve_general", "2 y 3", "Negativo")
replace_strings_df <- replace_exact_match(replace_strings_df, "virus_detectados_ve_general", "positivo", "Negativo")



colnames(replace_strings_df)
#CHECK FRECUENCIES
tabla_frecuency <- count_values_2(replace_strings_df, "Valor", "ID")
print(tabla_frecuency, n=40)


# COLUMNA  ::  resultado_nuevo_coronavirus_sars_co_v_2_ve_general

replace_strings_df <- replace_string_in_column(replace_strings_df, "resultado_nuevo_coronavirus_sars_co_v_2_ve_general", "Positivo", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "resultado_nuevo_coronavirus_sars_co_v_2_ve_general", "Nega", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "resultado_nuevo_coronavirus_sars_co_v_2_ve_general", "no cumple", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "resultado_nuevo_coronavirus_sars_co_v_2_ve_general", "no se rea", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "resultado_nuevo_coronavirus_sars_co_v_2_ve_general", "ndeter", "Negativo")

unique(replace_strings_df$resultado_nuevo_coronavirus_sars_co_v_2_ve_general)



# COLUMNA  ::  adenovirus_ad_v_por_rt_pcr_ve_general


replace_strings_df <- replace_string_in_column(replace_strings_df, "adenovirus_ad_v_por_rt_pcr_ve_general", "Posi", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "adenovirus_ad_v_por_rt_pcr_ve_general", "POSI", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "adenovirus_ad_v_por_rt_pcr_ve_general", "Nega", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "adenovirus_ad_v_por_rt_pcr_ve_general", "no se rea", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "adenovirus_ad_v_por_rt_pcr_ve_general", "insuf", "Negativo")
replace_strings_df <- replace_exact_match(replace_strings_df, "adenovirus_ad_v_por_rt_pcr_ve_general", "Adenovirus", "Positivo")


unique(replace_strings_df$virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general)

# COLUMNA  ::  virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general

replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Aden", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Posi", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "POSI", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Posi", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Posi", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Sici", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Sincitial", "Positivo")

replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "Neg", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "virus_sincitial_respiratorio_vsr_por_rt_pcr_ve_general", "no se", "Negativo")


# COLUMNA  ::  influenza_a_por_rt_pcr_ve_general

replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_a_por_rt_pcr_ve_general", "NEG", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_a_por_rt_pcr_ve_general", "POS", "Positivo")

unique(replace_strings_df$influenza_a_por_rt_pcr_ve_general)


# COLUMNA  ::  influenza_b_por_rt_pcr_ve_general

replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_b_por_rt_pcr_ve_general", "NEG", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_b_por_rt_pcr_ve_general", "POS", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_b_por_rt_pcr_ve_general", "Neg", "Negativo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_b_por_rt_pcr_ve_general", "Pos", "Positivo")
replace_strings_df <- replace_string_in_column(replace_strings_df, "influenza_b_por_rt_pcr_ve_general", "no se rea", "Negativo")

unique(replace_strings_df$influenza_b_por_rt_pcr_ve_general)


# COLUMNA  ::  rango_de_edad_ve_general

replace_strings_df <- clean_column(replace_strings_df, "rango_de_edad_ve_general")
unique(replace_strings_df$rango_de_edad_ve_general)


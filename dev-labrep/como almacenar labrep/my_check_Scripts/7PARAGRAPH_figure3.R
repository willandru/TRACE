

get_cons_viruses_age_text <- function(dataset,col_name = "grupo_edad",text_group) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  text_cons_virues <- NULL
  vs_age_group_2 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[1]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  vs_age_group_2 <- vs_age_group_2[1:3, ]
  vs_age_group_4 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[2]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym("porcentaje")))
  vs_age_group_4 <- vs_age_group_4[1:3, ]
  text_cons_virues <-
    paste0(text_cons_virues,
           get_text_viruses(dataset = vs_age_group_2,
                            tam = nrow(vs_age_group_2)),
           text_group,
           get_text_viruses(dataset = vs_age_group_4,
                            tam = nrow(vs_age_group_4)))
  return(text_cons_virues)
}
get_text_viruses <- function(dataset, tam) {
  text_viruses <- NULL
  for (i in seq(1:tam)) {
    virus <- dataset[i, ]
    if (i < tam) {
      text_viruses <- paste0(text_viruses, virus$etiqueta,
                             " (", virus$porcentaje, " %), ")
    } else {
      token <- " y "
      if (startsWith(virus$etiqueta, prefix = "I")) {
        token <- " e "
      }
      text_viruses <- paste0(substr(text_viruses,
                                    1,
                                    nchar(text_viruses) - 2), token,
                             virus$etiqueta, " (", virus$porcentaje, " %)")
    }
  }
  return(text_viruses)
}


text_cons_viruses <- get_cons_viruses_age_text(dataset = viruses_age_groups,text_group = ", para los niños entre 2 y 4 años se presentan casos de ")
text_cons_viruses
#En la figura 3 se presenta el consolidado de los virus respiratorios por los
#diferentes grupos de edad, en los niños menores de 2 años las mayores frecuencias
#son de casos de `r text_cons_viruses`.


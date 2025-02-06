

get_perc_viruses_age <- function(dataset,  col_name = "grupo_edad") {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  vs_age_group_4 <- dataset %>%
    dplyr::filter(!!dplyr::sym(col_name) %in%
                    category_labels[1:2]) %>%
    dplyr::arrange(dplyr::desc(!!dplyr::sym(col_name)))
  perc_group_4 <-
    round((sum(vs_age_group_4$casos) * 100) / sum(dataset$casos), 1)
  return(perc_group_4)
}

perc_vrs_age_group <- get_perc_viruses_age(dataset = viruses_age_groups)
perc_sars_age_group <-  get_perc_viruses_age(dataset = sars_viruses_age_groups)

# -- OK (running)

#Para esta semana, por grupo de edad, el `r perc_vrs_age_group`% de
#pacientes positivos para virus respiratorios diferentes al SARS-CoV-2
#en `r current_year` corresponden a la población de menores de 5 años
#(figura 2) y si se incluye al SARS-CoV-2 los menores corresponden al
#`r perc_sars_age_group` % (figura 3).
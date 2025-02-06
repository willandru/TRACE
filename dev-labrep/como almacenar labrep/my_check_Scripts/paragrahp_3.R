viruses_age_groups <-
  get_dist_fci_other_vrs(fci_data = filmarray_age_groups,
                         vrs_data = other_viruses_age_groups)

sars_age_groups <- get_cases_sars(other_viruses_cleaned_data,
                                  age_groups = TRUE)
sars_viruses_age_groups <- get_distribution_age_vr_sars(viruses_age_groups,
                                                        sars_age_groups)
top_sars_age_groups <-
  group_columns_total(disease_data = sars_viruses_age_groups,
                      sum_cases = TRUE, col_names = "grupo_edad",
                      wt_percentage = TRUE, col_order = "porcentaje",
                      etiqueta = FALSE)

perc_vrs_age_group <-
  get_perc_viruses_age(dataset = viruses_age_groups)
perc_sars_age_group <-
  get_perc_viruses_age(dataset = sars_viruses_age_groups)


Para esta semana, por grupo de edad, el `r perc_vrs_age_group`% de
pacientes positivos para virus respiratorios diferentes al SARS-CoV-2
en `r current_year` corresponden a la población de menores de 5 años
(figura 2) y si se incluye al SARS-CoV-2 los menores corresponden al
`r perc_sars_age_group` % (figura 3).

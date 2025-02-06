


plot_age_group_distribution(esi_age_groups, include_sars = TRUE)

trans_esi_age_groups <-  convert_age_groups_as_cols(dataset = esi_age_groups)
plot_table_legend(report_data = trans_esi_age_groups, include_sars = TRUE)


# Distribución de virus respiratorios en casos de IRA Grave, según grupos de edad

irag_grave_age_groups <-get_distribution_surveillance(other_viruses_cleaned_data,include_sars = TRUE,surveillance_type = "irag_grave", epiweek = params$epiweek)
plot_age_group_distribution(irag_grave_age_groups, include_sars = TRUE)


trans_irag_grave_age_groups <-convert_age_groups_as_cols(dataset = irag_grave_age_groups)
plot_table_legend(report_data = trans_irag_grave_age_groups,include_sars = TRUE)



#Distribución de virus respiratorios en casos de IRAG Inusitado, según grupos de edad.
irag_inusitado_age_groups <-get_distribution_surveillance(other_viruses_cleaned_data,include_sars = TRUE,surveillance_type = "irag_inusitado",epiweek = params$epiweek)
irag_inusitado_age_groups <- irag_inusitado_age_groups %>% arrange(grupo_edad)

plot_age_group_distribution(irag_inusitado_age_groups,include_sars = TRUE)

trans_irag_grave_age_groups <- convert_age_groups_as_cols(dataset = irag_inusitado_age_groups)
plot_table_legend(report_data = trans_irag_grave_age_groups,include_sars = TRUE)




plot_age_group_distribution(sars_viruses_age_groups, var_y = "casos",include_sars = TRUE)


trans_sars_age_groups <-convert_age_groups_as_cols(dataset = sars_viruses_age_groups)
plot_table_legend(report_data = trans_sars_age_groups,include_sars = TRUE)

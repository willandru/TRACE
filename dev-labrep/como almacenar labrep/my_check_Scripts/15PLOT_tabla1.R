


distribution_epiweek <- get_cases_epiweeks(data_grouped =test_distribution_epiweek$cases_epiweeks,col_epiweek = "semanaepidemiologicavegeneral",table = TRUE)

plot_table_vrs_epiweek(distribution_epiweek,epiweek = params$epiweek)

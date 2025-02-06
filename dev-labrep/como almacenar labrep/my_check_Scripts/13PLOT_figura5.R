


test_distribution_epiweek <-get_distribution_test(report_data = other_viruses_cleaned_data,include_sars = FALSE)

plot_distribution_epiweek(report_data = test_distribution_epiweek$viruses_epiweeks,var_y = "casos",var_fill = "etiqueta",positives =test_distribution_epiweek$cases_epiweeks)
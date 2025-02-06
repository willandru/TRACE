
#Begin

get_cases_sars <- function(report_data,positive_value = "DETECTADO",age_groups = FALSE,epiweek = NULL) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  col_epiweek <- config::get(file = config_path,
                             "other_viruses")$epiweek$col_valid
  viruses_age_group <- data.frame()
  positive_cases_sars <-
    report_data[report_data$resultadonuevocoronavirussarscov2vegeneral ==
                  "positivo_para_nuevo_coronavirus_sars_cov_2", ]
  positive_cases_virusvgeneral <- report_data[which(stringr::str_detect(
    report_data$virusdetectadosvegeneral, "covid_19")), ]
  positive_cases_sars <- rbind(positive_cases_sars,
                               positive_cases_virusvgeneral)
  if (!is.null(epiweek)) {
    sars_epiweeks <-
      group_columns_total(positive_cases_sars,
                          event_name = "sars",
                          col_names = col_epiweek,
                          event_label = "SARS CoV 2")
    sars_epiweeks <-
      get_cases_epiweeks(report_data = report_data,
                         data_grouped = sars_epiweeks,
                         col_epiweek = col_epiweek)
    sars_epiweeks$evento <- "sars"
    sars_epiweeks$etiqueta <- "SARS CoV 2"
    if (epiweek != "all") {
      sars_epiweeks <- sars_epiweeks %>%
        dplyr::filter(!!dplyr::sym(col_epiweek) == as.numeric(epiweek))
    }
    return(sars_epiweeks)
  }
  if (age_groups) {
    sars_age_groups <-
      generate_age_groups_viruses(positive_cases_sars,
                                  event_name = "sars",
                                  wt_percentage = TRUE,
                                  total_cases = nrow(positive_cases_sars),
                                  event_label = "SARS CoV 2")
    return(sars_age_groups)
  }
  return(positive_cases_sars)
}

sars_age_groups <- get_cases_sars(other_viruses_cleaned_data,age_groups = TRUE)

# -- OK (running)

get_distribution_age_vr_sars <- function(data_vr, data_sars) {
  distribution_age_vr_sars <- rbind(data_vr, data_sars)
  return(distribution_age_vr_sars)
}

sars_viruses_age_groups <- get_distribution_age_vr_sars(viruses_age_groups,sars_age_groups)

# -- OK (running)

top_sars_age_groups <-group_columns_total(disease_data = sars_viruses_age_groups,sum_cases = TRUE, col_names = "grupo_edad", wt_percentage = TRUE, col_order = "porcentaje", etiqueta = FALSE)

# -- OK (running)
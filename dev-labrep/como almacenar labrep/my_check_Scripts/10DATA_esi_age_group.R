

get_distribution_surveillance <- function(report_data,epiweek = 0,include_sars = FALSE,surveillance_type = "esi",test = NULL) {
  viruses_age_group <- data.frame()
  report_data_esi <- report_data
  report_data_sars <- report_data
  report_data_others <- report_data
  report_data_irag <- report_data
  if (!is.null(surveillance_type)) {
    if (surveillance_type == "esi") {
      report_data_esi <- report_data_esi[
        report_data_esi$eventovegeneral == "esi_irag_centinela_345", ]
      report_data_esi <- report_data_esi[
        report_data_esi$clasificacionvegeneral == "ambulatorio", ]
    } else if (surveillance_type == "irag_grave") {
      report_data_esi <- report_data_esi[
        report_data_esi$eventovegeneral == "esi_irag_centinela_345", ]
      report_data_esi <- report_data_esi[
        which(stringr::str_detect(
          report_data_esi$clasificacionvegeneral, "hospitalizado")), ]
    } else if (surveillance_type == "irag_inusitado") {
      report_data_esi <- report_data_esi[
        report_data_esi$eventovegeneral == "irag_inusitado_348", ]
    }
  }
  if (epiweek > 0) {
    report_data_esi <- report_data_esi[
      report_data_esi$semanaepidemiologicavegeneral == epiweek, ]
  }
  report_data_esi <- report_data_esi[which(
    !is.na(
      report_data_esi$fechainiciodesintomasvegeneral)), ]
  
  if (!is.null(test)) {
    if (test == "bio_molecular") {
      positive_cases_sars <- dplyr::filter(report_data_esi ,
                                           .data$fluorescenciavegeneral == "la_prueba_no_se_realiza" || 
                                             is.na(.data$fluorescenciavegeneral))
    } else if (test == "fluorescencia") {
      report_data_esi <-
        dplyr::filter(
          report_data_esi,
          .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
            .data$fluorescenciavegeneral != "la_prueba_no_se_realiza" &&
            .data$fluorescenciavegeneral != "muestra_escasa_de_células" &&
            .data$fluorescenciavegeneral != "muestra_insuficiente_no_se_procesa"
          && .data$fluorescenciavegeneral 
          != "no_se_procesa_ifi_tiempo_de_toma_de_muestra_superior_a_7_dias_nota_2" &&
            !is.na(.data$fluorescenciavegeneral))
    }
  }
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  column_names <- config::get(file = config_path, "respiratory_virus_detected")
  names <- config::get(file = config_path, "respiratory_virus_detected_names")
  i <- 1
  for (column in column_names) {
    positive_cases <- report_data_esi[which(stringr::str_detect(
      report_data_esi$virusdetectadosvegeneral, column)), ]
    positive_cases_age_group <- generate_age_groups_viruses(positive_cases, 
                                                            event_name = column, 
                                                            wt_percentage = TRUE, 
                                                            total_cases = nrow(positive_cases), 
                                                            event_label = names[i])
    viruses_age_group <- rbind(viruses_age_group, positive_cases_age_group)
    i <- i + 1
  }
  if (include_sars) {
    positive_cases <- report_data_esi[
      report_data_esi$resultadonuevocoronavirussarscov2vegeneral 
      == "positivo_para_nuevo_coronavirus_sars_cov_2", ]
    positive_cases_virusvgeneral <- report_data_esi[which(stringr::str_detect(
      report_data_esi$virusdetectadosvegeneral, "covid_19")), ]
    if (nrow(positive_cases_virusvgeneral) > 1) {
      positive_cases <- rbind(positive_cases, positive_cases_virusvgeneral)
    }
    positive_cases_age_group <-
      generate_age_groups_viruses(positive_cases,
                                  event_name = "sars",
                                  wt_percentage = TRUE,
                                  total_cases = nrow(positive_cases),
                                  event_label = "SARS CoV 2")
    viruses_age_group <- rbind(viruses_age_group, positive_cases_age_group)
  }
  return(viruses_age_group)
}

epiweek <- 23


esi_age_groups <- get_distribution_surveillance(report_data = other_viruses_cleaned_data, include_sars = TRUE, surveillance_type = "esi",epiweek = epiweek)

# -- OK (running)
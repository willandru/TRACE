
#Begin



get_dist_fci_other_vrs <- function(fci_data, vrs_data) {
  dist_fci_other_vrs <- rbind(fci_data, vrs_data)
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c("evento", "grupo_edad","etiqueta")))) %>%
    dplyr::summarise(casos = sum(casos),
                     total_casos = sum(total_casos),
                     .groups = "drop")
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::mutate(porcentaje =
                    round((.data$casos * 100)/.data$total_casos))
  dist_fci_other_vrs <- dist_fci_other_vrs %>%
    dplyr::select(.data$grupo_edad,
                  .data$casos,
                  .data$porcentaje,
                  .data$evento,
                  .data$etiqueta)
  return(dist_fci_other_vrs)
}

viruses_age_groups <- get_dist_fci_other_vrs(fci_data = filmarray_age_groups, vrs_data = other_viruses_age_groups)

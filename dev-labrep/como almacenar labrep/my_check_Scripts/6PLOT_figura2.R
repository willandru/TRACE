
#Begin

#PLOTTING

#FIGURE
plot_age_group_distribution <- function(report_data,var_x = "grupo_edad",var_y = "porcentaje",var_fill = "etiqueta",stacked_percentage = TRUE,include_sars = FALSE) {
  colors <- get_colors_age_groups(include_sars = include_sars)
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  plot <- ggplot2::ggplot(report_data,
                          ggplot2::aes_string(x =
                                                factor(report_data[[var_x]],
                                                       levels =
                                                         category_labels),
                                              y = var_y,
                                              fill = var_fill)) +
    ggplot2::geom_bar(position = "fill",
                      stat = "identity") +
    { if (stacked_percentage) {
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1),
                                  labels = scales::percent_format())
    }
    } +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") +
    ggplot2::ylab("Porcentaje de casos") +
    ggplot2::theme(legend.position = "none",
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 11),
                   axis.title = ggplot2::element_text(face = "bold"),
                   axis.title.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = colors,
                               name = "Virus respiratorios")
  return(plot)
}
get_colors_age_groups <- function(order = FALSE,hex_cods = FALSE,include_sars = FALSE) {
  colors <- c("Adenovirus" = "#9E4B9F",
              "Rinovirus" = "#145765",
              "Bocavirus" = "#D49392",
              "Parainfluenza" = "#64439B",
              "Influenza B" = "#B94846",
              "Metapneumovirus" = "#87C762",
              "VSR" = "#2274BB",
              "H1N1" = "#7451c0",
              "H1N1 2009" = "#9DB2D0",
              "H3N2" = "#7dcea0",
              "A no subtipificado" = "#F4802D",
              "Otros Virus" = "#4E82BE")
  if (include_sars) {
    colors <- c(colors, "SARS CoV 2" = "#e05f55")
  }
  if (order) {
    colors <- colors[order(names(colors))]
  }
  if (hex_cods) {
    color <- unname(colors)
  }
  return(colors)
}

plot_age_group_distribution(viruses_age_groups)

#TABLE

convert_age_groups_as_cols <- function(dataset) {
  config_path <- system.file("extdata", "config.yml", package = "labrep")
  category_labels <-
    config::get(file = config_path,
                "age_categories")$age_categories
  category_labels <- c("etiqueta", category_labels)
  data_groups <- dataset %>%
    dplyr::select(.data$etiqueta, .data$grupo_edad, .data$casos) %>% # Seleccionar columnas relevantes
    tidyr::pivot_wider(
      names_from = .data$grupo_edad, # Columna que se convierte en encabezados
      values_from = .data$casos      # Valores que llenan la tabla
    )
  cols_order <- factor(colnames(data_groups),
                       levels = category_labels)
  data_groups <- data_groups %>%
    dplyr::select(dplyr::all_of(levels(cols_order)))
  return(data_groups)
}

trans_viruses_age_groups <- convert_age_groups_as_cols(dataset = viruses_age_groups)

plot_table_legend <- function(report_data,include_sars = FALSE) {
  report_data$cs <- ""
  report_data <- report_data %>%
    dplyr::arrange(.data$etiqueta) %>%
    dplyr::relocate(.data$cs, .after = .data$etiqueta)
  colors <- get_colors_age_groups(order = TRUE,
                                  hex_cods = TRUE,
                                  include_sars = include_sars)
  col_names <- names(report_data)
  table <- knitr::kable(report_data,
                        col.names = NULL,
                        align = "c",
                        longtable = TRUE) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      latex_options = c("bordered", "hold_position"),
      font_size = 9
    )  %>%
    kableExtra::column_spec(2, background = colors) %>%
    kableExtra::column_spec(1, border_left = TRUE) %>%
    kableExtra::column_spec(length(col_names), border_right = TRUE) %>%
    kableExtra::column_spec(column = seq(3, length(col_names)),
                            width = "1.6cm") %>%
    kableExtra::column_spec(length(col_names), border_right = TRUE,
                            width = "1.7cm")
  return(table)
}

plot_table_legend(report_data = trans_viruses_age_groups)




viruses_cumulative_proportion <-get_viruses_cumulative_proportion(report_data = viruses_age_groups)

total_cases <- sum(viruses_cumulative_proportion$casos)
total_samples <- nrow(other_viruses_cleaned_data)
text_vrs_cumulative <- get_text_viruses(dataset = viruses_cumulative_proportion,tam = 4)

#Dentro de la circulación viral acumulada predominan `r text_vrs_cumulative` 
#cada uno respectivamente en las `r total_cases` muestras positivas de las 
#`r total_samples` analizadas (figura 6).

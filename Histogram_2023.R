  library(ggplot2)
  library(dplyr)
  library(forcats)
  library(tidyr)
  
  cve_data <- read.csv("Data/cve_data_2023.csv")
  
  options(repr.plot.width = 16, repr.plot.height = 8)
  blacklist <- c('Mitre', 'VulnDB', 'GitHub_M', 'PatchStack', 'VulDB', '@huntrdev', 'wordfence', 'WPScan', 'Talos', 'CERTVDE', 'ICS-CERT', 'TR-CERT', 'Snyk', 'HackerOne', 'Tenable')
  
  filtered_data <- cve_data[!tolower(cve_data$assignerShortName) %in% tolower(blacklist), ]
  cleaned_data <- filtered_data %>%
    filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")
  
  grouped_data <- cleaned_data %>%
    group_by(assignerShortName, baseSeverity) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    pivot_wider(names_from = baseSeverity, values_from = count, values_fill = 0)
  
  grouped_data <- grouped_data %>%
    select(assignerShortName, CRITICAL, HIGH, LOW, MEDIUM, total)

  grouped_data_sorted <- grouped_data %>%
    arrange(desc(total))

  grouped_data_sorted_demo <- head(grouped_data_sorted, 30)

  print(grouped_data_sorted_demo)
  melted_data <- grouped_data_sorted_demo %>%
    pivot_longer(cols = -assignerShortName, names_to = "Severity", values_to = "count") %>%
    filter(Severity != "total")
  
  severity_colors <- c('CRITICAL' = '#A958F5', 'HIGH' = 'red', 'MEDIUM' = 'orange', 'LOW' = '#3ABBF3')
  
  melted_data <- melted_data %>%
    filter(count != 0)
  
  melted_data <- melted_data %>%
    group_by(assignerShortName) %>%
    mutate(total_count = sum(count)) %>%
    ungroup()
  
  melted_data <- melted_data %>%
    mutate(assignerShortName = fct_reorder(assignerShortName, total_count, .desc = TRUE))
  
  ggplot(melted_data, aes(x = assignerShortName, y = count, fill = Severity)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3, color = "black", fontface = "bold") + # Add text labels
    scale_fill_manual(values = severity_colors) +
    labs(title = "Total Number of Vulnerabilities by Company",
         x = "Company",
         y = "Total Number of Vulnerabilities",
         fill = "Severity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
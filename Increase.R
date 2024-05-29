library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

file_path <- "Data/cve_data_2014-24.csv"
data <- read_csv(file_path)

blacklist <- c('Mitre', 'VulnDB', 'GitHub_M', 'PatchStack', 'VulDB', '@huntrdev', 'wordfence', 'WPScan', 'Talos', 'CERTVDE', 'ICS-CERT', 'TR-CERT', 'Snyk', 'HackerOne', 'Tenable')

filtered_data <- data[!tolower(data$assignerShortName) %in% tolower(blacklist), ]
data <- filtered_data %>%
  filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")

# CVE-YYYY-XXXXX
data <- data %>%
  mutate(year = str_split(cveId, "-") %>% map_chr(2),
         baseScore = as.numeric(baseScore))
data <- data[data$year != '2024', ]
data$year <- as.numeric(data$year)

vulnerability_counts <- data %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(year)

ggplot(vulnerability_counts, aes(x = year, y = count)) +
  geom_line(color = "green") + 
  geom_point(color = "blue", size = 3) +  # Add points
  labs(x = "Year", y = "Number of Vulnerabilities", title = "Number of Vulnerabilities Over Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


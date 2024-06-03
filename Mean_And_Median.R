library(tidyverse)
library(readr)
library(stringr)
library(purrr)
file_path <- "Data/cve_data_2014-24.csv"
data <- read_csv(file_path)

data <- filtered_data %>%
  filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")

data <- data %>%
  mutate(year = str_split(cveId, "-") %>% map_chr(2),
         baseScore = as.numeric(baseScore))

grouped_data <- data %>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")) %>%
  group_by(year) %>%
  summarise(median = median(baseScore), mean = mean(baseScore))

print(grouped_data)
grouped_data$year <- as.numeric(grouped_data$year)


ggplot(grouped_data, aes(x = year)) +
  geom_line(aes(y = median, color = "Median"), size = 1) +
  geom_line(aes(y = mean, color = "Mean"), size = 1) +
  labs(x = "Year", y = "BaseScore", title = "Median and Mean BaseScore Over Time", color = "Legend") +
  ylim(0, 10) +
  scale_x_continuous(breaks = seq(2016, 2024, by = 1)) +
  scale_color_manual(values = c("Median" = "blue", "Mean" = "red")) +
  theme_minimal()



library(tidyverse)

file_path <- "Data/cve_data_2014-24.csv"
data <- read_csv(file_path)


data <- filtered_data %>%
  filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")

data <- data %>%
  mutate(year = str_split(cveId, "-") %>% map_chr(2),
         baseScore = as.numeric(baseScore))

unique_years <- unique(data$year)

custom_palette <- rainbow(length(unique_years))


ggplot(data, aes(x = year, y = baseScore, fill = year)) +
  geom_boxplot() +
  labs(x = "Year", y = "BaseScore", title = "Distribution of BaseScore by Year") +
  theme_minimal() +
  scale_fill_manual(values = custom_palette) +  # Use custom color palette
  facet_wrap(~ year, scales = "free_x", nrow = 3)  # Create separate boxplots for each year

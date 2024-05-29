extract_cia_values <- function(vector_string) {
  matches <- gregexpr("(?:/C:([A-Z])|/I:([A-Z])|/A:([A-Z]))", vector_string)
  matches <- regmatches(vector_string, matches)
  c_value <- NA
  i_value <- NA
  a_value <- NA
  
  for (match in matches[[1]]) {
    code <- substr(match, 2, 2)
    value <- substr(match, 4, 4)
    if (code == "C") {
      c_value <- value
    } else if (code == "I") {
      i_value <- value
    } else if (code == "A") {
      a_value <- value
    }
  }
  
  return(list("confidentiality" = c_value, "integrity" = i_value, "availability" = a_value))
}

#extract_cia_values("CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")

impact_levels <- c('H', 'L', 'N')
categories <- c('critical', 'high', 'medium', 'low', 'none')

counts <- list()

for (category in categories) {
  counts[[category]] <- list(
    confidentiality = setNames(rep(0, length(impact_levels)), impact_levels),
    integrity = setNames(rep(0, length(impact_levels)), impact_levels),
    availability = setNames(rep(0, length(impact_levels)), impact_levels)
  )
}

get_counts <- function(data) {
  
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    #print(row['vectorString'])
    if (tolower(row['baseSeverity']) != 'moderate') {
      base_severity <- tolower(row['baseSeverity'])
      vector_string <- row['vectorString']
      cia_values <- extract_cia_values(vector_string)
      print(cia_values)
        counts[[base_severity]][['confidentiality']][[cia_values$confidentiality]] <- counts[[base_severity]][['confidentiality']][[cia_values$confidentiality]] + 1
        counts[[base_severity]][['integrity']][[cia_values$integrity]] <- counts[[base_severity]][['integrity']][[cia_values$integrity]] + 1
        counts[[base_severity]][['availability']][[cia_values$availability]] <- counts[[base_severity]][['availability']][[cia_values$availability]] + 1
    }
  }
  
  print(counts)
  return(counts)
}

cve_data <- read.csv("Data/cve_data_2023.csv")

blacklist <- c('Mitre', 'VulnDB', 'GitHub_M', 'PatchStack', 'VulDB', '@huntrdev', 'wordfence', 'WPScan', 'Talos', 'CERTVDE', 'ICS-CERT', 'TR-CERT', 'Snyk', 'HackerOne', 'Tenable')

filtered_data <- cve_data[!tolower(cve_data$assignerShortName) %in% tolower(blacklist), ]
cleaned_data <- filtered_data %>%
  filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")

counts <- get_counts(cleaned_data)

par(mfrow = c(5, 3), mar = c(4, 4, 2, 1))
lighter_colors <- c('H' = '#FF6666', 'L' = '#FFFF99', 'N' = '#99FF99')

for (i in 1:length(categories)) {
  category <- categories[i]
  for (j in 1:3) {
    subcategory <- c('confidentiality', 'integrity', 'availability')[j]
    bar_colors <- sapply(impact_levels, function(impact) lighter_colors[impact])
    #print(counts[[category]][[subcategory]])
    barplot(counts[[category]][[subcategory]], main = paste(category, "-", subcategory), xlab = "Impact Level", ylab = "Number of CVEs", col = bar_colors, names.arg = impact_levels)
  }
}

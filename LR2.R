library(readr)
library(dplyr)
library(purrr)
library(stringr)

extract_vector_values <- function(vector_string) {
  av_pattern <- "/AV:([NALP])"
  ac_pattern <- "/AC:([LH])"
  pr_pattern <- "/PR:([NLH])"
  ui_pattern <- "/UI:([NR])"
  c_pattern <- "/C:([NLH])"
  i_pattern <- "/I:([NLH])"
  a_pattern <- "/A:([NLH])"
  
  av_match <- regexpr(av_pattern, vector_string, perl = TRUE)
  ac_match <- regexpr(ac_pattern, vector_string, perl = TRUE)
  pr_match <- regexpr(pr_pattern, vector_string, perl = TRUE)
  ui_match <- regexpr(ui_pattern, vector_string, perl = TRUE)
  c_match <- regexpr(c_pattern, vector_string, perl = TRUE)
  i_match <- regexpr(i_pattern, vector_string, perl = TRUE)
  a_match <- regexpr(a_pattern, vector_string, perl = TRUE)
  
  av_value <- ifelse(av_match > 0, substr(vector_string, av_match + attr(av_match, "match.length") - 1, av_match + attr(av_match, "match.length") - 1), NA)
  ac_value <- ifelse(ac_match > 0, substr(vector_string, ac_match + attr(ac_match, "match.length") - 1, ac_match + attr(ac_match, "match.length") - 1), NA)
  pr_value <- ifelse(pr_match > 0, substr(vector_string, pr_match + attr(pr_match, "match.length") - 1, pr_match + attr(pr_match, "match.length") - 1), NA)
  ui_value <- ifelse(ui_match > 0, substr(vector_string, ui_match + attr(ui_match, "match.length") - 1, ui_match + attr(ui_match, "match.length") - 1), NA)
  c_value <- ifelse(c_match > 0, substr(vector_string, c_match + attr(c_match, "match.length") - 1, c_match + attr(c_match, "match.length") - 1), NA)
  i_value <- ifelse(i_match > 0, substr(vector_string, i_match + attr(i_match, "match.length") - 1, i_match + attr(i_match, "match.length") - 1), NA)
  a_value <- ifelse(a_match > 0, substr(vector_string, a_match + attr(a_match, "match.length") - 1, a_match + attr(a_match, "match.length") - 1), NA)
  
  return(c(av_value, ac_value, pr_value, ui_value, c_value, i_value, a_value))
}

convert_to_severity <- function(base_score) {
  if (base_score == 0.0) {
    return('NONE')
  } else if (base_score < 4.0) {
    return('LOW')
  } else if (base_score < 7.0) {
    return('MEDIUM')
  } else if (base_score < 9.0) {
    return('HIGH')
  } else {
    return('CRITICAL')
  }
}

file_path <- "Data/cve_data_2014-24.csv"
data <- read_csv(file_path)
data <- data %>%
  mutate(year = str_split(cveId, "-") %>% map_chr(2))
data <- data %>%
  filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")
 
# MSE : 2.76 :(
#features <-c("AV", "AC", "PR", "UI")
# MSE : 0.76
# features <-c("AV", "AC", "C", "I", "A") 
# MSE : 0.85
# features <-c("AV", "C", "I", "A") 
# MSE : 1.1
# features <-c("C", "I", "A") 
# MSE : 0.74
# features <-c("AC", "PR","C", "I", "A") 
# MSE : 0.84
# features <-c("PR","C", "I", "A")
# MSE : 0.52
features <-c("AC", "AV", "PR","C", "I", "A") 

data_2023 <- data[data$year == '2023', ]
vectorStringmatrix <- as.matrix(data_2023$vectorString)
vector_values <- apply(vectorStringmatrix, 1, function(x) unlist(extract_vector_values(x)))
transposed_matrix <- t(vector_values)
colnames(transposed_matrix) <- c("AV", "AC", "PR", "UI", "C", "I", "A")
transposed_matrix <- transposed_matrix[,features]
X <- transposed_matrix
X_precode <- X
Y <- data_2023['baseScore']

mapping <- list(
  AV = c('N' = 4, 'A' = 3, 'L' = 2, 'P' = 1),
  AC = c('L' = 2, 'H' = 1),
  PR = c('N' = 3, 'L' = 2, 'H' = 1),
  UI = c('N' = 2, 'R' = 1),
  I = c('N' = 0, 'L' = 1, 'H' = 2),
  A = c('N' = 0, 'L' = 1, 'H' = 2),
  C = c('N' = 0, 'L' = 1, 'H' = 2)
)
#mapping_severity <- c('LOW','MEDIUM','HIGH','CRITICAL')

for (i in 1:nrow(X)) {
  for (j in colnames(X)) {
    char_value <- X[i,j]
    X[i, j] <- mapping[[j]][[X[i,j]]]
  }
}

set.seed(42)
train_indices <- sample(1:nrow(X), 0.9 * nrow(X))
X_train <- X[train_indices, ]
Y_train <- Y[train_indices,]
X_test <- X[-train_indices, ]
Y_test <- Y[-train_indices,]
Y_test_char <- data_2023[-train_indices,'baseSeverity']

X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)
Y_train <- as.matrix(Y_train)
Y_test <- as.matrix(Y_test)

model <- lm(Y_train ~ ., data = X_train)

y_pred <- predict(model, newdata = X_test)
y_pred_matrix <- as.matrix(y_pred)
y_pred_matrix <- as.matrix(Y_test_char)
# Map continuous predictions to categorical values

# Calculate accuracy
invisible({
# Print results
print("Data before transformation:")
print(X_precode[1:10, ])
print("Data after transformation:")
print(X[1:10, ])

cat("Coefficients:\n")
print(coefficients(model))

mse <- mean((y_pred - Y_test)^2)
cat("MSE: ", mse)


library(ggplot2)

# Combine test data and predictions for plotting
plot_data <- data.frame(Y_test = Y_test, Y_pred = y_pred)

# Assuming X_test has one predictor variable for simplicity in plotting
# If there are multiple predictors, choose one for plotting, e.g., X_test[,1]
ggplot(plot_data, aes(x = Y_test, y = Y_pred)) +
  geom_point() +                      # Scatter plot of actual vs predicted
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line y = x for reference
  labs(title = "Regression Line and Data Points",
       x = "Actual Values (Y_test)",
       y = "Predicted Values (y_pred)") +
  theme_minimal()

residuals <- Y_test - y_pred

# # Plot residuals
# ggplot(plot_data, aes(x = Y_test, y = residuals)) +
#   geom_point() +
#   geom_hline(yintercept = 0, color = "red") +
#   labs(title = "Residual Plot",
#        x = "Actual Values (Y_test)",
#        y = "Residuals") +
#   theme_minimal()

})
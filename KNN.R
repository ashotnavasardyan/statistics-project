library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
#install.packages("FNN")
library(FNN)

perform_knn <- function(train_data, test_data, train_labels, k, distance_metric) {
  model <- kknn::kknn(
    formula = as.factor(train_labels) ~ .,
    train = train_data,
    test = test_data,
    k = k,
    distance = distance_metric
  )
  fitted(model)$fitted.values
}

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

# Testing the extract_vector_values function
#  "CVSS:3.1/AV:L/AC:L/PR:H/UI:N/S:U/C:H/I:N/A:H" %>%
#  extract_vector_values() %>%
#  print()

# Read the data
file_path <- "Data/cve_data_2014-24.csv"
data <- read_csv(file_path)

# Extract year from cveId
data <- data %>%
  mutate(year = str_split(cveId, "-") %>% map_chr(2))

# Filter data for the year 2023
data_2023 <- data[data$year == '2023', ]

features <-c("AV", "AC", "PR", "UI", "C", "I", "A")

vectorStringmatrix <- as.matrix(data_2023$vectorString)
vector_values <- apply(vectorStringmatrix, 1, function(x) unlist(extract_vector_values(x)))
transposed_matrix <- t(vector_values)
colnames(transposed_matrix) <- c("AV", "AC", "PR", "UI", "C", "I", "A")
transposed_matrix <- transposed_matrix[,features]
#vector_values <- transposed_matrix
# Extract target variable (Confidentiality)

#print(transposed_matrix[5, ])
Y <- as.matrix(data_2023[, 'baseSeverity'])
X <- transposed_matrix
# rownames(X) <- gsub("^V", "", rownames(X))
Y <- as.matrix(Y)
X_precode <- X

mapping <- list(
  AV = c('N' = 4, 'A' = 3, 'L' = 2, 'P' = 1),
  AC = c('L' = 2, 'H' = 1),
  PR = c('N' = 3, 'L' = 2, 'H' = 1),
  UI = c('N' = 2, 'R' = 1),
  I = c('N' = 0, 'L' = 1, 'H' = 2),
  A = c('N' = 0, 'L' = 1, 'H' = 2),
  C = c('N' = 0, 'L' = 1, 'H' = 2)
)

# Apply encoding
for (i in 1:nrow(X)) {
  for (j in colnames(X)) {
    char_value <- X[i,j]
    X[i, j] <- mapping[[j]][[X[i,j]]]
  }
}

# for (i in 1:nrow(Y)) {
#     char_value <- Y[i,'C']
#     Y[i, 'C'] <- mapping[[j]][[Y[i,'C']]]
# }

X <- apply(X, 2, as.numeric)
set.seed(124)
train_index <- sample(1:nrow(X), size = 0.8 * nrow(X))
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
Y_train <- Y[train_index]
Y_test <- Y[-train_index]

X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test, center = attr(X_train_scaled, "scaled:center"), scale = attr(X_train_scaled, "scaled:scale"))

k_values <- 1:20

cross_validate_k <- function(k, X_train, Y_train, folds = 5) {
  fold_size <- floor(nrow(X_train) / folds)
  accuracies <- c()
  
  for (i in 1:folds) {
    validation_indices <- ((i - 1) * fold_size + 1):(i * fold_size)
    if (i == folds) {
      validation_indices <- ((i - 1) * fold_size + 1):nrow(X_train)
    }
    
    X_validation <- X_train[validation_indices, ]
    Y_validation <- Y_train[validation_indices]
    
    X_training_fold <- X_train[-validation_indices, ]
    Y_training_fold <- Y_train[-validation_indices]
    
    X_training_fold_scaled <- scale(X_training_fold)
    X_validation_scaled <- scale(X_validation, center = attr(X_training_fold_scaled, "scaled:center"), scale = attr(X_training_fold_scaled, "scaled:scale"))
    
    predicted_Y <- knn(train = X_training_fold_scaled, test = X_validation_scaled, cl = Y_training_fold, k = k)
    
    fold_accuracy <- sum(predicted_Y == Y_validation) / length(Y_validation)
    accuracies <- c(accuracies, fold_accuracy)
  }
  
  # Return average accuracy across folds
  return(mean(accuracies))
}

cv_results <- sapply(k_values, function(k) cross_validate_k(k, X_train_scaled, Y_train))

best_k <- k_values[which.max(cv_results)]
final_predicted_Y <- knn(train = X_train_scaled, test = X_test_scaled, cl = Y_train, k = best_k)

# Evaluate the model using accuracy
final_accuracy <- sum(final_predicted_Y == Y_test) / length(Y_test)

results_df <- data.frame(k = k_values, accuracy = cv_results)

# Plot the accuracy vs. k with colors
ggplot(data = results_df, aes(x = k, y = accuracy, color = accuracy)) +
  geom_line() +
  geom_point() +
  scale_color_gradient(low = "red", high = "green") + 
  labs(title = "Accuracy vs. Number of Neighbors (k)",
       x = "Number of Neighbors (k)",
       y = "Accuracy") +
  theme_minimal()

Y_test_factor <- factor(Y_test, levels = c("CRITICAL", "HIGH", "MEDIUM", "LOW"))
predicted_labels <- factor(final_predicted_Y, levels = c("CRITICAL", "HIGH", "MEDIUM", "LOW"))

confusion_matrix <- table(Predicted = predicted_labels, Actual = Y_test_factor)

install.packages("pheatmap")
library(pheatmap)

confusion_matrix_numeric <- as.matrix(confusion_matrix)

pheatmap(confusion_matrix_numeric, 
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE, 
         number_format = "%.0f", 
         fontsize_number = 12, 
         main = "Confusion Matrix",
         color = colorRampPalette(c("white", "#FFEA38","#BDFF38"))(50))



invisible({
  print(cv_results)
  print(paste("Best k:", best_k))
  print(paste("Final Accuracy:", final_accuracy))
})


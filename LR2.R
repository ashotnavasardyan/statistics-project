library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(caret)
library(randomForest)

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

forward_selection <- function(X_train, Y_train, X_test, Y_test, metric = "mse") {
  n_features <- ncol(X_train)
  selected_features <- c()
  remaining_features <- 1:n_features
  
  while (length(selected_features) < n_features) {
    best_score <- Inf
    best_feature <- NULL
    results_list <- list()

    for (feature in remaining_features) {
      features_to_try <- c(selected_features, feature)
      
      lm_model <- lm(Y_train ~ ., data = X_train[, features_to_try, drop = FALSE])
      
      predictions <- predict(lm_model, newdata = X_test[, features_to_try, drop = FALSE])
      
      if (metric == "mse") {
        error <- mean((predictions - Y_test)^2)
      } else if (metric == "mae") {
        error <- mean(abs(predictions - Y_test))
      } else if (metric == "rmse") {
        error <- sqrt(mean((predictions - Y_test)^2))
      }

      if (error < best_score) {
        best_score <- error
        best_feature <- feature
      }
    }
    
    selected_features <- c(selected_features, best_feature)
    remaining_features <- remaining_features[remaining_features != best_feature]
    
    # cat("Selected feature:", names(X_train)[best_feature], "\n")
    # cat("Current best error (", metric, "):", best_score, "\n")
  }
  
  return(list(features = selected_features, error = best_score))
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

demonstrate_results <- function(model,metric,error, X_test, Y_test){
  y_pred <- predict(model, newdata = X_test)
  
  print("Data before transformation:")
  print(X_precode[1:10, ])
  print("Data after transformation:")
  print(X[1:10, ])
  
    
  cat("Error (", metric, "):", error, "\n")
    
  
  plot_data <- data.frame(Y_test = Y_test, Y_pred = y_pred)
  
  ggplot(plot_data, aes(x = Y_test, y = Y_pred)) +
    geom_point() +                     
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(title = "Regression Line and Data Points",
         x = "Actual Values (Y_test)",
         y = "Predicted Values (y_pred)") +
    theme_minimal()
}


file_path <- "Data/cve_data_2014-24.csv"
data <- read_csv(file_path)
data <- data %>%
  mutate(year = str_split(cveId, "-") %>% map_chr(2))
data <- data %>%
  filter(!is.na(assignerShortName) & !is.na(baseSeverity) & assignerShortName != "" & baseSeverity != "")

features <-c("AC", "AV", "PR","UI","C", "I", "A") 

#data <- data[data$year == '2023', ]
#years <- c("2021", "2022", "2023")
years <- c("2023")
data <- data[data$year %in% years, ]

vectorStringmatrix <- as.matrix(data$vectorString)
vector_values <- apply(vectorStringmatrix, 1, function(x) unlist(extract_vector_values(x)))
transposed_matrix <- t(vector_values)
colnames(transposed_matrix) <- c("AV", "AC", "PR", "UI", "C", "I", "A")
transposed_matrix <- transposed_matrix[,features]
X <- transposed_matrix
X_precode <- X
Y <- data['baseScore']

complete_rows <- complete.cases(X)
X <- X[complete_rows, , drop = FALSE]
Y <- Y[complete_rows, , drop = FALSE]

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

set.seed(50)
train_indices <- sample(1:nrow(X), 0.9 * nrow(X))
X_train <- X[train_indices, ]
Y_train <- Y[train_indices,]
X_test <- X[-train_indices, ]
Y_test <- Y[-train_indices,]
Y_test_char <- data[-train_indices,'baseSeverity']

X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)
Y_train <- as.matrix(Y_train)
Y_test <- as.matrix(Y_test)

suppressMessages({
result_mse <- forward_selection(X_train, Y_train, X_test, Y_test, metric = "mse")
cat("Final selected features (MSE):", names(X_train)[result_mse$features], "\n")
lm_model_mse <- lm(Y_train ~ ., data = X_train[, result_mse$features, drop = FALSE])
demonstrate_results(lm_model_mse,"MSE",result_mse$error, X_test, Y_test)

result_mae <- forward_selection(X_train, Y_train, X_test, Y_test, metric = "mae")
cat("Final selected features (MAE):", names(X_train)[result_mae$features], "\n")
lm_model_mae <- lm(Y_train ~ ., data = X_train[, result_mae$features, drop = FALSE])
demonstrate_results(lm_model_mae,"MAE",result_mae$error, X_test, Y_test)

result_rmse <- forward_selection(X_train, Y_train, X_test, Y_test, metric = "rmse")
cat("Final selected features (RMSE):", names(X_train)[result_rmse$features], "\n")
lm_model_rmse <- lm(Y_train ~ ., data = X_train[, result_rmse$features, drop = FALSE])
demonstrate_results(lm_model_mse, "RMSE",result_rmse$error, X_test, Y_test)
})

#install.packages("caret")
#install.packages("randomForest")

#################################


control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

Y_train <- as.vector(Y_train)
Y_test <- as.vector(Y_test)
rfe_results <- rfe(X_train, Y_train, sizes = c(1:7), rfeControl = control)
selected_features <- predictors(rfe_results)
print(paste("Selected Features:", paste(selected_features, collapse = ", ")))

X_train_selected <- X_train[, selected_features]
X_test_selected <- X_test[, selected_features]

rf_model <- randomForest(X_train_selected, Y_train)

importances <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importances), Importance = importances[, 1])
importance_df <- importance_df[order(-importance_df$Importance), ]

print("Feature Importances:")
print(importance_df)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#5584F9") +
  coord_flip() +
  xlab("Features") +
  ylab("Importance") +
  ggtitle("Feature Importances (Random Forest - Selected Features)") +
  theme_minimal()

# https://towardsdatascience.com/effective-feature-selection-recursive-feature-elimination-using-r-148ff998e4f7

Y_pred <- predict(rf_model, X_test_selected)

results_df <- data.frame(Actual = Y_test, Predicted = Y_pred)

ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  ggtitle("Actual vs Predicted Values (Random Forest)") +
  theme_minimal()

mse <- mean((Y_pred - Y_test)^2)

print(paste("Mean Squared Error (MSE):", mse))

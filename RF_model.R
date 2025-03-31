library(caret)
library(randomForest)
library(recipes)
library(janitor)
library(stringr)
library(dplyr)
library(MLmetrics)  
library(e1071)
library(xgboost)

# THis code file implements a Random Forest Classification algorithm after implementing all the data pre-processing steps


df <- read.csv("C:\\Users\\Sinjini\\Documents\\Semester 2\\Data Mining\\California Wildfires Project\\cleaned_dataset2.csv", header=TRUE, sep=",") 

View(dim(df))
# Making the damage categories binary
df$damage_binary <- ifelse(df$damage == "No Damage", 0, 1)
df$damage_binary <- as.factor(df$damage_binary)
remove_cols <- c('damage')
df = subset(df, select = !(names(df) %in% remove_cols)) 

ggplot(df, aes(x = factor(damage_binary), fill = factor(damage_binary))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "red"), 
                    labels = c("No Damage", "Damaged")) +
  labs(title = "Binary target variable distribution", 
       x = "Damage", y = "Count", fill = "Damage Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Split data into features and target
y <- df$damage_binary
X <- df[, setdiff(names(df), "damage_binary")]

X <- na.omit(X)

# Since there are too many categories in street name, we compute the name frequency and perform the encoding by frequency
street_freq <- X %>%
  count(street_name, name = "street_freq")

X <- X %>%
  left_join(street_freq, by = "street_name") %>%
  select(-street_name)  # Drop original column



# Identify numerical and categorical columns
numerical_col <- names(X)[sapply(X, is.numeric)]
categorical_col <- names(X)[sapply(X, is.character) | sapply(X, is.factor)]


duplicated_levels <- X %>%
  select(all_of(categorical_col)) %>%
  summarise_all(~ length(unique(.)))

print(duplicated_levels)


X <- X %>%
  mutate(across(all_of(categorical_col), ~ str_replace_all(., "[^A-Za-z0-9]", "_")))




# Split data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.67, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]




# Preprocessing with "recipe"
recipe_obj <- recipe(~ ., data = X_train) %>%
  step_normalize(all_of(numerical_col)) %>%  
  step_dummy(all_of(categorical_col), one_hot = TRUE)  

prepped_recipe <- prep(recipe_obj, training = X_train)
X_train <- bake(prepped_recipe, new_data = X_train)%>% clean_names()

X_test <- bake(prepped_recipe, new_data = X_test)%>% clean_names()


# Replace missing numerical values with mean

X_test <- X_test %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Replace missing categorical values mode
fill_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

X_test <- X_test %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), fill_mode(.), .)))



set.seed(42)

# Train random forest model
rfc <- randomForest(x = X_train, y = y_train, ntree = 100)  




# Prediction
y_pred <- predict(rfc, X_test)

conf_matrix_rf <- table(Predicted = y_pred, Actual = y_test)
print(conf_matrix_svm)

# Calculate Accuracy anf F1 score
accuracy_rf <- sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
cat("Random Forest Accuracy:", accuracy_rf, "\n")
cat("Random Forest F1 Score: ", F1_Score(y_pred,y_test))


# Extract feature importance
feature_importance <- importance(rfc, type = 2)  
feature_importance <- as.data.frame(feature_importance)
feature_importance$Feature <- rownames(feature_importance)
top_10 <- feature_importance %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(10)

ggplot(top_10, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Important Features",
       x = "Features",
       y = "Importance Score") +
  theme_minimal()


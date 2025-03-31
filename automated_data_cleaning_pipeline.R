# This is an automated data cleaning pipeline to 

library(tidyverse)
library(data.table)


clean_dataset <- function(file_path) {
  df <-  read.csv(file_path)
  
  cat("\nDataset Information:\n")
  print(str(df))
  
  # visualising missing values withh a heatmap
  missing_vals <- as.data.frame(colSums(is.na(df)))
  colnames(missing_vals) <- "MissingCount"
  missing_vals <- missing_vals %>% rownames_to_column("Column")
  
  ggplot(missing_vals, aes(x = reorder(Column, -MissingCount), y = MissingCount)) +
    geom_bar(stat = "identity", fill = "purple") +
    theme_minimal() +
    labs(title = "Missing Values Per Column", x = "Columns", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  num_cols <- df %>% select(where(is.numeric)) %>% colnames()
  
  # Check for invalid values
  cat("\nChecking for invalid values in numerical columns:\n")
  for (col in num_cols) {
    invalid_count <- sum(is.na(df[[col]])) + sum(is.infinite(df[[col]]))
    cat(paste0("Column '", col, "': ", invalid_count, " invalid values\n"))
  }

  df[num_cols] <- df[num_cols] %>%
    mutate(across(everything(), ~ replace(., is.infinite(.), NA))) %>%  
    mutate(across(everything(), ~ replace_na(., 0))) %>%  
    mutate(across(everything(), ~ pmin(pmax(., -1e6), 1e6)))
  
  
  cat("\nBasic Statistics After Cleaning:\n")
  print(summary(df))
  
  cat("\nMissing Values:\n")
  print(colSums(is.na(df)))
  
  # Drop columns with more than half og the values missing
  df_cleaned <- df %>% select(where(~ mean(is.na(.)) < 0.5))
  
  # Drop numerical columns that contain more thwn 90% zeros.
  threshold <- 0.9
  df_cleaned <- df_cleaned %>%
    select(where(~ !is.numeric(.) || mean(. == 0, na.rm = TRUE) <= threshold))
  

  df_cleaned <- df_cleaned %>%
    fill(everything(), .direction = "downup")  
  
  missing_vals_cleaned <- as.data.frame(colSums(is.na(df_cleaned)))
  colnames(missing_vals_cleaned) <- "MissingCount"
  missing_vals_cleaned <- missing_vals_cleaned %>% rownames_to_column("Column")
  
  ggplot(missing_vals_cleaned, aes(x = reorder(Column, -MissingCount), y = MissingCount)) +
    geom_bar(stat = "identity", fill = "red") +
    theme_minimal() +
    labs(title = "Missing Values After Cleaning", x = "Columns", y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the cleaned dataset in a new csv file
  cleaned_file_path <- "cleaned_dataset2.csv"
  fwrite(df_cleaned, cleaned_file_path)
  cat("\nCleaned dataset saved to:", cleaned_file_path, "\n")
  
  return(df_cleaned)
}

cleaned_data <- clean_dataset("cleaned_dataset.csv")


df <- read.csv("cleaned_dataset2.csv", header=TRUE, sep=",") 
View(df)
dim(df)



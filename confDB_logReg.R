#--------------------------------------------------------------------------
# Analysis from "The Confidence Database" paper by Rahnev, Desender, Lee, et al.
# 
# This analysis explores changes in meta-d' (as a measure of metacognitive efficiency) relative to trial number 
#
# To run this analysis, all the files of the Confidence Database should be placed in a folder called 'Confidence Database' located in your current WD
#
# Written by Trevor Caruso. Last update: April 21, 2024.
#--------------------------------------------------------------------------


library(data.table)
library(dplyr)
library(ggplot2)

options(scipen = 999)


################ Preprocess data (FIrst 100 trials) ##########################

# Set the working directory
setwd("C:/Users/Trevor/Desktop/BioAI/confDB")

# List all CSV files
file_names <- list.files(pattern = "*.csv", full.names = TRUE)

# Define a function to read, standardize, and prepare each dataset
read_and_prepare <- function(file_path) {
  # Read the file safely
  df <- tryCatch({
    read.csv(file_path)
  }, error = function(e) {
    warning(paste("Failed to read", file_path, ":", e$message))
    return(NULL)
  })

  # Proceed if the file was read successfully
  if (!is.null(df)) {
    # Standardize column names if necessary
    if ("Response" %in% names(df)) names(df)[names(df) == "Response"] <- "StimulusResponse"
    if ("Rating" %in% names(df)) names(df)[names(df) == "Rating"] <- "Confidence"
    if ("Participant" %in% names(df)) names(df)[names(df) == "Participant"] <- "Subj_idx"

    # Check for essential columns
    necessary_columns <- c("Subj_idx", "Stimulus", "StimulusResponse", "Confidence")
    if (!all(necessary_columns %in% names(df))) {
      warning(paste("Missing columns in", file_path, "Needed columns: Subj_idx, Stimulus, StimulusResponse, Confidence"))
      return(NULL)
    }

    # Select only the necessary columns and add the study column
    df <- df[, necessary_columns, drop = FALSE]
    study_name <- tools::file_path_sans_ext(basename(file_path))  # Removing file extension
    df$Study <- study_name
    df$ParticipantID <- paste(df$Subj_idx, study_name, sep = "_")  # Ensuring unique identifiers across studies
    df$Subj_idx <- paste(df$Subj_idx, study_name, sep = "_")

    # Filter to the first 100 trials per participant
    df <- df %>%
          group_by(ParticipantID) %>%
          slice_head(n = 100) %>%
          ungroup()

    # Data transformations
    df$isCorrect <- df$Stimulus == df$StimulusResponse
    df$Confidence <- ifelse(df$isCorrect, abs(df$Confidence), -abs(df$Confidence))

    # Add quartile information
    df <- df %>%
          group_by(ParticipantID) %>%
          mutate(Trial = row_number(), Quartile = ntile(Trial, 4)) %>%
          ungroup()

    # Remove rows containing NaN values
    df <- na.omit(df)

    return(df)
  } else {
    return(NULL)
  }
}

# Process each file and remove NULL entries
all_data <- lapply(file_names, read_and_prepare)
all_data <- all_data[!sapply(all_data, is.null)]

# Combine all data frames into one
exp_df <- do.call(rbind, all_data)

# Check the combined data frame structure
str(exp_df)

sampled_rows <- sample_n(exp_df, 10)  # Randomly sample 10 rows
print(sampled_rows)



##################################################################################################################################################
# 
# ██       ██████   ██████  ██ ███████ ████████ ██  ██████     ██████  ███████  ██████  ██████  ███████ ███████ ███████ ██  ██████  ██ ███    ██ 
# ██      ██    ██ ██       ██ ██         ██    ██ ██          ██   ██ ██      ██       ██   ██ ██      ██      ██      ██ ██    ██ ██ ████   ██ 
# ██      ██    ██ ██   ███ ██ ███████    ██    ██ ██          ██████  █████   ██   ███ ██████  █████   ███████ ███████ ██ ██    ██ ██ ██ ██  ██ 
# ██      ██    ██ ██    ██ ██      ██    ██    ██ ██          ██   ██ ██      ██    ██ ██   ██ ██           ██      ██ ██ ██    ██ ██ ██  ██ ██ 
# ███████  ██████   ██████  ██ ███████    ██    ██  ██████     ██   ██ ███████  ██████  ██   ██ ███████ ███████ ███████ ██  ██████  ██ ██   ████ 
#                                                                                                                                                
##################################################################################################################################################

# Further preprocessing 
exp_df2 <- exp_df

# Create a binary correctness indicator (1 for correct, -1 for incorrect)
exp_df2 <- exp_df2 %>%
  mutate(Accuracy = ifelse(Confidence < 0, 0, 1))  # 0 for incorrect, 1 for correct

# Convert all confidence ratings to positive values and prepare exp_df2
exp_df2 <- exp_df2 %>%
  mutate(Confidence = abs(Confidence))

# Ensure 'Trial' and 'Confidence' are treated as numeric
exp_df2$Trial <- as.numeric(exp_df2$Trial)
exp_df2$Confidence <- as.numeric(exp_df2$Confidence)

# Filter out zero confidence values
exp_df2 <- exp_df2 %>% 
  filter(Confidence > 0)

#~~~~~~~~~~~~~~~~

# Set seed for reproducibility
set.seed(123)

# Create training and testing datasets
train_index <- sample(1:nrow(exp_df2), 0.8 * nrow(exp_df2))  # 80% for training
train_data <- exp_df2[train_index, ]
test_data <- exp_df2[-train_index, ]

#~~~~~~~~~~~~~~~~

# Fit the logistic regression model with an interaction term between Confidence and Trial
model <- glm(Accuracy ~ Confidence + Trial + Confidence * Trial, data = train_data, family = binomial())

# Summarize the model
summary(model)

#~~~~~~~~~~~~~~~~

# Check output summary for insights on the coefficients (interaction term)
# Evaluate model performance using the test dataset.

# Make predictions on the testing set
predictions <- predict(model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calculate the confusion matrix to evaluate model
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$Accuracy)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

#~~~~~~~~~~~~~~~~

# Visualize the interaction effect to help understand how Confidence and Trial impact Accuracy.



### test
# Generate a new data frame for prediction that spans the range of Confidence and Trial
pred_data <- with(exp_df2, expand.grid(Confidence = seq(min(Confidence), max(Confidence), length.out = 100),
                                       Trial = seq(min(Trial), max(Trial), length.out = 100)))

# Predict probabilities using the fitted model
pred_data$PredictedProbability <- predict(model, newdata = pred_data, type = "response")

# Plot the predicted probabilities
library(ggplot2)
ggplot(pred_data, aes(x = Trial, y = PredictedProbability, color = Confidence)) +
  geom_line() +
  labs(title = "Predicted Probability of Accuracy Across Trials",
       x = "Trial Number",
       y = "Predicted Probability of Being Correct",
       color = "Confidence Level") +
  theme_minimal()



# Predict the probabilities for the entire dataset
exp_df2$PredictedProbability <- predict(model, newdata = exp_df2, type = "response")

# Group by Trial and Confidence, then calculate means
grouped_means <- exp_df2 %>%
  group_by(Trial, Confidence) %>%
  summarise(MeanAccuracy = mean(PredictedProbability, na.rm = TRUE))

# Plot the mean predicted probabilities for each level of Confidence across Trial numbers
ggplot(grouped_means, aes(x = Trial, y = MeanAccuracy, group = Confidence, color = as.factor(Confidence))) +
  geom_line(size = 1.5) +  # Makes lines thicker
  labs(title = "Mean Predicted Probability of Accuracy by Confidence Level Across Trials",
       x = "Trial Number",
       y = "Mean Predicted Probability of Accuracy",
       color = "Confidence Level") +
  theme_minimal()



ggplot(grouped_means, aes(x = Trial, y = MeanAccuracy, group = Confidence, color = as.factor(Confidence))) +
  geom_line(size = 1.5) +
  geom_text(aes(label = Confidence), hjust = 1.1, vjust = 1.1) +
  labs(title = "Mean Predicted Probability of Accuracy by Confidence Level Across Trials",
       x = "Trial Number",
       y = "Mean Predicted Probability of Accuracy",
       color = "Confidence Level") +
  theme_minimal()







### end test

# Generate data for visualization
vis_data <- expand.grid(Confidence = seq(min(exp_df2$Confidence), max(exp_df2$Confidence), length.out = 100),
                        Trial = seq(min(exp_df2$Trial), max(exp_df2$Trial), length.out = 100))

# Predict probabilities using the model
vis_data$Accuracy <- predict(model, newdata = vis_data, type = "response")

# Plot
ggplot(vis_data, aes(x = Trial, y = Accuracy, color = as.factor(Confidence))) +
  geom_line() +
  labs(title = "Predicted Probability of Accuracy Across Trials (100)",
       x = "Trial Number",
       y = "Probability of Accuracy",
       color = "Confidence Level") +
  theme_minimal()


# Generate predictions across the range of Confidence and Trial
ggplot(test_data, aes(x = Trial, color = as.factor(Confidence))) +
  geom_jitter(aes(y = predictions), alpha = 0.5) +
  geom_smooth(aes(y = predictions), method = "loess") +
  labs(title = "Predicted Probability of Accuracy Across Trials by Confidence",
       x = "Trial Number",
       y = "Predicted Probability of Accuracy") +
  scale_color_viridis_d(begin = 0.3, end = 0.9, name = "Confidence Level") +
  theme_minimal()


# Change in Predicted Probability of Accuracy Over Trials
# Group data by Trial and calculate average predicted probability
avg_pred_prob_by_trial <- pred_data %>%
  group_by(Trial) %>%
  summarise(Avg_Predicted_Probability = mean(Predicted_Probability))

# Plotting
ggplot(avg_pred_prob_by_trial, aes(x = Trial, y = Avg_Predicted_Probability)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Predicted Probability of Accuracy Over Trials",
       x = "Trial Number",
       y = "Average Predicted Probability") +
  theme_minimal()


#  Distribution of Confidence and Accuracy
ggplot(exp_df2, aes(x = Confidence, fill = as.factor(isCorrect))) +
  geom_histogram(position = "dodge", binwidth = 0.5) +
  scale_fill_manual(values = c("red", "green"), labels = c("Incorrect", "Correct")) +
  labs(title = "Distribution of Confidence by Accuracy",
       x = "Confidence Level",
       y = "Count",
       fill = "Accuracy") +
  theme_minimal()




##################################################################################################################################################
# end logistic regression
##################################################################################################################################################






# SMOTE (Synthetic Minority Over-sampling Technique): Generate synthetic samples for the minority class instead of simple replication.

# Check the balance of Accuracy classes
table(exp_df2$Accuracy)


# Install and load ROSE package
if (!require(ROSE)) install.packages("ROSE", dependencies=TRUE)
library(ROSE)

# Use ROSE to generate balanced data
rose_data <- ovun.sample(Accuracy ~ ., data = train_data, method = "over", N = 200000)$data  # Adjust N based on your needs

# Fit logistic regression model on the balanced dataset
rose_model <- glm(Accuracy ~ Confidence + Trial + Confidence * Trial, data = rose_data, family = binomial())
summary(rose_model)

# Evaluate the model on the unchanged test set
rose_predictions <- predict(rose_model, test_data, type = "response")
rose_predicted_classes <- ifelse(rose_predictions > 0.5, 1, 0)
rose_confusion_matrix <- table(Predicted = rose_predicted_classes, Actual = test_data$Accuracy)
print(rose_confusion_matrix)
rose_accuracy <- sum(diag(rose_confusion_matrix)) / sum(rose_confusion_matrix)
print(paste("ROSE Adjusted Accuracy:", rose_accuracy))



## Use k-fold cross-validation to validate the results more robustly, ensuring that the model performs consistently across different subsets
# Convert 'Accuracy' to a factor with meaningful level names
exp_df2$Accuracy <- factor(exp_df2$Accuracy, levels = c(0, 1), labels = c("Incorrect", "Correct"))


# Install caret if not already installed
if (!require(caret)) install.packages("caret", dependencies = TRUE)
library(caret)

# Set up cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(exp_df2$Accuracy, k = 5)  # Create 5 folds
# Update trainControl to save predictions and calculate classification metrics
cv_control <- trainControl(
  method = "cv",
  index = folds,
  savePredictions = "final",
  classProbs = TRUE,  # If probability output is also needed
  summaryFunction = twoClassSummary  # For binary classification
)

# Re-train the logistic regression model with correct settings
model_cv <- train(
  Accuracy ~ Confidence + Trial + Confidence * Trial,
  data = exp_df2,
  method = "glm",
  family = binomial(),  # Ensure it's set for logistic regression
  metric = "Accuracy",
  maximize = TRUE,  # Specify to maximize the accuracy
  trControl = cv_control
)


# Print model summary
print(model_cv)


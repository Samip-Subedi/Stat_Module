#------------------------------------
# Install and Load Required Packages
#------------------------------------



library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

#------------------------------------
# Set Working Directory
#------------------------------------

getwd()

setwd("C:/Users/samip/Desktop/statmodule")

#------------------------------------
# Read Dataset
#------------------------------------

time <- read.csv(
  "time.csv",
  header = FALSE
)

X <- read.csv(
  "X.csv",
  header = FALSE
)

y <- read.csv(
  "y.csv",
  header = FALSE
)

#------------------------------------
# Rename Columns
#------------------------------------

colnames(time) <- "Time"

colnames(X) <- c(
  "X1",
  "X2",
  "X3",
  "X4"
)

colnames(y) <- "Y"

#------------------------------------
# View Sample Data
#------------------------------------

head(time, 10)

head(X, 10)

head(y, 10)

#------------------------------------
# Check Dataset
#------------------------------------

dim(time)
dim(X)
dim(y)

str(time)
str(X)
str(y)

sum(is.na(time))
sum(is.na(X))
sum(is.na(y))

summary(time)
summary(X)
summary(y)

#------------------------------------
# Combine Dataset
#------------------------------------

EEG_Data <- cbind(
  time,
  X,
  y
)

# View Combined Dataset

head(EEG_Data, 10)

View(EEG_Data)

#------------------------------------
# Time Series Analysis
#------------------------------------

# Combine Time with Input Signals
data_x <- cbind(
  time,
  X
)

# View Sample Data
head(data_x, 10)

View(data_x)

# Convert Input Signals to Long Format
data_long <- pivot_longer(
  data_x,
  cols = c(X1, X2, X3, X4),
  names_to = "Signal",
  values_to = "Amplitude"
)

#------------------------------------
# Time Series Plot of Input EEG Signals
#------------------------------------

ggplot(
  data_long,
  aes(
    x = Time,
    y = Amplitude,
    colour = Signal
  )
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Time Series Plot of Input EEG Signals",
    x = "Time (Seconds)",
    y = "Amplitude",
    colour = "Signal"
  ) +
  theme_minimal()

#------------------------------------
# Time Series Plot of Individual Input EEG Signals
#------------------------------------

ggplot(
  data_long,
  aes(
    x = Time,
    y = Amplitude
  )
) +
  geom_line(color = "steelblue") +
  facet_wrap(
    ~Signal,
    ncol = 2,
    scales = "free_y"
  ) +
  labs(
    title = "Time Series Plot of Individual Input EEG Signals",
    x = "Time (Seconds)",
    y = "Amplitude"
  ) +
  theme_minimal()

#------------------------------------
# Output EEG Signal
#------------------------------------

data_y <- cbind(
  time,
  y
)

head(data_y, 10)

View(data_y)

#------------------------------------
# Time Series Plot of Output EEG Signal
#------------------------------------

ggplot(
  data_y,
  aes(
    x = Time,
    y = Y
  )
) +
  geom_line(
    color = "red",
    linewidth = 0.8
  ) +
  labs(
    title = "Time Series Plot of Output EEG Signal",
    x = "Time (Seconds)",
    y = "Amplitude"
  ) +
  theme_minimal()

#------------------------------------
# Combined Histogram of Input EEG Signals
#------------------------------------

library(ggplot2)
library(RColorBrewer)

ggplot(
  data_long,
  aes(
    x = Amplitude,
    fill = Signal
  )
) +
  geom_histogram(
    bins = 30,
    position = "identity",
    alpha = 0.5,
    color = "black"
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  labs(
    title = "Combined Histogram of Input EEG Signals",
    x = "Amplitude",
    y = "Frequency",
    fill = "Input Signal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

#------------------------------------
# Distribution Analysis
#------------------------------------

# Histogram of Input EEG Signals

ggplot(
  data_long,
  aes(x = Amplitude)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "black"
  ) +
  facet_wrap(
    ~Signal,
    ncol = 2,
    scales = "free"
  ) +
  labs(
    title = "Distribution of Input EEG Signals",
    x = "Amplitude",
    y = "Frequency"
  ) +
  theme_minimal()

#------------------------------------
# Histogram of Output EEG Signal
#------------------------------------

ggplot(
  data_y,
  aes(x = Y)
) +
  geom_histogram(
    bins = 30,
    fill = "tomato",
    color = "black"
  ) +
  labs(
    title = "Distribution of Output EEG Signal",
    x = "Amplitude",
    y = "Frequency"
  ) +
  theme_minimal()

#------------------------------------
# Correlation Matrix
#------------------------------------
library(corrplot)
cor_data <- cbind(X, Y = y$Y)

cor_matrix <- cor(cor_data)

round(cor_matrix, 3)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  diag = FALSE
)

#------------------------------------
# Scatter Plot Matrix
#------------------------------------

pairs(
  cor_data,
  main = "Scatter Plot Matrix of EEG Variables",
  pch = 19,
  col = "steelblue"
)

#------------------------------------
# Correlation of All X Inputs with Y
#------------------------------------
install.packages("tidyr")   # Only run once if not installed
library(tidyr)
scatter_data <- data.frame(
  X1 = X$X1,
  X2 = X$X2,
  X3 = X$X3,
  X4 = X$X4,
  Y  = y$Y
)

scatter_long <- pivot_longer(
  scatter_data,
  cols = c(X1, X2, X3, X4),
  names_to = "Signal",
  values_to = "Input"
)


ggplot(
  scatter_long,
  aes(
    x = Input,
    y = Y,
    colour = Signal
  )
) +
  geom_point(
    size = 2,
    alpha = 0.7
  ) +
  labs(
    title = "Correlation of X Inputs with Y Output",
    x = "Input Signal",
    y = "Output Signal (Y)",
    colour = "Signal"
  ) +
  theme_minimal()

#------------------------------------
# Individual Correlation Plots
#------------------------------------

ggplot(
  scatter_long,
  aes(
    x = Input,
    y = Y
  )
) +
  geom_point(
    color = "steelblue",
    size = 1.8,
    alpha = 0.7
  ) +
  facet_wrap(
    ~Signal,
    ncol = 2,
    scales = "free_x"
  ) +
  labs(
    title = "Correlation of Individual X Inputs with Y Output",
    x = "Input Signal",
    y = "Output Signal (Y)"
  ) +
  theme_minimal()


# Combine all variables into one dataset
EEG_Model <- data.frame(
  X1 = X$X1,
  X2 = X$X2,
  X3 = X$X3,
  X4 = X$X4,
  Y = y$Y
)

head(EEG_Model)

# Model 1
model1 <- lm(
  Y ~ X4 +
    I(X1^2) +
    I(X1^3) +
    I(X2^4),
  data = EEG_Model
)

summary(model1)

# Model 2
model2 <- lm(
  Y ~ X4 +
    I(X1^3) +
    I(X3^4),
  data = EEG_Model
)

summary(model2)

# Model 3
model3 <- lm(
  Y ~ I(X3^3) +
    I(X2^4),
  data = EEG_Model
)

summary(model3)

# Model 4
model4 <- lm(
  Y ~ X2 +
    I(X1^3) +
    I(X3^4),
  data = EEG_Model
)

summary(model4)

# Model 5
model5 <- lm(
  Y ~ X4 +
    I(X1^2) +
    I(X1^3) +
    I(X3^4),
  data = EEG_Model
)

summary(model5)

# Calculate RSS for all models

rss1 <- sum(residuals(model1)^2)
rss2 <- sum(residuals(model2)^2)
rss3 <- sum(residuals(model3)^2)
rss4 <- sum(residuals(model4)^2)
rss5 <- sum(residuals(model5)^2)

rss_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  RSS = c(rss1, rss2, rss3, rss4, rss5)
)

rss_table

# Calculate Log-Likelihood

loglik_table <- data.frame(
  Model = c("Model 1","Model 2","Model 3","Model 4","Model 5"),
  LogLikelihood = c(
    as.numeric(logLik(model1)),
    as.numeric(logLik(model2)),
    as.numeric(logLik(model3)),
    as.numeric(logLik(model4)),
    as.numeric(logLik(model5))
  )
)

loglik_table

# Create AIC and BIC comparison table

aic_bic_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  AIC = c(
    AIC(model1),
    AIC(model2),
    AIC(model3),
    AIC(model4),
    AIC(model5)
  ),
  BIC = c(
    BIC(model1),
    BIC(model2),
    BIC(model3),
    BIC(model4),
    BIC(model5)
  )
)

aic_bic_table


#==========================================================
# Residual Analysis - Model 1
#==========================================================

residuals_model1 <- residuals(model1)

par(mfrow = c(1,2))

hist(
  residuals_model1,
  breaks = 20,
  col = "skyblue",
  border = "black",
  main = "Histogram (Model 1)",
  xlab = "Residuals"
)

qqnorm(
  residuals_model1,
  pch = 19,
  col = "darkblue",
  main = "Normal Q-Q Plot (Model 1)"
)
qqline(
  residuals_model1,
  col = "red",
  lwd = 2
)

par(mfrow = c(1,1))

#==========================================================
# Residual Analysis - Model 2
#==========================================================

residuals_model2 <- residuals(model2)

par(mfrow = c(1,2))

hist(
  residuals_model2,
  breaks = 20,
  col = "lightgreen",
  border = "black",
  main = "Histogram (Model 2)",
  xlab = "Residuals"
)

qqnorm(
  residuals_model2,
  pch = 19,
  col = "darkgreen",
  main = "Normal Q-Q Plot (Model 2)"
)
qqline(
  residuals_model2,
  col = "red",
  lwd = 2
)

par(mfrow = c(1,1))

#==========================================================
# Residual Analysis - Model 3
#==========================================================

residuals_model3 <- residuals(model3)

par(mfrow = c(1,2))

hist(
  residuals_model3,
  breaks = 20,
  col = "gold",
  border = "black",
  main = "Histogram (Model 3)",
  xlab = "Residuals"
)

qqnorm(
  residuals_model3,
  pch = 19,
  col = "orange",
  main = "Normal Q-Q Plot (Model 3)"
)
qqline(
  residuals_model3,
  col = "red",
  lwd = 2
)

par(mfrow = c(1,1))

#==========================================================
# Residual Analysis - Model 4
#==========================================================

residuals_model4 <- residuals(model4)

par(mfrow = c(1,2))

hist(
  residuals_model4,
  breaks = 20,
  col = "plum",
  border = "black",
  main = "Histogram (Model 4)",
  xlab = "Residuals"
)

qqnorm(
  residuals_model4,
  pch = 19,
  col = "purple4",
  main = "Normal Q-Q Plot (Model 4)"
)
qqline(
  residuals_model4,
  col = "red",
  lwd = 2
)

par(mfrow = c(1,1))

#==========================================================
# Residual Analysis - Model 5
#==========================================================

residuals_model5 <- residuals(model5)

par(mfrow = c(1,2))

hist(
  residuals_model5,
  breaks = 20,
  col = "tomato",
  border = "black",
  main = "Histogram (Model 5)",
  xlab = "Residuals"
)

qqnorm(
  residuals_model5,
  pch = 19,
  col = "brown",
  main = "Normal Q-Q Plot (Model 5)"
)
qqline(
  residuals_model5,
  col = "blue",
  lwd = 2
)

par(mfrow = c(1,1))

#------------------------------------
# Task 2.6: Model Selection
#------------------------------------

# Create Model Comparison Table

model_selection <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  RSS = c(
    sum(residuals(model1)^2),
    sum(residuals(model2)^2),
    sum(residuals(model3)^2),
    sum(residuals(model4)^2),
    sum(residuals(model5)^2)
  ),
  LogLikelihood = c(
    as.numeric(logLik(model1)),
    as.numeric(logLik(model2)),
    as.numeric(logLik(model3)),
    as.numeric(logLik(model4)),
    as.numeric(logLik(model5))
  ),
  AIC = c(
    AIC(model1),
    AIC(model2),
    AIC(model3),
    AIC(model4),
    AIC(model5)
  ),
  BIC = c(
    BIC(model1),
    BIC(model2),
    BIC(model3),
    BIC(model4),
    BIC(model5)
  )
)

# Round values for presentation
model_selection[,2:5] <- round(model_selection[,2:5], 3)

# Display the comparison table
model_selection

#------------------------------------
# Select Best Model
#------------------------------------

best_model <- model_selection[
  which.min(model_selection$AIC),
]

best_model


#==========================================================
# Task 2.7: Model Evaluation
# Selected Model: Model 2
#==========================================================

# Predicted Values
predicted <- predict(model2)

# Actual Values
actual <- EEG_Model$Y

#----------------------------------------------------------
# Performance Measures
#----------------------------------------------------------

RMSE <- sqrt(mean((actual - predicted)^2))

MAE <- mean(abs(actual - predicted))

R2 <- summary(model2)$r.squared

Adj_R2 <- summary(model2)$adj.r.squared

evaluation_table <- data.frame(
  Measure = c(
    "RMSE",
    "MAE",
    "R-Squared",
    "Adjusted R-Squared"
  ),
  Value = round(
    c(
      RMSE,
      MAE,
      R2,
      Adj_R2
    ),
    4
  )
)

evaluation_table

#----------------------------------------------------------
# Actual vs Predicted Plot
#----------------------------------------------------------

plot(
  actual,
  predicted,
  pch = 19,
  col = "steelblue",
  main = "Actual vs Predicted Values",
  xlab = "Actual Output",
  ylab = "Predicted Output"
)

abline(
  0,
  1,
  col = "red",
  lwd = 2
)

#----------------------------------------------------------
# Residuals vs Fitted Plot
#----------------------------------------------------------

plot(
  fitted(model2),
  residuals(model2),
  pch = 19,
  col = "darkgreen",
  main = "Residuals vs Fitted Values",
  xlab = "Fitted Values",
  ylab = "Residuals"
)

abline(
  h = 0,
  col = "red",
  lwd = 2,
  lty = 2)


#==========================================================
# Task 2.7
# Step 1: Split Dataset into Training and Testing Sets
#==========================================================
if (nrow(EEG_Model) == 201) {
  EEG_Model <- EEG_Model[-1, ]
}

# Set seed for reproducibility
set.seed(123)

# Total number of observations
n <- nrow(EEG_Model)

# Select 70% of observations for training
train_index <- sample(
  1:n,
  size = round(0.70 * n)
)

# Create training dataset
train_data <- EEG_Model[train_index, ]

# Create testing dataset
test_data <- EEG_Model[-train_index, ]


#------------------------------------
# Function to Display Dataset Size
#------------------------------------

display_observations <- function(train_data, test_data) {
  
  cat(
    "Training set =",
    nrow(train_data),
    "observations\n"
  )
  
  cat(
    "Testing set =",
    nrow(test_data),
    "observations\n"
  )
  
}

# Call the function
display_observations(train_data, test_data)


#==========================================================
# Task 2.7
# Step 2: Train the Selected Model (Model 2)
#==========================================================

# Fit Model 2 using the training dataset
model2_train <- lm(
  Y ~ X4 +
    I(X1^3) +
    I(X3^4),
  data = train_data
)

# Display model summary
summary(model2_train)


#==========================================================
# Task 2.7
# Step 3: Predict on Testing Dataset
#==========================================================

# Predict on testing data with 95% prediction intervals

prediction_results <- predict(
  model2_train,
  newdata = test_data,
  interval = "prediction",
  level = 0.95
)

# Display first few predictions
head(prediction_results)


#==========================================================
# Task 2.7
# Step 4: Plot Predictions with 95% Prediction Intervals
#==========================================================

plot(
  test_data$Y,
  prediction_results[, "fit"],
  pch = 19,
  col = "blue",
  xlab = "Observed Output",
  ylab = "Predicted Output",
  main = "Observed vs Predicted Output (95% Prediction Intervals)"
)

# Identity line
abline(
  0,
  1,
  col = "red",
  lwd = 2
)

# Add 95% prediction interval error bars
arrows(
  x0 = test_data$Y,
  y0 = prediction_results[, "lwr"],
  x1 = test_data$Y,
  y1 = prediction_results[, "upr"],
  angle = 90,
  code = 3,
  length = 0.05,
  col = "darkgreen"
)


#==========================================================
# Task 2.7
# Step 5: Prediction Accuracy on Testing Dataset
#==========================================================

# Actual values
actual <- test_data$Y

# Predicted values
predicted <- prediction_results[, "fit"]

# Calculate evaluation metrics
RMSE <- sqrt(mean((actual - predicted)^2))
MAE <- mean(abs(actual - predicted))
MSE <- mean((actual - predicted)^2)

# R-squared
R2 <- cor(actual, predicted)^2

# Display results
evaluation <- data.frame(
  Measure = c(
    "RMSE",
    "MSE",
    "MAE",
    "R-Squared"
  ),
  Value = round(
    c(
      RMSE,
      MSE,
      MAE,
      R2
    ),
    4
  )
)

evaluation


#==========================================================
# Final Prediction Plot with 95% Prediction Intervals
#==========================================================

prediction_df <- data.frame(
  Observation = 1:nrow(test_data),
  Actual = test_data$Y,
  Predicted = prediction_results[, "fit"],
  Lower = prediction_results[, "lwr"],
  Upper = prediction_results[, "upr"]
)

plot(
  prediction_df$Observation,
  prediction_df$Predicted,
  type = "b",
  pch = 19,
  col = "blue",
  ylim = range(
    prediction_df$Lower,
    prediction_df$Upper
  ),
  xlab = "Testing Observation",
  ylab = "EEG Output",
  main = "Prediction with 95% Prediction Intervals"
)

points(
  prediction_df$Observation,
  prediction_df$Actual,
  pch = 17,
  col = "red"
)

arrows(
  prediction_df$Observation,
  prediction_df$Lower,
  prediction_df$Observation,
  prediction_df$Upper,
  angle = 90,
  code = 3,
  length = 0.05,
  col = "darkgreen"
)

legend(
  "topright",
  legend = c(
    "Predicted",
    "Actual",
    "95% Prediction Interval"
  ),
  col = c(
    "blue",
    "red",
    "darkgreen"
  ),
  pch = c(
    19,
    17,
    NA
  ),
  lty = c(
    1,
    NA,
    1
  ),
  lwd = c(
    1,
    NA,
    2
  )
)


#====================================================
# Task 3
# Step 1 : Extract Estimated Parameters
#====================================================

coef(model2)

summary(model2)$coefficients

#====================================================
# Task 3
# Step 2 : Define Uniform Prior Distributions
#====================================================

# Estimated coefficients from Model 2
beta0_hat <- coef(model2)[1]
beta1_hat <- coef(model2)[2]

# Uniform prior limits (±50%)
beta0_lower <- beta0_hat * 0.5
beta0_upper <- beta0_hat * 1.5

beta1_lower <- beta1_hat * 0.5
beta1_upper <- beta1_hat * 1.5

# Display prior ranges
prior_table <- data.frame(
  Parameter = c("Intercept", "X4"),
  Lower = c(beta0_lower, beta1_lower),
  Upper = c(beta0_upper, beta1_upper)
)

prior_table

#====================================================
# Task 3
# Step 3 : Sample from Uniform Priors
#====================================================

set.seed(123)

N <- 10000

beta0_prior <- runif(
  N,
  min = beta0_lower,
  max = beta0_upper
)

beta1_prior <- runif(
  N,
  min = beta1_lower,
  max = beta1_upper
)

# Display first few samples
head(beta0_prior)
head(beta1_prior)

#====================================================
# Task 3
# Step 4 : Rejection ABC
#====================================================

# Fixed parameters from Model 2
beta2 <- coef(model2)["I(X1^3)"]
beta3 <- coef(model2)["I(X3^4)"]

# Observed output
y_obs <- EEG_Model$Y

# Number of prior samples
N <- length(beta0_prior)

# Distance vector
distance <- numeric(N)

# Loop through each sampled parameter pair
for(i in 1:N){
  
  # Model prediction
  y_sim <-
    beta0_prior[i] +
    beta1_prior[i] * EEG_Model$X4 +
    beta2 * (EEG_Model$X1^3) +
    beta3 * (EEG_Model$X3^4)
  
  # Euclidean distance
  distance[i] <-
    sqrt(sum((y_obs - y_sim)^2))
  
}
head(distance)

#====================================================
# Task 3
# Step 5 : Accept Posterior Samples
#====================================================

# Acceptance threshold (5%)
epsilon <- quantile(
  distance,
  0.05
)

accepted <- distance <= epsilon

# Posterior samples
posterior_beta0 <- beta0_prior[accepted]
posterior_beta1 <- beta1_prior[accepted]

cat(
  "Accepted Samples =",
  length(posterior_beta0),
  "\n"
)

#====================================================
# Task 3
# Marginal Posterior Distribution of β0
#====================================================

hist(
  posterior_beta0,
  breaks = 30,
  probability = TRUE,
  col = "skyblue",
  border = "black",
  main = expression("Marginal Posterior of " * beta[0]),
  xlab = expression(beta[0])
)

lines(
  density(posterior_beta0),
  col = "red",
  lwd = 2
)

abline(
  v = beta0_hat,
  col = "blue",
  lwd = 2,
  lty = 2
)

legend(
  "topright",
  legend = c("Posterior Density", "Least Squares Estimate"),
  col = c("red", "blue"),
  lwd = 2,
  lty = c(1,2)
)

#====================================================
# Marginal Posterior Distribution of β1
#====================================================

hist(
  posterior_beta1,
  breaks = 30,
  probability = TRUE,
  col = "lightgreen",
  border = "black",
  main = expression("Marginal Posterior of " * beta[1]),
  xlab = expression(beta[1])
)

lines(
  density(posterior_beta1),
  col = "red",
  lwd = 2
)

abline(
  v = beta1_hat,
  col = "blue",
  lwd = 2,
  lty = 2
)

legend(
  "topright",
  legend = c("Posterior Density", "Least Squares Estimate"),
  col = c("red", "blue"),
  lwd = 2,
  lty = c(1,2)
)

#====================================================
# Joint Posterior Distribution
#====================================================

plot(
  posterior_beta0,
  posterior_beta1,
  pch = 19,
  col = rgb(0,0,1,0.4),
  xlab = expression(beta[0]),
  ylab = expression(beta[1]),
  main = "Joint Posterior Distribution"
)

points(
  beta0_hat,
  beta1_hat,
  pch = 19,
  col = "red",
  cex = 2
)

legend(
  "topright",
  legend = c(
    "Posterior Samples",
    "Least Squares Estimate"
  ),
  col = c(
    rgb(0,0,1,0.4),
    "red"
  ),
  pch = 19
)

#====================================================
# Posterior Summary
#====================================================
posterior_summary <- data.frame(
  Parameter = c("Intercept (β0)", "X4 (β1)"),
  Mean = c(
    mean(posterior_beta0),
    mean(posterior_beta1)
  ),
  Median = c(
    median(posterior_beta0),
    median(posterior_beta1)
  ),
  SD = c(
    sd(posterior_beta0),
    sd(posterior_beta1)
  )
)

# Round only the numeric columns
posterior_summary[, 2:4] <- round(posterior_summary[, 2:4], 4)

posterior_summary

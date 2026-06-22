library(MASS)
library(car)
library(rgl)
library(rpart)
library(leaps)
set.seed(123)

# 1. Dataset Loading and Setup
df <- read.csv("global_university_students_performance_habits_10000.csv", 
               stringsAsFactors = FALSE, 
               check.names = FALSE)

# Conversion to factor
df$gender              <- factor(df$gender)
df$country             <- factor(df$country)
df$major               <- factor(df$major)
df$part_time_job       <- factor(df$part_time_job)
df$relationship_status <- factor(df$relationship_status)
df$family_income_level <- factor(df$family_income_level)
df$internet_quality    <- factor(df$internet_quality)
df$favorite_AI_tool    <- factor(df$favorite_AI_tool)
df$note_taking_method  <- factor(df$note_taking_method)

p <- dim(df)[2]
n <- dim(df)[1]

# 2. Data Split (80% Train, 20% Test)
n_train <- round(0.8 * nrow(df))
train_indices <- sample(seq_len(nrow(df)), size = n_train)

df_train <- df[train_indices, ]  
df_test  <- df[-train_indices, ] 

# 3. Quick EDA
head(df_train)
unique(names(df_train))
levels(df_train$family_income_level)


# ==========================================
# 4. Simple Linear Model
# ==========================================
par(mfrow = c(1,1))
simple_fit <- lm(GPA ~ study_hours_per_day, data = df_train)
summary(simple_fit)

# Intervals
conf <- predict(simple_fit, interval = "confidence", level = 0.95)
pred <- predict(simple_fit, interval = "prediction", level = 0.95)

# Plot and lines
plot(df_train$study_hours_per_day, df_train$GPA)
abline(simple_fit, col = "red")

order_index <- order(df_train$study_hours_per_day)
lines(df_train$study_hours_per_day[order_index], conf[order_index, "lwr"], lty = 2, col = "blue", lwd = 4)
lines(df_train$study_hours_per_day[order_index], conf[order_index, "upr"], lty = 2, col = "blue", lwd = 4)
lines(df_train$study_hours_per_day[order_index], pred[order_index, "lwr"], lty = 3, col = "darkgreen", lwd = 4)
lines(df_train$study_hours_per_day[order_index], pred[order_index, "upr"], lty = 3, col = "darkgreen", lwd = 4)

# ---> Test on Simple Model (Metrics on Test data)
pred_test_simple <- predict(simple_fit, newdata = df_test)

# 1. RMSE
rmse_simple <- sqrt(mean((df_test$GPA - pred_test_simple)^2))
cat("Simple Model RMSE (Test Data):", rmse_simple, "\n")

# 2. Calculation of real R-squared on predicted data (Test Set)
SST_test_simple <- sum((df_test$GPA - mean(df_test$GPA))^2)
SSR_test_simple <- sum((df_test$GPA - pred_test_simple)^2)
r2_test_simple  <- 1 - (SSR_test_simple / SST_test_simple)

cat("Simple R-squared (Real Test):", r2_test_simple, "\n\n")

# ==========================================
# 5. Variable Selection (Forward Method)
# ==========================================
df_clean_train <- df_train[, names(df_train) != "student_id"]

var_select <- regsubsets(GPA ~ ., data = df_clean_train, nvmax = p, method = "forward")
res_select <- summary(var_select)


# Elbow plot (Adjusted R-squared)
plot(res_select$adjr2, 
     type = "b", pch = 19, col = "steelblue",
     xlab = "Number of Variables", ylab = "Adjusted R-squared",
     main = "Variable Selection: Adjusted R-squared")
points(7, res_select$adjr2[7], col = "red", pch = 19, cex = 1.5)

print(names(coef(var_select, 7)))


# ==========================================
# 6. Multiple Linear Model (Selected Variables)
# ==========================================
fit_multi <- lm(GPA ~ study_hours_per_day + class_attendance_percent + 
                  sleep_hours + social_media_hours + mental_stress_level + 
                  final_exam_score + assignment_score, 
                data = df_train)
summary(fit_multi)

# 1. Diagnostic plots of the residuals
x11()
par(mfrow = c(2,2))
plot(fit_multi)
par(mfrow = c(1,1))

dev.off()
# 2. Formal Normality test on the residuals
shapiro.test(sample(residuals(fit_multi), 5000)) #not normal ok


vif_values <- vif(fit_multi)
vif_values

#OSSERVATION:
# This anomaly observed in the residuals is directly caused by the fact
# that the provided GPA data is strictly capped at a maximum value of 4.0.
# Consequently, the model encounters a censoring effect where distinct 
# profiles—such as a student studying for 10 hours and another studying 
# for 12 hours—are forced to share the exact same recorded GPA of 4.0.

# Technically, this censoring biases the OLS coefficient estimates and 
# generates structurally impossible predictions above the 4.0 threshold. 
# However, for the primary objective of identifying the key drivers of 
# student performance (such as isolating the marginal effect of study 
# hours versus sleep hours), the linear model still provides a highly 
# interpretable, consistent, and robust empirical baseline.


# ---> Test on Multiple Model (RMSE on Test data)
pred_test_multi <- predict(fit_multi, newdata = df_test)
rmse_multi <- sqrt(mean((df_test$GPA - pred_test_multi)^2))
cat("Multiple Model RMSE (Test Data):", rmse_multi, "\n\n")

SST_test <- sum((df_test$GPA - mean(df_test$GPA))^2)
SSR_test <- sum((df_test$GPA - pred_test_multi)^2)
r2_test_multi <- 1 - (SSR_test / SST_test)

cat("Multiple R-squared (Real Test):", r2_test_multi, "\n")

#Bonferroni
k <- 7 # number of explanatory variables
alpha_bonf <- 0.05 / k
livello_conf <- 1 - alpha_bonf

# 1. Extraction of coefficients and intervals (excluding the intercept)
cf <- coef(fit_multi)[-1]
ci <- confint(fit_multi, level = livello_conf)[-1, ]

# 2. Sorting by absolute value (from smallest to largest)
ord <- order(abs(cf))
cf <- cf[ord]
ci <- ci[ord, ]

# 3. Plot 
par(mar = c(4, 14, 3, 2))
dotchart(cf, pch = 19, col = "steelblue", xlim = range(ci), 
         main = "Variable Impact (Ordered by Absolute Value)")
segments(ci[,1], 1:length(cf), ci[,2], 1:length(cf), col = "steelblue", lwd = 2)
abline(v = 0, col = "red", lty = 2, lwd = 2)



#Pair_plot just for the variables we used for the model
vars_modello <- c("GPA", "study_hours_per_day", "class_attendance_percent", 
                  "sleep_hours", "social_media_hours", "mental_stress_level", 
                  "final_exam_score", "assignment_score")

# pair plot with variable of the model
pairs(df_train[sample(nrow(df_train), 500), vars_modello], 
      main = "Scatterplot Matrix - Base R",
      col = "darkblue", 
      pch = 16, 
      cex = 0.5)


# --- Added-Variable Plots (avPlots) Explanation ---
# These plots display the pure marginal effect of each predictor on the GPA.
# The notation "Variable | others" means the data has been conditioned 
# on all the other regressors in the model (ceteris paribus assumption).
#
# Key takeaways for interpretation:
# 1. THE AXES: They do not show raw data, but RESIDUALS. 
#    - X-axis: The unique variation of the predictor, independent of others.
#    - Y-axis: The unique variation of the GPA, unexplained by others.
# 2. THE RED LINE: Its slope corresponds exactly to the partial coefficient 
#    estimated in the multiple OLS model (fit_multi).
par(mfrow = c(2, 4))
# avPlots   
avPlots(fit_multi, 
        col = "darkblue", 
        col.lines = "red", 
        lwd = 2, 
        pch = 16, 
        cex = 0.5,
        main = "Partial Effects (fit_multi)")

par(mfrow = c(1, 1))




#INFERENCE!

# Is it true that studying one hour more is better than sleeping one hour more?
#H0: beta_study = beta_sleep
C <- rbind(c(0,1,0,-1,0,0,0,0))
linearHypothesis(fit_multi, C)

#p-value 2.2e-16, so 0.05/2 > 2.2e-16, rejct H0
#and we know that B_study > sleep_hours

#H0: Does an additional hour of study per day provide double the return on GPA compared to an additional hour of sleep?
C <- rbind(c(0,1,0,-2,0,0,0,0))
linearHypothesis(fit_multi, C)

#YES!


# H0: Do the "lifestyle" variables (sleep, stress, social media) 
# jointly have ZERO effect on the GPA?
# (i.e., beta_sleep = 0 AND beta_stress = 0 AND beta_social_media = 0)
linearHypothesis(fit_multi, c("sleep_hours = 0", 
                              "social_media_hours = 0", 
                              "mental_stress_level = 0"))





# ==========================================
# 7. The "True Behavioral" Model (Avoiding Data Leakage)
# ==========================================

# BUSINESS QUESTION: 
# Can we predict a student's academic success strictly based on their 
# lifestyle and habits, without knowing their exam grades in advance?
#
# EXPLANATION: 
# Including 'final_exam_score' and 'assignment_score' to predict GPA creates 
# a structural tautology (Data Leakage), as GPA is mathematically derived 
# from those exact evaluations. This artificially inflates the R-squared 
# and completely absorbs the true impact of behavioral variables.

# IMPORTANT CAVEAT:
# It is true that a single exam score is subject to individual variance 
# (e.g., a top-GPA student having a bad day on one specific test). However, 
# using it as a predictor still artificially inflates the R-squared and 
# completely absorbs the true predictive power of behavioral variables.
#
# By removing the end-of-term grades, we estimate a pure predictive model 
# that reveals the actual early-semester drivers of student performance.

fit_behavioral <- lm(GPA ~ study_hours_per_day + class_attendance_percent + 
                       
                       sleep_hours + social_media_hours + mental_stress_level, 
                     
                     data = df_train)

# Let's observe how the R-squared drops to a realistic level, 
# while the coefficients for habits (study, sleep, stress) reveal their true impact.
summary(fit_behavioral)

# ---> Test on Behavioral Model (Metrics on Test data)
# Evaluating the real predictive power on unseen data
pred_test_behavioral <- predict(fit_behavioral, newdata = df_test)

# 1. Calculation of RMSE (Root Mean Squared Error)
rmse_behavioral <- sqrt(mean((df_test$GPA - pred_test_behavioral)^2))
cat("Behavioral Model RMSE (Test Data):", rmse_behavioral, "\n")

# 2. Calculation of out-of-sample R-squared (Test Set)
SST_test_behav <- sum((df_test$GPA - mean(df_test$GPA))^2)
SSR_test_behav <- sum((df_test$GPA - pred_test_behavioral)^2)
r2_test_behavioral <- 1 - (SSR_test_behav / SST_test_behav)

cat("Behavioral R-squared (Real Test):", r2_test_behavioral, "\n\n")

# Final Comparison
cat(sprintf(
  "\n======================================================\n--- TEST SET PERFORMANCE RECAP (Unseen Data) ---\n======================================================\nSimple Model            | R2: %.4f  | RMSE: %.4f\nMultiple Model          | R2: %.4f  | RMSE: %.4f\nBehavioral Model        | R2: %.4f  | RMSE: %.4f\n======================================================\n\n",
  r2_test_simple, rmse_simple,
  r2_test_multi, rmse_multi,
  r2_test_behavioral, rmse_behavioral
))
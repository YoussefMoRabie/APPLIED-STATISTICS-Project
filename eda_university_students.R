# Small EDA for global_university_students_performance_habits_10000.csv
# Mirrors the flow in university-student-performance-and-study-eda.ipynb (Kaggle notebook).
#
# Uses base R only. Optional: install.packages(c("readr", "dplyr", "ggplot2", "tidyr"))
# and swap read.csv -> readr::read_csv if you prefer tidyverse I/O.

csv_path <- "global_university_students_performance_habits_10000.csv"
if (!file.exists(csv_path)) {
  stop("Place this script in the project root (next to the CSV) or set csv_path.", call. = FALSE)
}

df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

# ---- Structure (head / tail / shape / columns / types) ----
cat("Dimensions (rows, cols): ", nrow(df), " x ", ncol(df), "\n\n", sep = "")
cat("Column names:\n")
print(names(df))

cat("\n--- head(6) ---\n")
print(utils::head(df, 6))

cat("\n--- tail(6) ---\n")
print(utils::tail(df, 6))

cat("\n--- str() ---\n")
str(df, vec.len = 2)

# ---- Missing values & duplicates ----
na_counts <- colSums(is.na(df))
cat("\n--- Missing values per column (non-zero only) ---\n")
nz <- na_counts[na_counts > 0]
if (length(nz)) print(nz) else cat("(none)\n")

cat("\nDuplicate rows: ", sum(duplicated(df)), "\n", sep = "")

# ---- Numeric summaries ----
num_idx <- vapply(df, is.numeric, logical(1))
cat("\n--- summary() for numeric columns ---\n")
print(summary(df[, num_idx, drop = FALSE]))

# ---- Plots (same themes as notebook, compact subset) ----
# 1–2: histograms (GPA, study hours); 3–6: scatter / boxplot; 7: correlation heatmap
plot_no <- 1L

hist(df$GPA, breaks = 30, col = adjustcolor("#4C72B0", 0.85), border = "white",
     main = paste0(plot_no, ". Distribution of GPA"), xlab = "GPA", freq = FALSE)
lines(density(df$GPA, na.rm = TRUE), col = "#C44E52", lwd = 2)
plot_no <- plot_no + 1L

hist(df$study_hours_per_day, breaks = 30, col = adjustcolor("#55A868", 0.85), border = "white",
     main = paste0(plot_no, ". Daily study hours"), xlab = "Study hours / day", freq = FALSE)
lines(density(df$study_hours_per_day, na.rm = TRUE), col = "#C44E52", lwd = 2)
plot_no <- plot_no + 1L

plot(df$study_hours_per_day, df$GPA, pch = 16, col = rgb(0, 0, 0, 0.12), cex = 0.5,
     xlab = "Study hours / day", ylab = "GPA",
     main = paste0(plot_no, ". Study hours vs GPA"))
abline(lm(GPA ~ study_hours_per_day, data = df), col = "#C44E52", lwd = 2)
plot_no <- plot_no + 1L

boxplot(GPA ~ gender, data = df, col = adjustcolor("#8172B3", 0.6), outline = FALSE,
        main = paste0(plot_no, ". GPA by gender"), xlab = "Gender", ylab = "GPA")
points(as.numeric(factor(df$gender)), df$GPA,
       pch = 16, col = rgb(0, 0, 0, 0.08), cex = 0.35)
plot_no <- plot_no + 1L

plot(df$sleep_hours, df$GPA, pch = 16, col = rgb(0, 0, 0, 0.12), cex = 0.5,
     xlab = "Sleep hours", ylab = "GPA",
     main = paste0(plot_no, ". Sleep hours vs GPA"))
abline(lm(GPA ~ sleep_hours, data = df), col = "#C44E52", lwd = 2)
plot_no <- plot_no + 1L

plot(df$screen_time_hours, df$GPA, pch = 16, col = rgb(0, 0, 0, 0.12), cex = 0.5,
     xlab = "Screen time (hours)", ylab = "GPA",
     main = paste0(plot_no, ". Screen time vs GPA"))
abline(lm(GPA ~ screen_time_hours, data = df), col = "#C44E52", lwd = 2)
plot_no <- plot_no + 1L

# ---- Correlation heatmap (numeric features only; like notebook) ----
num_df <- df[, num_idx, drop = FALSE]
if (ncol(num_df) < 2) {
  warning("Not enough numeric columns for a correlation matrix.")
} else {
  cor_mat <- cor(num_df, use = "pairwise.complete.obs")
  main_title <- paste0(plot_no, ". Correlation heatmap (numeric features)")
  heatmap(
    cor_mat,
    Rowv = NA,
    Colv = NA,
    symm = TRUE,
    col = hcl.colors(50, "RdBu", rev = TRUE),
    margins = c(10, 10),
    main = main_title,
    cexRow = 0.55,
    cexCol = 0.55
  )
}

cat("\nEDA complete.\n")

#-------------------------------------------PCA-----------------------------------------------------

# Keep numeric variables only
students.num <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]

students.num <- students.num[, colnames(students.num) != "university_year"] #not continous distribution

#gaming hours is righ skewed need log transform
#first we add 1 for all value of gaming hour if there any zeros prevent the log can get error and goes to infinity
students.num$gaming_hours <- students.num$gaming_hours + 1
students.num$gaming_hours <- log(students.num$gaming_hours)

par(mfrow = c(1,2))
hist(df$gaming_hours, main = "Before", col = "red")
hist(students.num$gaming_hours, main = "After Log", col = "green")

#coffee consumption per day is righ skewed need log transform
#first we add 1 for all value of coffee consumption per day if there any zeros prevent the log can get error and goes to infinity
students.num$coffee_consumption_per_day <- students.num$coffee_consumption_per_day + 1
students.num$coffee_consumption_per_day <- log(students.num$coffee_consumption_per_day)

par(mfrow = c(1,2))
hist(df$coffee_consumption_per_day, main = "Before", col = "red")
hist(students.num$coffee_consumption_per_day, main = "After Log", col = "green")

#-------------------------------------------Applying PCA-----------------------------------------------------

students.num.sd <- scale(students.num)
students.num.sd <- data.frame(students.num.sd)

par(mfrow = c(1, 1))
boxplot(students.num.sd, las = 2, col = 'gold', main = "Standardized Data")

pc.students <- princomp(students.num.sd, scores = TRUE)
summary(pc.students)

#------------------------------------------visualization---------------------------------------------------

graphics.off()

# Visualization of variance
layout(matrix(c(2, 3, 1, 3), 2, byrow = TRUE))

# Variance explained by PCs
p <- ncol(students.num.sd)
plot(pc.students, las = 2, main = 'Principal Components', ylim = c(0, p))
abline(h = 1, col = 'blue')

# Variances of original standardized variables
barplot(sapply(students.num.sd, sd)^2, las = 2, main = 'Original Variables', ylim = c(0, p),
        ylab = 'Variances')

# Cumulative variance
plot(cumsum(pc.students$sdev^2) / sum(pc.students$sdev^2), type = 'b', axes = FALSE, 
     xlab = 'Number of components', ylab = 'Contribution to the total variance', ylim = c(0, 1))
abline(h = 1, col = 'blue')
abline(h = 0.8, lty = 2, col = 'blue')
box()
axis(2, at = 0:10/10, labels = 0:10/10)
axis(1, at = 1:ncol(students.num.sd),labels = 1:ncol(students.num.sd),las = 2)

#Approximately 8 to 9 principal components are required to explain 80% of the total variance
# If we wanted to perform dimensionality reduction, we could keep 9 PCs 

#-------------------------------------------------------Loading-------------------------------------------------------

# Loadings
load.students <- pc.students$loadings
load.students

# Graphical representation
par(mar = c(8,4,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.students[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''),las=2,cex.names = 0.6)

#PC1 = Academic Performance vs Distraction
#if PC1  is high: Hardworking student
#if PC1 is low: Waste of his time

#PC2 = Lifestyle / Well-being vs Digital Stress
#if PC2  is high: balanced / healthy lifestyle
#if PC2 is low: Stressed/unhealthy lifestyle

#PC3 = Stress vs Activity / Lifestyle
#if PC3  is high: A stressed person who isn't living their life
#if PC3 is low: balanced person

## ---------------------------------------------------Scores ----------------------------------------------------------
scores.students <- pc.students$scores
scores.students

#scores plot
par(mfrow = c(1,1))
plot(scores.students[, 1:2], pch = 19, col = "darkblue", xlab = "PC1", ylab = "PC2", main = "Scores Plot: Students")
abline(h = 0, v = 0, lty = 2, col = "grey")

#Biplot
par(mfrow = c(1,1))
biplot(pc.students, cex = c(0.5, 0.8))

#We color according to categorical variables
students.label <- df[, !vapply(df, is.numeric, logical(1))]

head(students.label)
names(students.label)

gender.label <- factor(df$gender)

colors.gender <- c("red", "blue", "darkgreen", "purple")
col.gender <- colors.gender[as.numeric(gender.label)]

plot(scores.students[, 1:2], col = col.gender, pch = 19, xlab = "PC1", ylab = "PC2", main = "Scores Plot Colored by Gender")
abline(h = 0, v = 0, lty = 2, col = "grey")

legend("topright", legend = levels(gender.label),fill = colors.gender[1:length(levels(gender.label))],
       bty = "n")

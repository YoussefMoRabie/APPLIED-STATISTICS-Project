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

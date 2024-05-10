# Ensure necessary packages are installed and loaded
if (!require(psych, quietly = TRUE)) {
  install.packages("psych")
  library(psych)
}
if (!require(e1071, quietly = TRUE)) {
  install.packages("e1071")
  library(e1071)
}

# Data initialization
students <- 1:17
no_visual_aids <- c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61)
with_visual_aids <- c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)

data <- data.frame(
  Students = students,
  NoVisualAids = no_visual_aids,
  WithVisualAids = with_visual_aids
)

# Descriptive statistics using psych package
describe_stats <- describe(data[, c("NoVisualAids", "WithVisualAids")])
print(describe_stats)

# Boxplot visualization
boxplot(data$WithVisualAids, data$NoVisualAids,
        names = c("With Visual Aids", "No Visual Aids"),
        main = "Boxplot of Scores with and without Visual Aids",
        ylab = "Scores",
        col = c("green", "orange"))

# Plotting histograms for both conditions
par(mfrow = c(1, 2))  # Setting up the plotting area to have 2 plots side by side
hist(data$NoVisualAids, main = "Histogram for No Visual Aids", xlab = "Scores", col = "blue", breaks = 10)
abline(v = mean(data$NoVisualAids), col = "red", lwd = 2)
hist(data$WithVisualAids, main = "Histogram for With Visual Aids", xlab = "Scores", col = "green", breaks = 10)
abline(v = mean(data$WithVisualAids), col = "red", lwd = 2)

# Descriptive statistics
mean_no_visual <- mean(data$NoVisualAids)
sd_no_visual <- sd(data$NoVisualAids)
skewness_no_visual <- skewness(data$NoVisualAids)
mean_with_visual <- mean(data$WithVisualAids)
sd_with_visual <- sd(data$WithVisualAids)
skewness_with_visual <- skewness(data$WithVisualAids)

# Output descriptive statistics
cat("Descriptive Statistics for No Visual Aids:\n")
cat("Mean:", mean_no_visual, "\nSD:", sd_no_visual, "\nSkewness:", skewness_no_visual, "\n\n")
cat("Descriptive Statistics for With Visual Aids:\n")
cat("Mean:", mean_with_visual, "\nSD:", sd_with_visual, "\nSkewness:", skewness_with_visual, "\n")

# Shapiro-Wilk normality tests
normality_no_visual <- shapiro.test(data$NoVisualAids)$p.value
normality_with_visual <- shapiro.test(data$WithVisualAids)$p.value
cat("Shapiro-Wilk Test for No Visual Aids: p-value =", normality_no_visual, "\n")
cat("Shapiro-Wilk Test for With Visual Aids: p-value =", normality_with_visual, "\n")

# Decision-making for statistical tests
if (normality_with_visual > 0.05 && normality_no_visual > 0.05) {
  # Both distributions assumed normal
  test_result <- t.test(data$WithVisualAids, data$NoVisualAids, paired = TRUE)
  test_type <- "Paired t-test"
  ci <- test_result$conf.int
  cat("Using t-test, Confidence Interval for mean difference: [", ci[1], ", ", ci[2], "]\n")
} else {
  # Non-normal distributions
  test_result <- wilcox.test(data$WithVisualAids, data$NoVisualAids, paired = TRUE)
  test_type <- "Wilcoxon signed-rank test"
  cat("Using Wilcoxon test; CI for median differences not calculated.\n")
}
cat("Test used: ", test_type, "\n")
print(test_result)

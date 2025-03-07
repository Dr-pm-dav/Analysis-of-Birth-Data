#CS871_IP2

# Load the required library
library(nutshell)

# Load the Births dataset
data(births2006.smpl)

# Rename the dataset
data=births2006.smpl

# Display the first 5 rows
head(data, 5)

# Bar chart for frequencies by day of the week
births_by_day <- table(data$DOB_WK)
barplot(births_by_day, main = "Births by Day of the Week",
        xlab = "Day of the Week", ylab = "Number of Births", col = "skyblue")

# Cross-tabulation of day of the week and method of delivery
births_two_way <- table(data$DOB_WK, data$DPLURAL)
print(births_two_way)

# Load the lattice package
library(lattice)

# Density histogram conditioned on multiple births and method of delivery
histogram(~DBWT | factor(DPLURAL) + factor(data$DPLURAL), data,
          main = "Density Histogram of Birth Weight",
          xlab = "Birth Weight", col = "lightblue")

# Box plot: Birth weight vs. Apgar score
boxplot(DBWT ~ APGAR5, data,
        main = "Birth Weight by Apgar Score",
        xlab = "Apgar Score", ylab = "Birth Weight (grams)", col = "lightgreen")

# Box plot: Birth weight by day of the week
boxplot(DBWT ~ DOB_WK, data,
        main = "Birth Weight by Day of the Week",
        xlab = "Day of the Week", ylab = "Birth Weight (grams)", col = "orange")

# Average birth weight by gender and multiple births
avg_weight <- tapply(data$DBWT, list(data$SEX, data$DPLURAL), mean, na.rm = TRUE)
barplot(avg_weight, beside = TRUE, col = c("blue", "pink"),
        legend = rownames(avg_weight), main = "Average Birth Weight",
        xlab = "Multiple Births", ylab = "Average Weight (grams)")


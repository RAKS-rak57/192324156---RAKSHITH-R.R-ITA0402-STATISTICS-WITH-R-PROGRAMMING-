# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 17
# TITANIC DATASET VISUALIZATION
# ======================================================================
#
# AIM:
#   To explore the Titanic dataset: bar chart of survival vs class, modify the plot by gender and draw a histogram of Age.
#
# PSEUDOCODE:
#   1. Start
#   2. Read titanic.csv (kept in the same folder; falls back to a GitHub copy)
#   3. Create table of Survived vs Pclass and draw bar chart
#   4. Create table of Survived vs Sex and draw grouped bar chart
#   5. Draw histogram of Age (ignoring NA values)
#   6. Stop
#
# SAMPLE INPUT:
#   File: titanic.csv (891 passengers) - keep it in the working directory
#
# SAMPLE OUTPUT:
#   Rows: 891  Columns: 12 
#           Class
#   Survived   1   2   3
#          0  80  97 372
#          1 136  87 119
#           Sex
#   Survived female male
#          0     81  468
#          1    233  109
#   Mean age: 29.7
#   [Graph(s) appear in the R Graphics window]
#
# ======================================================================
# PROGRAM
# ======================================================================

if (file.exists("titanic.csv")) {
  titanic <- read.csv("titanic.csv")
} else {
  titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
}
cat("Rows:", nrow(titanic), " Columns:", ncol(titanic), "\n")

# Bar chart: Survival vs Class
t1 <- table(Survived = titanic$Survived, Class = titanic$Pclass)
print(t1)
barplot(t1, beside = TRUE, col = c("tomato", "seagreen"),
        main = "Survival by Passenger Class", xlab = "Class", ylab = "Passengers",
        legend.text = c("Died", "Survived"))

# Modified plot: by gender
t2 <- table(Survived = titanic$Survived, Sex = titanic$Sex)
print(t2)
barplot(t2, beside = TRUE, col = c("tomato", "seagreen"),
        main = "Survival by Gender", xlab = "Gender", ylab = "Passengers",
        legend.text = c("Died", "Survived"))

# Histogram of Age
hist(titanic$Age, breaks = 15, col = "skyblue", main = "Histogram of Age", xlab = "Age")
cat("Mean age:", round(mean(titanic$Age, na.rm = TRUE), 2), "\n")

# ======================================================================
# RESULT:
#   Bar charts of survival by class and gender and a histogram of Age were drawn successfully (see the Graphics window).
# ======================================================================

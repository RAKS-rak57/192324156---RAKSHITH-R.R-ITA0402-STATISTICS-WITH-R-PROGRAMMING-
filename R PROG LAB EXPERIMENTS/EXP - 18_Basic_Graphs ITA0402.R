# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 18
# BASIC GRAPHS IN R
# ======================================================================
#
# AIM:
#   To create a boxplot, histogram, bar plot, line chart and scatter plot in R.
#
# PSEUDOCODE:
#   1. Start
#   2. Take sample data (mtcars and simple vectors)
#   3. Draw boxplot using boxplot()
#   4. Draw histogram using hist()
#   5. Draw bar plot using barplot()
#   6. Draw line chart using plot(type = "o")
#   7. Draw scatter plot using plot()
#   8. Stop
#
# SAMPLE INPUT:
#   Dataset: mtcars (built-in) and months/sales vectors
#
# SAMPLE OUTPUT:
#   Five graphs created.
#   [Graph(s) appear in the R Graphics window]
#
# ======================================================================
# PROGRAM
# ======================================================================

par(mfrow = c(2, 3))

boxplot(mpg ~ cyl, data = mtcars, main = "Boxplot", xlab = "Cylinders",
        ylab = "Mileage", col = "lightblue")

hist(mtcars$mpg, main = "Histogram", xlab = "Mileage", col = "orange")

counts <- table(mtcars$gear)
barplot(counts, main = "Bar Plot", xlab = "Gears", ylab = "Cars", col = "green")

sales <- c(20, 35, 30, 45, 60, 55)
plot(1:6, sales, type = "o", main = "Line Chart", xlab = "Month", ylab = "Sales", col = "red")

plot(mtcars$wt, mtcars$mpg, main = "Scatter Plot", xlab = "Weight", ylab = "Mileage",
     pch = 19, col = "blue")

par(mfrow = c(1, 1))
cat("Five graphs created.\n")

# ======================================================================
# RESULT:
#   Boxplot, histogram, bar plot, line chart and scatter plot were created successfully (see the Graphics window).
# ======================================================================

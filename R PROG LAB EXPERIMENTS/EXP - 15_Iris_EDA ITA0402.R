# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 15
# EXPLORATORY DATA ANALYSIS ON IRIS
# ======================================================================
#
# AIM:
#   To perform EDA on the iris dataset: dimensions, summary, standard deviation, quantiles, grouping by Species, pivot table and categorical grouping of Sepal.Length.
#
# PSEUDOCODE:
#   1. Start
#   2. Load iris and display dimensions, head and summary
#   3. Find standard deviation of numeric columns using sapply()
#   4. Find quantiles of Sepal.Length
#   5. Group by Species and compute mean using aggregate()
#   6. Create pivot table using tapply()
#   7. Create Sepal.Length categories using cut() and tabulate with Species
#   8. Stop
#
# SAMPLE INPUT:
#   Dataset: iris (built-in)
#
# SAMPLE OUTPUT:
#   Dimensions: 150 5 
#
#     Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#   1          5.1         3.5          1.4         0.2  setosa
#   2          4.9         3.0          1.4         0.2  setosa
#   3          4.7         3.2          1.3         0.2  setosa
#   4          4.6         3.1          1.5         0.2  setosa
#   5          5.0         3.6          1.4         0.2  setosa
#   6          5.4         3.9          1.7         0.4  setosa
#
#   Summary:
#     Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
#    Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
#    1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
#    Median :5.800   Median :3.000   Median :4.350   Median :1.300  
#    Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
#    3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
#    Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
#          Species  
#    setosa    :50  
#    versicolor:50  
#    virginica :50  
#
#
#
#
#   Standard deviation:
#   Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#      0.8280661    0.4358663    1.7652982    0.7622377 
#
#   Quantiles of Sepal.Length:
#     0%  25%  50%  75% 100% 
#    4.3  5.1  5.8  6.4  7.9 
#
#   Mean of each variable grouped by Species:
#        Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#   1     setosa        5.006       3.428        1.462       0.246
#   2 versicolor        5.936       2.770        4.260       1.326
#   3  virginica        6.588       2.974        5.552       2.026
#
#   Pivot table (mean Petal.Length by Species):
#       setosa versicolor  virginica 
#        1.462      4.260      5.552 
#
#   Sepal.Length categories:
#
#                Short Medium Long
#     setosa        47      3    0
#     versicolor    11     31    8
#     virginica      1     27   22
#
# ======================================================================
# PROGRAM
# ======================================================================

data(iris)
cat("Dimensions:", dim(iris), "\n\n")
print(head(iris))

cat("\nSummary:\n")
print(summary(iris))

cat("\nStandard deviation:\n")
print(sapply(iris[, 1:4], sd))

cat("\nQuantiles of Sepal.Length:\n")
print(quantile(iris$Sepal.Length))

cat("\nMean of each variable grouped by Species:\n")
print(aggregate(. ~ Species, data = iris, FUN = mean))

cat("\nPivot table (mean Petal.Length by Species):\n")
print(tapply(iris$Petal.Length, iris$Species, mean))

cat("\nSepal.Length categories:\n")
iris$SL_cat <- cut(iris$Sepal.Length, breaks = c(0, 5.5, 6.5, 10),
                   labels = c("Short", "Medium", "Long"))
print(table(iris$Species, iris$SL_cat))

# ======================================================================
# RESULT:
#   EDA on the iris dataset was completed and all the statistics were displayed successfully.
# ======================================================================

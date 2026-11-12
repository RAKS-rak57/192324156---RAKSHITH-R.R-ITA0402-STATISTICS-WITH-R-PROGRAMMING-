# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 21
# LOGISTIC REGRESSION ON IRIS WITH TRAIN/TEST SPLIT
# ======================================================================
#
# AIM:
#   To split iris randomly into train/test (80/20), build a logistic regression (Species ~ Petal.Length + Petal.Width), predict and evaluate with a confusion matrix.
#
# PSEUDOCODE:
#   1. Start
#   2. Set seed and randomly select 80% of rows for training, 20% for testing
#   3. Build logistic regression on training data (multinom handles 3 species)
#   4. Predict Species on test data
#   5. Create confusion matrix using table(Actual, Predicted)
#   6. Compute accuracy = correct / total
#   7. Stop
#
# SAMPLE INPUT:
#   Dataset: iris (built-in) | Split: 80% train (120 rows), 20% test (30 rows) | set.seed(123)
#
# SAMPLE OUTPUT:
#   Training rows: 120  Testing rows: 30 
#              (Intercept) Petal.Length Petal.Width
#   versicolor   -22.80429     6.592006    8.275971
#   virginica    -68.83031    12.810970   18.028532
#
#   Confusion Matrix:
#               Predicted
#   Actual       setosa versicolor virginica
#     setosa         10          0         0
#     versicolor      0         14         1
#     virginica       0          0         5
#
#   Accuracy: 96.67 %
#
# ======================================================================
# PROGRAM
# ======================================================================

# NOTE: glm(family = binomial) works only for 2 classes. Iris has 3 species,
# so multinomial logistic regression (nnet::multinom) is used.
library(nnet)

set.seed(123)
idx   <- sample(1:nrow(iris), 0.8 * nrow(iris))
train <- iris[idx, ]
test  <- iris[-idx, ]
cat("Training rows:", nrow(train), " Testing rows:", nrow(test), "\n")

model <- multinom(Species ~ Petal.Length + Petal.Width, data = train, trace = FALSE)
print(summary(model)$coefficients)

pred <- predict(model, test)

cm <- table(Actual = test$Species, Predicted = pred)
cat("\nConfusion Matrix:\n")
print(cm)

accuracy <- sum(diag(cm)) / sum(cm)
cat("\nAccuracy:", round(accuracy * 100, 2), "%\n")

# ======================================================================
# RESULT:
#   The train/test split, logistic regression model, prediction and confusion matrix evaluation were completed successfully.
# ======================================================================

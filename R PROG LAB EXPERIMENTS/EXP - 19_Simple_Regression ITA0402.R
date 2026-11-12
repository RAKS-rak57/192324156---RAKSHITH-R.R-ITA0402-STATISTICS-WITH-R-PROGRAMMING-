# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 19
# SIMPLE LINEAR REGRESSION (SALES ~ SPEND)
# ======================================================================
#
# AIM:
#   To build a simple regression model on an advertising dataset (Sales ~ Spend) and predict Sales.
#
# PSEUDOCODE:
#   1. Start
#   2. Create advertising data frame with Spend and Sales
#   3. Build model using lm(Sales ~ Spend)
#   4. Display model summary (coefficients, R-squared)
#   5. Predict Sales for new Spend value using predict()
#   6. Plot data with regression line
#   7. Stop
#
# SAMPLE INPUT:
#   Spend = 10,20,30,40,50,60,70,80,90,100 | Sales = 25,32,41,48,57,66,71,82,90,98 | New Spend = 120
#
# SAMPLE OUTPUT:
#
#   Call:
#   lm(formula = Sales ~ Spend, data = advertising)
#
#   Residuals:
#       Min      1Q  Median      3Q     Max 
#   -2.2364 -0.3167  0.3424  0.5667  0.9212 
#
#   Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 16.13333    0.68106   23.69 1.07e-08 ***
#   Spend        0.81576    0.01098   74.32 1.20e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#   Residual standard error: 0.997 on 8 degrees of freedom
#   Multiple R-squared:  0.9986,	Adjusted R-squared:  0.9984 
#   F-statistic:  5524 on 1 and 8 DF,  p-value: 1.197e-12
#
#
#   Coefficients:
#   (Intercept)       Spend 
#    16.1333333   0.8157576 
#
#   Predicted Sales for Spend = 120 : 114.02
#
# ======================================================================
# PROGRAM
# ======================================================================

advertising <- data.frame(
  Spend = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  Sales = c(25, 32, 41, 48, 57, 66, 71, 82, 90, 98)
)

model <- lm(Sales ~ Spend, data = advertising)
print(summary(model))

cat("\nCoefficients:\n")
print(coef(model))

new_data <- data.frame(Spend = 120)
pred <- predict(model, new_data)
cat("\nPredicted Sales for Spend = 120 :", round(pred, 2), "\n")

plot(advertising$Spend, advertising$Sales, pch = 19, col = "blue",
     main = "Sales vs Spend", xlab = "Spend", ylab = "Sales")
abline(model, col = "red", lwd = 2)

# ======================================================================
# RESULT:
#   A regression model was built, Sales for Spend = 120 was predicted and the fitted line was plotted successfully.
# ======================================================================

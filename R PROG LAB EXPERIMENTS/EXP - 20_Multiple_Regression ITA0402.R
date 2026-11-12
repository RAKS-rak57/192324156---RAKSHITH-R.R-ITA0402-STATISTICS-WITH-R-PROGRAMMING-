# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 20
# MULTIPLE REGRESSION ON CHICKWEIGHT
# ======================================================================
#
# AIM:
#   To create a multiple regression model on ChickWeight with Time and Diet as predictors, predict weight and compute model error.
#
# PSEUDOCODE:
#   1. Start
#   2. Load ChickWeight as a data frame
#   3. Build model using lm(weight ~ Time + Diet)
#   4. Display model summary
#   5. Predict weight for all records and for a new chick (Time = 10, Diet = 1)
#   6. Compute error: residuals, RMSE and MAE
#   7. Stop
#
# SAMPLE INPUT:
#   Dataset: ChickWeight (built-in) | New data: Time = 10, Diet = 1
#
# SAMPLE OUTPUT:
#
#   Call:
#   lm(formula = weight ~ Time + Diet, data = cw)
#
#   Residuals:
#        Min       1Q   Median       3Q      Max 
#   -136.851  -17.151   -2.595   15.033  141.816 
#
#   Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  10.9244     3.3607   3.251  0.00122 ** 
#   Time          8.7505     0.2218  39.451  < 2e-16 ***
#   Diet2        16.1661     4.0858   3.957 8.56e-05 ***
#   Diet3        36.4994     4.0858   8.933  < 2e-16 ***
#   Diet4        30.2335     4.1075   7.361 6.39e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#   Residual standard error: 35.99 on 573 degrees of freedom
#   Multiple R-squared:  0.7453,	Adjusted R-squared:  0.7435 
#   F-statistic: 419.2 on 4 and 573 DF,  p-value: < 2.2e-16
#
#
#   Predicted weight (Time=10, Diet=1): 98.43 
#
#   RMSE : 35.84 
#   MAE  : 25 
#   R-squared: 0.7453
#
# ======================================================================
# PROGRAM
# ======================================================================

cw <- as.data.frame(ChickWeight)

model <- lm(weight ~ Time + Diet, data = cw)
print(summary(model))

# Prediction for a new chick
new_chick <- data.frame(Time = 10, Diet = factor("1", levels = levels(cw$Diet)))
cat("\nPredicted weight (Time=10, Diet=1):", round(predict(model, new_chick), 2), "\n")

# Model error
pred <- predict(model, cw)
error <- cw$weight - pred
rmse <- sqrt(mean(error^2))
mae  <- mean(abs(error))

cat("\nRMSE :", round(rmse, 2), "\n")
cat("MAE  :", round(mae, 2), "\n")
cat("R-squared:", round(summary(model)$r.squared, 4), "\n")

# ======================================================================
# RESULT:
#   The multiple regression model was built, weight was predicted and the model error (RMSE, MAE) was computed successfully.
# ======================================================================

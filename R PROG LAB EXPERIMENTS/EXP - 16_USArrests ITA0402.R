# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 16
# USARRESTS DATASET EXPLORATION
# ======================================================================
#
# AIM:
#   To explore the USArrests dataset: summary, state with largest Rape arrests, max/min Murder rates, correlation, filtering and visualization.
#
# PSEUDOCODE:
#   1. Start
#   2. Load USArrests and display summary
#   3. Find state with maximum Rape value using which.max()
#   4. Find states with maximum and minimum Murder rate
#   5. Find correlation matrix using cor()
#   6. Filter states with Assault above median and Murder in bottom 25%
#   7. Plot histogram, density, scatter plot and bar graph
#   8. Stop
#
# SAMPLE INPUT:
#   Dataset: USArrests (built-in)
#
# SAMPLE OUTPUT:
#              Murder Assault UrbanPop Rape
#   Alabama      13.2     236       58 21.2
#   Alaska       10.0     263       48 44.5
#   Arizona       8.1     294       80 31.0
#   Arkansas      8.8     190       50 19.5
#   California    9.0     276       91 40.6
#   Colorado      7.9     204       78 38.7
#
#   Summary:
#        Murder          Assault         UrbanPop          Rape      
#    Min.   : 0.800   Min.   : 45.0   Min.   :32.00   Min.   : 7.30  
#    1st Qu.: 4.075   1st Qu.:109.0   1st Qu.:54.50   1st Qu.:15.07  
#    Median : 7.250   Median :159.0   Median :66.00   Median :20.10  
#    Mean   : 7.788   Mean   :170.8   Mean   :65.54   Mean   :21.23  
#    3rd Qu.:11.250   3rd Qu.:249.0   3rd Qu.:77.75   3rd Qu.:26.18  
#    Max.   :17.400   Max.   :337.0   Max.   :91.00   Max.   :46.00  
#
#   State with largest Rape arrests: Nevada 
#   State with maximum Murder rate : Georgia ( 17.4 )
#   State with minimum Murder rate : North Dakota ( 0.8 )
#
#   Correlation among features:
#            Murder Assault UrbanPop Rape
#   Murder     1.00    0.80     0.07 0.56
#   Assault    0.80    1.00     0.26 0.67
#   UrbanPop   0.07    0.26     1.00 0.41
#   Rape       0.56    0.67     0.41 1.00
#
#   States with Assault above median:
#    [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
#    [5] "California"     "Colorado"       "Delaware"       "Florida"       
#    [9] "Georgia"        "Illinois"       "Louisiana"      "Maryland"      
#   [13] "Michigan"       "Mississippi"    "Missouri"       "Nevada"        
#   [17] "New Mexico"     "New York"       "North Carolina" "Rhode Island"  
#   [21] "South Carolina" "Tennessee"      "Texas"          "Wyoming"       
#
#   States in bottom 25% for Murder:
#    [1] "Connecticut"   "Idaho"         "Iowa"          "Maine"        
#    [5] "Minnesota"     "New Hampshire" "North Dakota"  "Rhode Island" 
#    [9] "South Dakota"  "Utah"          "Vermont"       "Washington"   
#   [13] "Wisconsin"
#   [4 plots appear in the R Graphics window]
#
# ======================================================================
# PROGRAM
# ======================================================================

data(USArrests)
print(head(USArrests))
cat("\nSummary:\n"); print(summary(USArrests))

cat("\nState with largest Rape arrests:", rownames(USArrests)[which.max(USArrests$Rape)], "\n")
cat("State with maximum Murder rate :", rownames(USArrests)[which.max(USArrests$Murder)],
    "(", max(USArrests$Murder), ")\n")
cat("State with minimum Murder rate :", rownames(USArrests)[which.min(USArrests$Murder)],
    "(", min(USArrests$Murder), ")\n")

cat("\nCorrelation among features:\n")
print(round(cor(USArrests), 2))

cat("\nStates with Assault above median:\n")
print(rownames(USArrests)[USArrests$Assault > median(USArrests$Assault)])

cat("\nStates in bottom 25% for Murder:\n")
print(rownames(USArrests)[USArrests$Murder <= quantile(USArrests$Murder, 0.25)])

# Visualization
par(mfrow = c(2, 2))
hist(USArrests$Murder, main = "Histogram of Murder", xlab = "Murder", col = "skyblue")
plot(density(USArrests$Assault), main = "Density of Assault", col = "red")
plot(USArrests$Assault, USArrests$Murder, main = "Assault vs Murder",
     xlab = "Assault", ylab = "Murder", pch = 19, col = "blue")
barplot(head(USArrests$Murder, 10), names.arg = head(rownames(USArrests), 10),
        las = 2, main = "Murder rate (first 10 states)", col = "orange")
par(mfrow = c(1, 1))

# ======================================================================
# RESULT:
#   USArrests data was explored with summary statistics, correlation, filtering and four plots successfully.
# ======================================================================

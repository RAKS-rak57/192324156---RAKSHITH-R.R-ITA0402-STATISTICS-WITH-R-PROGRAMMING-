# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 12
# DATA RESHAPING ON AIRQUALITY (MELT AND CAST)
# ======================================================================
#
# AIM:
#   To perform data reshaping on the airquality dataset using melt and cast, and compute monthly averages of Ozone, Solar.R, Wind and Temp.
#
# PSEUDOCODE:
#   1. Start
#   2. Load reshape2 and the airquality dataset
#   3. Convert column names to lowercase
#   4. Melt the data with id = (month, day)
#   5. Cast the molten data by month with mean using dcast()
#   6. Display monthly averages
#   7. Stop
#
# SAMPLE INPUT:
#   Dataset: airquality (built-in)
#
# SAMPLE OUTPUT:
#     ozone solar.r wind temp month day
#   1    41     190  7.4   67     5   1
#   2    36     118  8.0   72     5   2
#   3    12     149 12.6   74     5   3
#   4    18     313 11.5   62     5   4
#   5    NA      NA 14.3   56     5   5
#   6    28      NA 14.9   66     5   6
#
#   Molten data:
#     month day variable value
#   1     5   1    ozone    41
#   2     5   2    ozone    36
#   3     5   3    ozone    12
#   4     5   4    ozone    18
#   6     5   6    ozone    28
#   7     5   7    ozone    23
#
#   Monthly averages:
#     month ozone solar.r  wind  temp
#   1     5 23.62  181.30 11.62 65.55
#   2     6 29.44  190.17 10.27 79.10
#   3     7 59.12  216.48  8.94 83.90
#   4     8 59.96  171.86  8.79 83.97
#   5     9 31.45  167.43 10.18 76.90
#
# ======================================================================
# PROGRAM
# ======================================================================

if (!require(reshape2)) install.packages("reshape2")
library(reshape2)

names(airquality) <- tolower(names(airquality))
print(head(airquality))

# MELT: wide to long
aq_melt <- melt(airquality, id.vars = c("month", "day"), na.rm = TRUE)
cat("\nMolten data:\n")
print(head(aq_melt))

# CAST: long to wide with monthly averages
monthly_avg <- dcast(aq_melt, month ~ variable, mean)
cat("\nMonthly averages:\n")
print(round(monthly_avg, 2))

# ======================================================================
# RESULT:
#   The airquality data was melted and cast, and monthly averages were computed successfully.
# ======================================================================

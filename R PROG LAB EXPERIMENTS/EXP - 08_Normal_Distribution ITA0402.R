# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 8
# RANDOM NUMBERS FROM A NORMAL DISTRIBUTION
# ======================================================================
#
# AIM:
#   To generate random numbers from a normal distribution and count their occurrences.
#
# PSEUDOCODE:
#   1. Start
#   2. Set seed for reproducibility
#   3. Generate 20 numbers using rnorm(20, mean = 5, sd = 2)
#   4. Round the values to whole numbers
#   5. Count occurrences of each value using table()
#   6. Count values above the mean
#   7. Stop
#
# SAMPLE INPUT:
#   n = 20, mean = 5, sd = 2, set.seed(123)
#
# SAMPLE OUTPUT:
#    [1] 3.88 4.54 8.12 5.14 5.26 8.43 5.92 2.47 3.63 4.11 7.45 5.72 5.80 5.22 3.89
#   [16] 8.57 6.00 1.07 6.40 4.05
#
#   Rounded values: 4 5 8 5 5 8 6 2 4 4 7 6 6 5 4 9 6 1 6 4 
#
#   Occurrences of each value:
#   r
#   1 2 4 5 6 7 8 9 
#   1 1 5 4 5 1 2 1 
#
#   Numbers greater than the mean ( 5.28 ): 9
#
# ======================================================================
# PROGRAM
# ======================================================================

set.seed(123)
x <- rnorm(20, mean = 5, sd = 2)
print(round(x, 2))

r <- round(x)
cat("\nRounded values:", r, "\n")

cat("\nOccurrences of each value:\n")
print(table(r))

cat("\nNumbers greater than the mean (", round(mean(x), 2), "):", sum(x > mean(x)), "\n")

# ======================================================================
# RESULT:
#   Normal random numbers were generated and the count of each value was displayed successfully.
# ======================================================================

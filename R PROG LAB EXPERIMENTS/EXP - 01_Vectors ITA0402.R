# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 1
# NUMERIC, CHARACTER AND LOGICAL VECTORS
# ======================================================================
#
# AIM:
#   To create numeric, character and logical vectors and display their type and content.
#
# PSEUDOCODE:
#   1. Start
#   2. Create a numeric vector using c()
#   3. Create a character vector using c()
#   4. Create a logical vector using c()
#   5. Display each vector with print()
#   6. Display its type using typeof() and class()
#   7. Stop
#
# SAMPLE INPUT:
#   num_vec = 10, 20.5, 30, 40 | chr_vec = "R", "Python", "Java" | log_vec = TRUE, FALSE, TRUE
#
# SAMPLE OUTPUT:
#   Numeric vector   : 10 20.5 30 40 
#   Type: double | Class: numeric 
#
#   Character vector : R Python Java 
#   Type: character | Class: character 
#
#   Logical vector   : TRUE FALSE TRUE 
#   Type: logical | Class: logical
#
# ======================================================================
# PROGRAM
# ======================================================================

num_vec <- c(10, 20.5, 30, 40)
chr_vec <- c("R", "Python", "Java")
log_vec <- c(TRUE, FALSE, TRUE)

cat("Numeric vector   :", num_vec, "\n")
cat("Type:", typeof(num_vec), "| Class:", class(num_vec), "\n\n")

cat("Character vector :", chr_vec, "\n")
cat("Type:", typeof(chr_vec), "| Class:", class(chr_vec), "\n\n")

cat("Logical vector   :", log_vec, "\n")
cat("Type:", typeof(log_vec), "| Class:", class(log_vec), "\n")

# ======================================================================
# RESULT:
#   Numeric, character and logical vectors were created and their type and content were displayed successfully.
# ======================================================================

# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 6
# LIST WITH VECTORS, MATRICES AND FUNCTIONS
# ======================================================================
#
# AIM:
#   To create an R list containing a vector, a matrix and a function and display its contents.
#
# PSEUDOCODE:
#   1. Start
#   2. Create a vector, a matrix and a function
#   3. Combine them into a list using list()
#   4. Display the whole list
#   5. Access each element using $ and call the function
#   6. Stop
#
# SAMPLE INPUT:
#   Vector = 1:5 | Matrix = 2x2 (1:4) | Function = square(x) = x^2
#
# SAMPLE OUTPUT:
#   $vector
#   [1] 1 2 3 4 5
#
#   $matrix
#        [,1] [,2]
#   [1,]    1    3
#   [2,]    2    4
#
#   $func
#   function (x) 
#   {
#       return(x^2)
#   }
#
#
#   Vector element : 1 2 3 4 5 
#   Function call square(4) = 16
#
# ======================================================================
# PROGRAM
# ======================================================================

my_vec <- c(1, 2, 3, 4, 5)
my_mat <- matrix(1:4, nrow = 2)
square <- function(x) {
  return(x^2)
}

my_list <- list(vector = my_vec, matrix = my_mat, func = square)

print(my_list)

cat("\nVector element :", my_list$vector, "\n")
cat("Function call square(4) =", my_list$func(4), "\n")

# ======================================================================
# RESULT:
#   The list containing a vector, a matrix and a function was created and its contents displayed successfully.
# ======================================================================

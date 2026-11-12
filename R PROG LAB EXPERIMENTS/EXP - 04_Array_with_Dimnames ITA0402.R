# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 4
# ARRAY FROM VECTORS WITH DIMENSION NAMES
# ======================================================================
#
# AIM:
#   To create an array from vectors with dimension names and print specific elements.
#
# PSEUDOCODE:
#   1. Start
#   2. Create two vectors vec1 and vec2
#   3. Combine them into an array of dim 3 x 3 x 2 with dimnames
#   4. Print the full array
#   5. Print specific elements using [row, column, table]
#   6. Stop
#
# SAMPLE INPUT:
#   vec1 = 5, 9, 3 | vec2 = 10, 11, 12, 13, 14, 15
#
# SAMPLE OUTPUT:
#   , , Matrix1
#
#        Col1 Col2 Col3
#   Row1    5   10   13
#   Row2    9   11   14
#   Row3    3   12   15
#
#   , , Matrix2
#
#        Col1 Col2 Col3
#   Row1    5   10   13
#   Row2    9   11   14
#   Row3    3   12   15
#
#
#   Element at Row3, Col2 of Matrix1 : 12 
#   Element at Row1, Col3 of Matrix2 : 13 
#   [1] "Second matrix:"
#        Col1 Col2 Col3
#   Row1    5   10   13
#   Row2    9   11   14
#   Row3    3   12   15
#
# ======================================================================
# PROGRAM
# ======================================================================

vec1 <- c(5, 9, 3)
vec2 <- c(10, 11, 12, 13, 14, 15)

result <- array(c(vec1, vec2), dim = c(3, 3, 2),
                dimnames = list(c("Row1","Row2","Row3"),
                                c("Col1","Col2","Col3"),
                                c("Matrix1","Matrix2")))
print(result)

cat("\nElement at Row3, Col2 of Matrix1 :", result[3, 2, 1], "\n")
cat("Element at Row1, Col3 of Matrix2 :", result["Row1", "Col3", "Matrix2"], "\n")
print("Second matrix:")
print(result[, , 2])

# ======================================================================
# RESULT:
#   The array with dimension names was created and specific elements were printed successfully.
# ======================================================================

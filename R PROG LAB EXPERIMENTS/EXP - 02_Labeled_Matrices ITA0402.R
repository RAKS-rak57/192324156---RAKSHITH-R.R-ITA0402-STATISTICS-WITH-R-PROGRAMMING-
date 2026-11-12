# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 2
# LABELED MATRICES (5X4, 3X3, 2X2)
# ======================================================================
#
# AIM:
#   To create labeled matrices of size 5x4, 3x3 and 2x2 filled by row and by column.
#
# PSEUDOCODE:
#   1. Start
#   2. Create a 5x4 matrix from 1:20 with byrow = TRUE
#   3. Give row names and column names using dimnames
#   4. Create a 3x3 matrix from 1:9 with byrow = FALSE (column-wise)
#   5. Create a 2x2 matrix filled by row
#   6. Display all matrices
#   7. Stop
#
# SAMPLE INPUT:
#   Values 1:20 (5x4, by row), 1:9 (3x3, by column), c(5,10,15,20) (2x2, by row)
#
# SAMPLE OUTPUT:
#   [1] "5 x 4 matrix (filled by row):"
#      C1 C2 C3 C4
#   R1  1  2  3  4
#   R2  5  6  7  8
#   R3  9 10 11 12
#   R4 13 14 15 16
#   R5 17 18 19 20
#   [1] "3 x 3 matrix (filled by column):"
#     x y z
#   a 1 4 7
#   b 2 5 8
#   c 3 6 9
#   [1] "2 x 2 matrix (filled by row):"
#        Col1 Col2
#   Row1    5   10
#   Row2   15   20
#
# ======================================================================
# PROGRAM
# ======================================================================

# 5 x 4 matrix filled BY ROW
m1 <- matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE,
             dimnames = list(c("R1","R2","R3","R4","R5"),
                             c("C1","C2","C3","C4")))
print("5 x 4 matrix (filled by row):")
print(m1)

# 3 x 3 matrix filled BY COLUMN
m2 <- matrix(1:9, nrow = 3, ncol = 3, byrow = FALSE,
             dimnames = list(c("a","b","c"), c("x","y","z")))
print("3 x 3 matrix (filled by column):")
print(m2)

# 2 x 2 matrix filled BY ROW
m3 <- matrix(c(5, 10, 15, 20), nrow = 2, byrow = TRUE,
             dimnames = list(c("Row1","Row2"), c("Col1","Col2")))
print("2 x 2 matrix (filled by row):")
print(m3)

# ======================================================================
# RESULT:
#   Labeled 5x4, 3x3 and 2x2 matrices were created and displayed with row/column names successfully.
# ======================================================================

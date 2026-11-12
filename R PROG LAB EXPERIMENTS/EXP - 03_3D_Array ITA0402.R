# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 3
# 3D ARRAY
# ======================================================================
#
# AIM:
#   To create and display a 3-dimensional array with specified rows, columns and tables.
#
# PSEUDOCODE:
#   1. Start
#   2. Take the numbers 1 to 24
#   3. Create an array using array() with dim = c(rows, columns, tables) = c(4, 3, 2)
#   4. Display the array
#   5. Stop
#
# SAMPLE INPUT:
#   Data = 1:24, rows = 4, columns = 3, tables = 2
#
# SAMPLE OUTPUT:
#   , , 1
#
#        [,1] [,2] [,3]
#   [1,]    1    5    9
#   [2,]    2    6   10
#   [3,]    3    7   11
#   [4,]    4    8   12
#
#   , , 2
#
#        [,1] [,2] [,3]
#   [1,]   13   17   21
#   [2,]   14   18   22
#   [3,]   15   19   23
#   [4,]   16   20   24
#
#   Dimensions of the array: 4 3 2
#
# ======================================================================
# PROGRAM
# ======================================================================

arr <- array(1:24, dim = c(4, 3, 2))   # 4 rows, 3 columns, 2 tables
print(arr)
cat("Dimensions of the array:", dim(arr), "\n")

# ======================================================================
# RESULT:
#   A 3D array with 4 rows, 3 columns and 2 tables was created and displayed successfully.
# ======================================================================

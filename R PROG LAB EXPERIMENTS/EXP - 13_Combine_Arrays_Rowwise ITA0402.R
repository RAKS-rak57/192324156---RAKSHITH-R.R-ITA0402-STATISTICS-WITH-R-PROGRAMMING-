# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 13
# COMBINE MULTIPLE ARRAYS ROW-WISE
# ======================================================================
#
# AIM:
#   To combine multiple arrays row-wise.
#
# PSEUDOCODE:
#   1. Start
#   2. Create three arrays of the same number of columns
#   3. Combine them using rbind()
#   4. Display the combined array and its dimensions
#   5. Stop
#
# SAMPLE INPUT:
#   arr1 = 1:6, arr2 = 7:12, arr3 = 13:18 (each 2 x 3)
#
# SAMPLE OUTPUT:
#   [1] "Combined row-wise:"
#        [,1] [,2] [,3]
#   [1,]    1    3    5
#   [2,]    2    4    6
#   [3,]    7    9   11
#   [4,]    8   10   12
#   [5,]   13   15   17
#   [6,]   14   16   18
#   Dimensions: 6 3
#
# ======================================================================
# PROGRAM
# ======================================================================

arr1 <- array(1:6,   dim = c(2, 3))
arr2 <- array(7:12,  dim = c(2, 3))
arr3 <- array(13:18, dim = c(2, 3))

combined <- rbind(arr1, arr2, arr3)

print("Combined row-wise:")
print(combined)
cat("Dimensions:", dim(combined), "\n")

# ======================================================================
# RESULT:
#   Three arrays were combined row-wise into a 6 x 3 array successfully.
# ======================================================================

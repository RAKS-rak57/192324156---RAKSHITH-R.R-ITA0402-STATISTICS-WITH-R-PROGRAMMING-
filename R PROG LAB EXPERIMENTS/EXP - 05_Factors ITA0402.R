# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 5
# FACTOR VARIABLES
# ======================================================================
#
# AIM:
#   To create and manipulate factor variables using the women dataset heights and a random sample of LETTERS.
#
# PSEUDOCODE:
#   1. Start
#   2. Convert women$height into a factor and display levels
#   3. Find number of levels and summary of the factor
#   4. Set a seed and take a random sample of 10 LETTERS
#   5. Convert the sample into a factor and tabulate it
#   6. Create an ordered factor and compare levels
#   7. Stop
#
# SAMPLE INPUT:
#   Dataset: women (built-in) | sample(LETTERS, 10, replace = TRUE) with set.seed(1)
#
# SAMPLE OUTPUT:
#    [1] "58" "59" "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "70" "71" "72"
#   Number of levels: 15 
#   58 59 60 61 62 
#    1  1  1  1  1 
#    [1] "Y" "D" "G" "A" "B" "W" "K" "N" "R" "S"
#    [1] Y D G A B W K N R S
#   Levels: A B D G K N R S W Y
#   f
#   A B D G K N R S W Y 
#   1 1 1 1 1 1 1 1 1 1 
#   [1] Low    High   Medium Low   
#   Levels: Low < Medium < High
#   [1] TRUE
#
# ======================================================================
# PROGRAM
# ======================================================================

# Part 1: women dataset
h <- factor(women$height)
print(levels(h))
cat("Number of levels:", nlevels(h), "\n")
print(summary(h)[1:5])

# Part 2: random sample of LETTERS
set.seed(1)
s <- sample(LETTERS, 10, replace = TRUE)
f <- factor(s)
print(s)
print(f)
print(table(f))

# Part 3: ordered factor
size <- factor(c("Low", "High", "Medium", "Low"),
               levels = c("Low", "Medium", "High"), ordered = TRUE)
print(size)
print(size[2] > size[1])

# ======================================================================
# RESULT:
#   Factor variables were created from the women dataset and a random LETTERS sample, and manipulated successfully.
# ======================================================================

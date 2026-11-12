# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 11
# READ A CSV FILE
# ======================================================================
#
# AIM:
#   To read a .csv file and display its contents.
#
# PSEUDOCODE:
#   1. Start
#   2. Create a sample CSV file students.csv (so the program is self-contained)
#   3. Read the file using read.csv()
#   4. Display the contents, structure and dimensions
#   5. Stop
#
# SAMPLE INPUT:
#   File: students.csv (id, name, marks)
#
# SAMPLE OUTPUT:
#     id   name marks
#   1  1   Arun    85
#   2  2   Bala    72
#   3  3 Charan    90
#   4  4  Divya    66
#   5  5   Esha    78
#   'data.frame':	5 obs. of  3 variables:
#    $ id   : int  1 2 3 4 5
#    $ name : chr  "Arun" "Bala" "Charan" "Divya" ...
#    $ marks: int  85 72 90 66 78
#   Rows: 5  Columns: 3
#
# ======================================================================
# PROGRAM
# ======================================================================

# Create a sample CSV file
students <- data.frame(id = 1:5,
                       name = c("Arun", "Bala", "Charan", "Divya", "Esha"),
                       marks = c(85, 72, 90, 66, 78))
write.csv(students, "students.csv", row.names = FALSE)

# Read the CSV file
data <- read.csv("students.csv")

print(data)
str(data)
cat("Rows:", nrow(data), " Columns:", ncol(data), "\n")

# ======================================================================
# RESULT:
#   The CSV file was read and its contents were displayed successfully.
# ======================================================================

# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 10
# DATA FRAME EXAM_DATA
# ======================================================================
#
# AIM:
#   To create and explore a data frame exam_data and perform extract, add row/column, sort and save to file.
#
# PSEUDOCODE:
#   1. Start
#   2. Create data frame with name, score, attempts, qualify
#   3. Display structure and summary
#   4. Extract first two rows and selected columns
#   5. Add a new row using rbind() and a new column using $
#   6. Sort by score using order()
#   7. Save the data frame to CSV using write.csv()
#   8. Stop
#
# SAMPLE INPUT:
#   10 students with name, score, attempts and qualify (yes/no)
#
# SAMPLE OUTPUT:
#           name score attempts qualify
#   1  Anastasia  12.5        1     yes
#   2       Dima   9.0        3      no
#   3  Katherine  16.5        2     yes
#   4      James    NA        3      no
#   5      Emily   9.0        2      no
#   6    Michael  20.0        3     yes
#   7    Matthew  14.5        1     yes
#   8      Laura  13.5        1      no
#   9      Kevin   8.0        2      no
#   10     Jonas  19.0        1     yes
#   'data.frame':	10 obs. of  4 variables:
#    $ name    : chr  "Anastasia" "Dima" "Katherine" "James" ...
#    $ score   : num  12.5 9 16.5 NA 9 20 14.5 13.5 8 19
#    $ attempts: num  1 3 2 3 2 3 1 1 2 1
#    $ qualify : chr  "yes" "no" "yes" "no" ...
#
#   First two rows:
#          name score attempts qualify
#   1 Anastasia  12.5        1     yes
#   2      Dima   9.0        3      no
#
#   Name and score columns:
#           name score
#   1  Anastasia  12.5
#   2       Dima   9.0
#   3  Katherine  16.5
#   4      James    NA
#   5      Emily   9.0
#   6    Michael  20.0
#   ... (output truncated here, full output appears in R console)
#
# ======================================================================
# PROGRAM
# ======================================================================

exam_data <- data.frame(
  name = c("Anastasia", "Dima", "Katherine", "James", "Emily",
           "Michael", "Matthew", "Laura", "Kevin", "Jonas"),
  score = c(12.5, 9, 16.5, NA, 9, 20, 14.5, 13.5, 8, 19),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c("yes", "no", "yes", "no", "no", "yes", "yes", "no", "no", "yes"),
  stringsAsFactors = FALSE
)

print(exam_data)
str(exam_data)

# Extract
cat("\nFirst two rows:\n");           print(exam_data[1:2, ])
cat("\nName and score columns:\n");   print(exam_data[, c("name", "score")])

# Add a new row
new_row <- data.frame(name = "Suresh", score = 15, attempts = 1, qualify = "yes")
exam_data <- rbind(exam_data, new_row)

# Add a new column
exam_data$country <- "India"
cat("\nAfter adding row and column:\n"); print(exam_data)

# Sort by score (descending)
sorted_data <- exam_data[order(-exam_data$score), ]
cat("\nSorted by score:\n"); print(sorted_data)

# Save to file
write.csv(exam_data, "exam_data.csv", row.names = FALSE)
cat("\nFile exam_data.csv saved in:", getwd(), "\n")

# ======================================================================
# RESULT:
#   The exam_data data frame was created, extracted, modified, sorted and saved to exam_data.csv successfully.
# ======================================================================

# ======================================================================
# ITA04 STATISTICS WITH R PROGRAMMING - LAB EXPERIMENT 7
# FACTORS OF A NUMBER, RANDOM INTEGERS AND FIZZBUZZ
# ======================================================================
#
# AIM:
#   To write R programs for (a) factors of a number, (b) 10 random integers between -50 and 50, (c) FizzBuzz for 1 to 100.
#
# PSEUDOCODE:
#   1. Start
#   2. (a) Read n; for i = 1 to n, if n %% i == 0 then print i
#   3. (b) Use sample(-50:50, 10) to generate random integers
#   4. (c) For i = 1 to 100: if i divisible by 15 print FizzBuzz, else if by 3 print Fizz, else if by 5 print Buzz, else print i
#   5. Stop
#
# SAMPLE INPUT:
#   n = 24 | set.seed(10) for reproducible random numbers
#
# SAMPLE OUTPUT:
#   Factors of 24 are: 1 2 3 4 6 8 12 24 
#
#   Random integers: -42 23 25 4 21 3 -12 32 37 -36 
#
#   [1] 1
#   [1] 2
#   [1] "Fizz"
#   [1] 4
#   [1] "Buzz"
#   [1] "Fizz"
#   [1] 7
#   [1] 8
#   [1] "Fizz"
#   [1] "Buzz"
#   [1] 11
#   [1] "Fizz"
#   [1] 13
#   [1] 14
#   [1] "FizzBuzz"
#   [1] 16
#   [1] 17
#   [1] "Fizz"
#   [1] 19
#   [1] "Buzz"
#   [1] "Fizz"
#   ... (output truncated here, full output appears in R console)
#
# ======================================================================
# PROGRAM
# ======================================================================

# (a) Factors of a number
n <- 24
cat("Factors of", n, "are: ")
for (i in 1:n) {
  if (n %% i == 0) cat(i, "")
}
cat("\n\n")

# (b) 10 random integers between -50 and 50
set.seed(10)
rand_vec <- sample(-50:50, 10, replace = TRUE)
cat("Random integers:", rand_vec, "\n\n")

# (c) FizzBuzz 1 to 100
for (i in 1:100) {
  if (i %% 15 == 0) {
    print("FizzBuzz")
  } else if (i %% 3 == 0) {
    print("Fizz")
  } else if (i %% 5 == 0) {
    print("Buzz")
  } else {
    print(i)
  }
}

# ======================================================================
# RESULT:
#   Factors of 24, 10 random integers in the range -50 to 50, and the FizzBuzz series 1-100 were generated successfully.
# ======================================================================

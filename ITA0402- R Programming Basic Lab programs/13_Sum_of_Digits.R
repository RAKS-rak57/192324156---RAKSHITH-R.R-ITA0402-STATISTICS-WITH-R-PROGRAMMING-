# Program 13: Sum of digits
n <- as.integer(readline("Enter a number: "))

total <- 0
while (n > 0) {
  digit <- n %% 10
  total <- total + digit
  n <- n %/% 10
}

print(paste("Sum of digits =", total))

# Program 5: Factorial
n <- as.integer(readline("Enter a number: "))

fact <- 1
for (i in seq_len(n)) {
  fact <- fact * i
}

print(paste("Factorial =", fact))

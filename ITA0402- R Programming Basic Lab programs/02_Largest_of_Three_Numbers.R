# Program 2: Largest of three numbers
a <- as.numeric(readline("Enter first number: "))
b <- as.numeric(readline("Enter second number: "))
d <- as.numeric(readline("Enter third number: "))

if (a >= b && a >= d) {
  largest <- a
} else if (b >= a && b >= d) {
  largest <- b
} else {
  largest <- d
}

print(paste("Largest =", largest))

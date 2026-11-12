# Program 4: Multiplication table
n <- as.integer(readline("Enter a number: "))

for (i in 1:10) {
  print(paste(n, "x", i, "=", n * i))
}

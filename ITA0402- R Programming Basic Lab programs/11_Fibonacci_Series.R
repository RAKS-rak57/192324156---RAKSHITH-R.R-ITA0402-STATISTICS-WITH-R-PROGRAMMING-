# Program 11: Fibonacci series
n <- as.integer(readline("Enter number of terms: "))

a <- 0
b <- 1

for (i in 1:n) {
  print(a)
  nxt <- a + b   # note: 'next' is a reserved word in R, so we use 'nxt'
  a <- b
  b <- nxt
}

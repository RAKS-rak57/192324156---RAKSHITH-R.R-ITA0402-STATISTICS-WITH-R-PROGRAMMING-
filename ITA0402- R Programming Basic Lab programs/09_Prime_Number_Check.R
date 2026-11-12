# Program 9: Prime number check
n <- as.integer(readline("Enter a number: "))

count <- 0
if (n > 1) {
  for (i in 1:n) {
    if (n %% i == 0) {
      count <- count + 1
    }
  }
}

if (count == 2) {
  print("Prime Number")
} else {
  print("Not a Prime Number")
}

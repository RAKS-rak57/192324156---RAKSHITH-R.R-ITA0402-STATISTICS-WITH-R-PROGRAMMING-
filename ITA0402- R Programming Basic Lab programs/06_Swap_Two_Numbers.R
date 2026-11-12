# Program 6: Swap two numbers (using a temporary variable)
a <- as.numeric(readline("Enter a: "))
b <- as.numeric(readline("Enter b: "))

temp <- a
a <- b
b <- temp

print(paste("After swapping, a =", a))
print(paste("After swapping, b =", b))

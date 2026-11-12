# Program 12: GCD and LCM
a <- as.integer(readline("Enter first number: "))
b <- as.integer(readline("Enter second number: "))

x <- a
y <- b

while (y != 0) {
  r <- x %% y
  x <- y
  y <- r
}

gcd <- x
lcm <- abs(a * b) / gcd

print(paste("GCD =", gcd))
print(paste("LCM =", lcm))

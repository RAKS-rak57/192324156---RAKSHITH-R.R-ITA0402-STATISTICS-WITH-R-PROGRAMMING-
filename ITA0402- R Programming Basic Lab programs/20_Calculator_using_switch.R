# Program 20: Calculator using switch()
a <- as.numeric(readline("Enter first number: "))
b <- as.numeric(readline("Enter second number: "))
op <- readline("Enter operation (+, -, *, /): ")

result <- switch(op,
                 "+" = a + b,
                 "-" = a - b,
                 "*" = a * b,
                 "/" = a / b,
                 "Invalid operator")

print(paste("Result =", result))

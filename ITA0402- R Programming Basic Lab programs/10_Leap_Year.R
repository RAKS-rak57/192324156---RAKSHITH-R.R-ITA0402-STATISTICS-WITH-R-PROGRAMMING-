# Program 10: Leap year
year <- as.integer(readline("Enter year: "))

if ((year %% 400 == 0) || (year %% 4 == 0 && year %% 100 != 0)) {
  print("Leap Year")
} else {
  print("Not a Leap Year")
}

# Program 7: Palindrome (string)
str <- readline("Enter a string: ")

rev_str <- paste(rev(strsplit(str, "")[[1]]), collapse = "")

if (str == rev_str) {
  print("Palindrome")
} else {
  print("Not Palindrome")
}

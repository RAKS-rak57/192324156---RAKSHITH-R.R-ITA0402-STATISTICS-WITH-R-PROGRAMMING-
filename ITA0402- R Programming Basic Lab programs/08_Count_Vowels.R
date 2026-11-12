# Program 8: Count vowels
str <- readline("Enter a string: ")

chars <- strsplit(tolower(str), "")[[1]]

count <- 0
for (ch in chars) {
  if (ch %in% c("a", "e", "i", "o", "u")) {
    count <- count + 1
  }
}

print(paste("Number of vowels =", count))

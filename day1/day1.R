library(data.table)

data <- fread("day1/input.txt", header = FALSE)$V1

# PART 1

numbers <- gsub("\\D", "", data)

numbers <- sapply(numbers, function(number) {
  as.integer(
    paste0(
      substr(number, 1, 1), 
      substr(number, nchar(number), nchar(number))))
})

print(paste("Part 1: The sum of all of the calibration values is:", sum(numbers)))

# PART 2

spelled_digits <- c(as.character(c(1:9)), c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"))

getmatch <- function(datastring, start) {
  i <- ifelse(start == "first", 1, nchar(datastring))
  
  foundmatch <- NA
  
  while (is.na(foundmatch)) {
    
    matches <- sapply(spelled_digits, function(digit) {
      grepl(
        paste0("^", digit), 
        substr(datastring, i, nchar(datastring)))
    })
    
    if (sum(matches) == 0) {
      if (start == "first") {
        i <- i + 1
      } else {
        i <- i - 1
      }
    } else {
      m <- matches[matches]
      
      foundmatch <- as.integer(names(matches[matches]))
      if (is.na(foundmatch)) {
        foundmatch <- which(matches)-9
      } 
    }
  }
  return(foundmatch)
}

numbers <- sapply(data, function(datastring) {
  return(
    as.integer(
      paste0(
        getmatch(datastring = datastring, start = "first"),
        getmatch(datastring = datastring, start = "last"))))
})

print(paste("Part 2: The sum of all of the calibration values is:", sum(numbers)))


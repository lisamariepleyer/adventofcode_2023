#!/usr/bin/env Rscript

data <- read.delim("day2/input.txt", header = FALSE)$V1

colours <- c(12, 13, 14)
names(colours) <- c("red", "green", "blue")

# PART 1

gameID <- 1:length(data)

is_too_many <- sapply(1:length(data), function(i){
  data_i_split <- strsplit(data[i], split = " ")[[1]]
  
  is_too_many <- sapply(1:length(colours), function(col_i) {
    which(as.numeric(data_i_split[grep(names(colours[col_i]), data_i_split)-1]) > colours[col_i])
  })
  
  return(ifelse(length(unlist(is_too_many)) > 0, T, F))
})

print(paste("Part 1: The sum of the IDs of all possible games is:", sum(gameID[is_too_many == F])))

# PART 2

prod_max_per_pull <- sapply(1:length(data), function(i){
  data_i_split <- strsplit(data[i], split = " ")[[1]]
  
  max_per_pull <- sapply(1:length(colours), function(col_i) {
    max(as.numeric(data_i_split[grep(names(colours[col_i]), data_i_split)-1]))
  })
  
  return(prod(max_per_pull))
})

print(paste("Part 2: The sum of the IDs of all possible games is:", sum(prod_max_per_pull)))


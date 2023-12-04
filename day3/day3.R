#!/usr/bin/env Rscript

data <- read.table("day3/input.txt", header = FALSE, comment.char = "")$V1

data_matrix <- matrix(unlist(
  lapply(data, function(s) {
    strsplit(s, split="")[[1]]
  })), ncol = nchar(data[1]), byrow = T)

# find all possible symbols
symbols <- unique(strsplit(paste(data, collapse = ""), split = "")[[1]])
symbols <- symbols[grep("\\D", symbols)]
symbols <- symbols[symbols != "."]

# function to find all indices in matrix surrounding match with
# row_i = match row; n_length = match length; n_match = position of match in row
get_surrounding_matrix_indices <- function(row_i, n_length, n_match) {
  if (row_i == 1) {
    close_rows <- c(row_i+1)
  } else if (row_i == nrow(data_matrix)) {
    close_rows <- c(row_i-1)
  } else {
    close_rows <- c((row_i-1), (row_i+1))
  }
  
  if (n_match == 1) {
    close_columns_i <- c(n_match+n_length)
    close_columns <- c(n_match:(n_match+n_length))
  } else if (n_match+n_length > ncol(data_matrix)) {
    close_columns_i <- c(n_match-1)
    close_columns <- c((n_match-1):(n_match+n_length-1))
  } else {
    close_columns_i <- c(n_match-1, (n_match+n_length))
    close_columns <- c((n_match-1):(n_match+n_length))
  }
  
  out <- list("surr_rows" = close_rows,
              "surr_cols" = close_columns,
              "surr_cols_i" = close_columns_i)
  return(out)
}

# PART 1

numbers_touching_symbols <- c()

# loop over rows and extract unique numbers per row
for (row_i in 1:nrow(data_matrix)) {
  data_row_i <- data[row_i]
  numbers <- unique(
    na.omit(
      as.numeric(
        strsplit(
          gsub("\\D", " ", data_row_i), " ")[[1]])))
  
  # loop over numbers per row, find matches per row per number
  for (n in numbers) {
    data_number_matches <- gregexpr(n, data_row_i)[[1]]
    symbol_matches <- c()
    
    # loop over matches per number per row and if there is a symbol in the surrounding matrix, add to vector
    for (dnm in data_number_matches) {
      
      sm <- get_surrounding_matrix_indices(row_i = row_i, n_length = nchar(n), n_match = dnm)
      
      # make sure the match actually matches the number and isn't just part of a number with more digits
      if(length(grep("\\d", data_matrix[row_i, sm[["surr_cols_i"]]])) == 0) {
        surroundings <- c(data_matrix[row_i, sm[["surr_cols_i"]]], 
                          c(data_matrix[sm[["surr_rows"]], sm[["surr_cols"]]]))
        
        symbol_matches <- c(symbol_matches, 
                            length(unlist(
                              lapply(symbols, function(symb) {
                                which(surroundings == symb)
                            }))))
      }
    }
    
    numbers_touching_symbols <- c(numbers_touching_symbols, rep(n, sum(symbol_matches)))
  }
}

print(paste("Part 1: The sum of all part numbers is:", sum(numbers_touching_symbols)))

# PART 2

data_matrix_numbers <- data_matrix

# loop over rows and extract numbers per row and positions of numbers per row
for (row_i in 1:nrow(data_matrix)) {
  data_row_i <- data[row_i]
  numbers <- na.omit(
    as.numeric(
      strsplit(
        gsub("\\D", " ", data_row_i), " ")[[1]]))
  number_pos <- gregexpr("\\d", data_row_i)[[1]]
  
  # replace single digits in matrix with actual number value
  # "." "2" "5" "." will become "." "25" "25" "."
  start <- 1
  for (n in numbers) {
    end <- start + nchar(n) - 1
    
    pos <- number_pos[start:end]
    data_matrix_numbers[row_i, pos] <- n
    
    start <- start + nchar(n)
  }
}

products_of_numbers_touching_starts <- c()

# loop over rows and extract positions of asterisks per row
for (row_i in 1:nrow(data_matrix)) {
  data_row_i <- data[row_i]
  star_positions <- gregexpr("\\*", data_row_i)[[1]]
  
  # make sure there are matches
  if (star_positions[1] != -1) {
    
    # loop over asterisks per row and if there is are two numbers in the surrounding matrix calculate product and add to vector
    for (sp in star_positions) {
      
      sm <- get_surrounding_matrix_indices(row_i = row_i, n_length = 1, n_match = sp)
      
      surroundings <- suppressWarnings(
        unique(
          na.omit(
            as.numeric(
              c(
                data_matrix_numbers[row_i, sm[["surr_cols_i"]]],
                c(data_matrix_numbers[sm[["surr_rows"]], sm[["surr_cols"]]]))))))
      
      if (length(surroundings) == 2) {
        products_of_numbers_touching_starts <- c(products_of_numbers_touching_starts, prod(surroundings))
      }
    }
  }
}

print(paste("Part 2: The sum of all gear ratios is:", sum(products_of_numbers_touching_starts)))

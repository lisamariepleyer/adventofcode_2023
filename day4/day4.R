#!/usr/bin/env Rscript

data <- readLines("day4/input.txt")

data_list <- lapply(1: length(data), function(i) {
  winningnumbers <- na.omit(as.numeric(strsplit(strsplit(strsplit(data[i], split = ":")[[1]][2], split = "\\|")[[1]][1], split = " ")[[1]]))
  mynumbers <- na.omit(as.numeric(strsplit(strsplit(data[i], split = "\\|")[[1]][2], split = " ")[[1]]))
  
  no_matches <- sum(sapply(winningnumbers, function(wn) {
    length(which(mynumbers == wn))
  }))
  
  return(list(no_matches = no_matches,
              points = 0,
              no_cards = 1))
})


# PART 1

for (i in 1:length(data)) {
  matches <- data_list[[i]]$no_matches
  
  points <- 0
  counter <- 1
  
  while (matches != 0) {
    if (counter == 1) {
      points <- points + 1
    }
    
    if (counter == 2) {
      points <- points + 1
    }
    
    if (counter >= 3) {
      points <- points * 2
    }
    
    matches <- matches - 1
    counter <- counter + 1
  }
  
  data_list[[i]]$points <- points
}

print(paste("Part 1: The cards are worth", sum(sapply(data_list, `[[`, "points")), "points."))

# PART 2

for (i in 1:(length(data)-1)) {
  if (data_list[[i]]$no_matches > 0) {
    card_copies <- (i+1):(i+data_list[[i]]$no_matches)
    card_copies <- rep(card_copies, data_list[[i]]$no_cards)
    
    for (cc in card_copies) {
      data_list[[cc]]$no_cards <- data_list[[cc]]$no_cards + 1
    }
  }
}

print(paste("Part 1: The is a total of", sum(sapply(data_list, `[[`, "no_cards")), "cards."))


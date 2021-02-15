
# Function Contents -----------------------------------------------------------
# Internal:
#   check: checks lineup and data frame input of player probabilities

# Check -----------------------------------------------------------------------
# Acts on lineup and data frame input of player probabilities
# Lineup must be single player (to be repeated) or nine players
# Matches name in lineup to names in stats (or uses player number)
# Checks that probabilities are valid
# Fills in fast player if not given
# Returns a 9-row data frame of stats in lineup order

check <- function(lineup, stats) {
  
  # uppercase column names in stats
  colnames(stats) <- toupper(colnames(stats))
  
  # check length of lineup
  if (length(lineup) == 1) {
    lineup <- rep(lineup, 9)
  } else if (length(lineup) != 9) {
    stop("Lineup must be of length 1 or 9")
  }
  
  # match lineup (either character or numeric)
  if (is.character(lineup)) { # match based on player name
    
    # check that stats has a name column
    if (!("NAME" %in% colnames(stats))) stop("stats must have a 'NAME' column")
    
    # convert name to character if not
    if (!is.character(stats$NAME)) stats$NAME <- as.character(stats$NAME)
    
    # match arguments
    lineup <- sapply(lineup, match.arg, choices = stats$NAME)
    
    # generate player_index
    player_index <- vector(length = 9)
    for (i in 1:9) player_index[i] <- which(stats$NAME == lineup[i])
    
  } else if (is.numeric(lineup)) { # match based on player number
    
    # check tht stats has a number column
    if (!("NUMBER" %in% colnames(stats))) stop("stats must have a 'NUMBER' column")
    
    # convert number to numeric if not
    if (!is.numeric(stats$NUMBER)) stats$NUMBER <- as.numeric(stats$NUMBER)
    
    # generate player_index
    player_index <- vector(length = 9)
    for (i in 1:9) player_index[i] <- which(stats$NUMBER == lineup[i])
    
  } else stop("Lineup must be either character or numeric")
  
  # check that each necessary column is present
  outcomes <- c("O", "S", "D", "TR", "HR", "W")
  for (i in 1:length(outcomes))
    if (!(outcomes[i] %in% colnames(stats))) stop(paste0("Missing column '", 
                                                         outcomes[i], "'"))
  
  # SBA and SB warning
  if ("SBA" %in% colnames(stats)) { # steal probabilities provided
    if (!("SB" %in% colnames(stats))) # success probabilities not provided
      stop("Probability of successful steal 'SB' must be specified")
  } else { # steal probabilities not provided
    stats$SBA <- rep(0, nrow(stats))
    stats$SB <- rep(0, nrow(stats))
    warning("'SBA' not specified, assigned probability zero")
  }
  
  # is SBA is zero and SB is NA, replace with zero
  for (i in 1:nrow(stats)) {
    if ((stats$SBA[i] == 0) && is.na(stats$SB[i])) stats$SB[i] <- 0
  }
  
  # check that all probabilities are positive
  if (!all(stats[, c("O", "S", "D", "TR", "HR", "W", "SBA", "SB")] >= 0))
    stop("All probabilities must be greater than or equal to zero")
  
  # check that probabilities of main six outcomes sum to one
  if (!all(rowSums(stats[, c("O", "S", "D", "TR", "HR", "W")]) > 0.99) | 
      !all(rowSums(stats[, c("O", "S", "D", "TR", "HR", "W")]) < 1.01))
    stop("Sum of probabilities 'O', 'S', 'D', 'TR', 'HR', and 'W' must equal one")
  
  # if fast player column is present, check that is is true or false
  # if fast player column is absent, create it based on SBA
  if ("FAST" %in% colnames(stats)) {
    if (!is.logical(stats$FAST)) stop("'FAST' column must be logical")
  } else {
    stats$FAST <- stats$SBA > 0.5
    warning("Fast players not specified, assigned using SBA probability with
            threshold 0.5")
  }
    
  # return cleaned stats with rows in the proper order (1 - 9 based on lineup)
  return(stats[player_index, ])
}

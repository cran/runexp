
# Contents --------------------------------------------------------------------
# Internal Data
#     base_states: vector of base states (in order)
#     out_states: vector of out states (in order)
#     fast_B: transition matrix for fast batter
#     slow_B: transition matrix for slow batter
#     fast_S: transition matrix for fast stealer
#     slow_S: transition matrix for slow stealer
# Internal Functions
#     decompose: splits transition matrix based on outcome and score
# External Functions
#     chain (see documentation below)
#     plot.chain (see documentation below)

# Imported Functions ----------------------------------------------------------
#' @importFrom stringr str_split
#' @importFrom graphics abline axis matplot par plot rect segments text

# Code to save "rda" file -----------------------------------------------------

# setwd("../../markov_chain")
# source('functions.R')
# slow_B <- data.frame(read_excel("transition_matrix.xlsx", sheet = 'slow_batter'))
# slow_S <- data.frame(read_excel("transition_matrix.xlsx", sheet = 'slow_stealer'))
# fast_B <- data.frame(read_excel("transition_matrix.xlsx", sheet = 'fast_batter'))
# fast_S <- data.frame(read_excel("transition_matrix.xlsx", sheet = 'fast_stealer'))

# slow_B <- clean_matrix(slow_B)
# slow_S <- clean_matrix(slow_S)
# fast_B <- clean_matrix(fast_B)
# fast_S <- clean_matrix(fast_S)

# base_states <- colnames(fast_B)
# out_states <- c(rep("0 outs", 18), rep("1 out", 18), rep("2 outs", 18), "3 outs")

# setwd("../runexp/R")
# save(slow_B, slow_S, fast_B, fast_S, base_states, out_states, file = "sysdata.rda")

# Decompose Matrix ------------------------------------------------------------
# Splits a transition matrix based on outcome and score
# Max of two outcomes per transition

decompose <- function(A) {
  
  k <- length(base_states)
  
  # Split matrix on plus sign (two possible outcomes from one transition)
  A1 <- matrix(NA, nrow = k, ncol = k) # first possible play outcome
  A2 <- matrix(NA, nrow = k, ncol = k) # second possible play outcome
  for (i in 1:k) {
    for (j in 1:k) {
      text_split <- str_split(A[i,j], '\\+')[[1]]
      text_split <- gsub(" ", "", text_split, fixed = TRUE)
      A1[i, j] <- text_split[1]
      A2[i, j] <- text_split[2]
    }
  }
  
  # Split A matrix into outcome and runs scored
  O1 <- matrix(NA, nrow = k, ncol = k) # first outcome
  R1 <- matrix(NA, nrow = k, ncol = k) # first runs scored 
  O2 <- matrix(NA, nrow = k, ncol = k) # second outcome
  R2 <- matrix(NA, nrow = k, ncol = k) # second runs scored 
  for (i in 1:k) {
    for (j in 1:k) {
      text_split <- str_split(A1[i,j], '\\(|\\)')[[1]]
      text_split <- gsub(" ", "", text_split, fixed = TRUE)
      O1[i, j] <- text_split[1]
      R1[i, j] <- text_split[2]
      text_split <- str_split(A2[i,j], '\\(|\\)')[[1]]
      text_split <- gsub(" ", "", text_split, fixed = TRUE)
      O2[i, j] <- text_split[1]
      R2[i, j] <- text_split[2]
    }
  }
  
  # Replace NA entries with zero (zero probability or zero runs scored)
  O1[is.na(O1)] <- 0
  O2[is.na(O2)] <- 0
  R1[is.na(R1)] <- 0
  R2[is.na(R2)] <- 0
  
  # Convert runs scored to numeric
  R1 <- matrix(sapply(R1, as.numeric), nrow = k, ncol = k)
  R2 <- matrix(sapply(R2, as.numeric), nrow = k, ncol = k)
  
  return(list(O1 = O1, O2 = O2, R1 = R1, R2 = R2))
}

#  Chain Function -------------------------------------------------------------
#' @title Softball run expectancy using discrete Markov chains
#' @description Uses discrete Markov chains to calculate softball run 
#'     expectancy for a single (half) inning.  Calculations depend on specified player
#'     probabilities (see details) and a nine-player lineup.  Optionally 
#'     incorporates attempted steals and "fast" players who are able to strech bases.
#'     
#' @details The typical state space for softball involves 25 states 
#'     defined by the base situation (runners on base) and number of outs.  The
#'     standard base situations are: (1) bases empty, (2) runner on first, (3) runner 
#'     on second, (4) runner on third, (5) runners on first and second, (6) runners 
#'     on second and third, (7) runners on first and third, and (8) bases loaded.
#'     These 8 states are crossed with each of three out states (0 outs, 1 out, or 
#'     2 outs) to form 24 states.  The final 25th state is the 3 outs that marks
#'     the end of an inning.
#'     
#'     We expand these 25 states to incorporate "fast" players.  We make the following 
#'     assumptions concerning fast players:
#'     \itemize{
#'         \item If a fast player is on first and the batter hits a single, the fast
#'         player will stretch to third base (leaving the batter on first).
#'         \item If a fast player is on second and the batter hits a single, the fast
#'         player will stretch home (leaving the batter on first and a single run scored).
#'         \item If a fast player is on first and the batter hits a double, the fast
#'         player will stretch home (leaving the batter on second base and a single run scored).
#'         \item A typical player (not fast) who successfully steals a base will become 
#'         a fast player for the remainder of that inning (meaning that a player 
#'         who successfully steals second base will stretch home on a single).
#'     }
#'     Based on these assumptions, we add base situations that designate runners on first
#'     and second base as either typical runners (R) or fast runners (F).  The entirety 
#'     of these base situations can be viewed using \code{plot.chain} with \code{fast = TRUE}.
#'     Aside from these fast player assumptions, runners advance bases as expected (a single
#'     advances each runner one base, a double advances each runner two bases, etc.).
#'     
#'     
#'     Each at bat results in a change to the base situation and/or the number of outs.  The 
#'     outcomes of an at-bat are limited to:
#'     \itemize{
#'         \item batter out (O): base state does not change, outs increase by one
#'         \item single (S): runners advance accordingly, score may increase, outs do not change
#'         \item double (D): runners advance accordingly, score may increase, outs do not change
#'         \item triple (TR): runners advance accordingly, score may increase, outs do not change
#'         \item homerun (HR): bases cleared, score increases accordingly, outs do not change
#'         \item walk (W): runners advance accordingly, score may increase, outs do not change
#'     }
#'     The transitions resulting from these outcomes are stored in "transition matrices."  We 
#'     utilize separate transition matrices for typical batters and fast batters (in order to 
#'     keep fast runners designated separately).  We additionally incorporate stolen bases.
#'     Steals are handled separately than the six at-bat outcomes because they do not result 
#'     in changes to the batter.  Following softball norms, we only entertain steals of second
#'     base.  Steals are considered in cases when there is a runner on first and no runner on second.
#'     In this situation, steal possibilities are limited to:
#'     \itemize{
#'         \item no steal attempt: base situation and outs do not change
#'         \item successful steal: runner advances to second base
#'         \item caught steal: runner is removed, outs increase by one
#'     }
#'     Steal possibilities are implemented in separate transition matrices.  All transition 
#'     matrices are stored as internal RData files.
#'      
#'     The \code{stats} input must be a data frame containing player probabilities.  It must 
#'     contain columns "O", "S", "D", "TR", "HR", and "W" whose entries are probabilities summing
#'     to one, corresponding to the probability of a player's at-bat resulting in each outcome.
#'     The data frame must contain either a "NAME" or "NUMBER" column to identify players (these
#'     must correspond to the \code{lineup}).  Extra rows for players not in the lineup will be ignored.
#'     This data frame may be generated from player statistics using \code{prob_calc}.
#'     
#'     The \code{stats} data frame may optionally include an "SBA" (stolen base attempt) column
#'     that provides the probability a given player will attempt a steal (provided they are on first
#'     base with no runner on second).  If "SBA" is specified, the data frame must also include 
#'     a "SB" (stolen base) column that provides the probability of a given player successfully
#'     stealing a base (conditional on them attempting a steal).  If these probabilities are not 
#'     specified, calculations will not involve any steals.
#'     
#'     The \code{stats} data frame may also include a logical "FAST" column that indicates
#'     whether a player is fast.  If this column is not specified, the "FAST" designation
#'     will be assigned based on each player's "SBA" probability.  Generally, players who are more 
#'     likely to attempt steals are the fast players.
#'     
#'     The \code{cycle} parameter is a useful tool for evaluating an entire lineup.  Through the course 
#'     of a game, any of the nine players may lead-off an inning.  A weighted or un-weighted average 
#'     of these nine expected scores provides a more holistic representation of the lineup than 
#'     the expected score based on a single lead-off.
#'     
#' @param lineup either character vector of player names or numeric vector
#'        of player numbers.  Must be of length 1 or 9.  If lineup is of length 1, the 
#'        single player will be "copied" nine times to form a complete lineup.  
#' @param stats data frame of player statistics (see details)
#' @param cycle logical indicating whether to calculate run expectancy for each
#'        of the 9 possible lead-off batters.  Preserves the order of the lineup. As a 
#'        default, only the first player in \code{lineup} is used as lead-off.  
#'        Cycling is not relevant when the lineup is made up of a single player.
#' @param max_at_bats maximum number of at bats (corresponding to matrix powers) used 
#'        in calculation.  Must be sufficiently large to achieve convergence.  
#'        Convergence may be checked using \code{plot} with \code{type = 1}.
#'        
#' @return A list of the S3 class "\code{chain}" with the following elements:
#'     \itemize{
#'         \item \code{lineup}: copy of input lineup
#'         \item \code{stats}: copy of input stats
#'         \item \code{score_full}: list of matrices containing expected score by 
#'         each base/out state and the number of at-bats (created by matrix powers).  
#'         List index corresponds to lead-off batter.  Rows of matrix correspond to 
#'         base/out states.  Each column represents an additional matrix power.  Used
#'         to assess convergence of the chain (through convergence of each row).
#'         \item \code{score_state}: matrix of expected score at the completion of
#'         an inning based on starting base/out state.  Rows correspond to initial state;
#'         columns correspond to lead-off batter.  Equal to the final column of 
#'         \code{score_full}.
#'         \item \code{score}: vector of expected score for an entire inning (starting 
#'         from zero runners and zero outs).  Index corresponds to lead-off batter.  
#'         Equal to the first row of \code{score_state}.
#'         \item \code{time}: computation time in seconds
#'     }
#'        
#' @references 
#' B. Bukiet, E. R. Harold, and J. L. Palacios, “A Markov Chain Approach to Baseball,” 
#'     Operations Research 45, 14–23 (1997).
#'     
#' @examples
#' # Expected score for single batter (termed "offensive potential")
#' chain1 <- chain("B", wku_probs)
#' plot(chain1)
#' 
#' # Expected score without cycling
#' lineup <- wku_probs$name[1:9]
#' chain2 <- chain(lineup, wku_probs)
#' plot(chain2)
#' 
#' # Expected score with cycling
#' chain3 <- chain(lineup, wku_probs, cycle = TRUE)
#' plot(chain3, type = 1:3)
#' 
#' \donttest{
#' # GAME SITUATION COMPARISON OF CHAIN AND SIMULATOR
#' 
#' # Select lineup made up of the nine "starters"
#' lineup <- sample(wku_probs$name[1:9], 9)
#' 
#' # Average chain across lead-off batters
#' chain_avg <- mean(chain(lineup, wku_probs, cycle = TRUE)$score)
#' 
#' # Simulate full 7 inning game (recommended to increase cores)
#' sim_score <- sim(lineup, wku_probs, inn = 7, reps = 50000, cores = 1)
#' 
#' # Split into bins in order to plot averages
#' sim_grouped <- split(sim_score$score, rep(1:100, times = 50000 / 100))
#' 
#' # Plot results
#' boxplot(sapply(sim_grouped, mean), ylab = 'Expected Score for Game')
#' points(1, sim_score$score_avg_game, pch = 16, cex = 2, col = 2)
#' points(1, chain_avg * 7, pch = 18, cex = 2, col = 3)
#' }
#'        
#' @export

chain <- function(lineup, stats, cycle = FALSE, max_at_bats = 18) {
  
  tic <- proc.time()[3]
  
  # only cycle if lineup is of length 9
  if (cycle & (length(lineup) != 9)) 
    warning("There is no point cycling lead-off batters with a single batter")
  
  k <- length(base_states)
  
  # create output object
  out <- list(lineup = lineup, stats = stats)
  class(out) <- "chain"

  # run check function, store new cleaned stats
  stats <- check(lineup, stats)
  
  # decompose batter matrices
  slow_B <- decompose(slow_B)
  fast_B <- decompose(fast_B)
  
  # for each player populate runs vector, transition matrix, and steal matrix
  run_vector <- list()
  bat_matrix <- list()
  steal_matrix <- list()
  
  for (p in 1:9) {
    
    # gather matrix templates
    if (stats[['FAST']][p]) {
      # for fast players use fast matrices
      temp_O1 <- fast_B$O1
      temp_O2 <- fast_B$O2
      temp_R1 <- fast_B$R1
      temp_R2 <- fast_B$R2
      temp_S <- fast_S # steal matrix doesn't need to be decomposed
    } else {
      # for slow players use slow matrices
      temp_O1 <- slow_B$O1
      temp_O2 <- slow_B$O2
      temp_R1 <- slow_B$R1
      temp_R2 <- slow_B$R2
      temp_S <- slow_S
    }
    
    # fill in transition matrices with player probabilities
    for (play in c("O", "S", "D", "TR", "HR", "W")){
      temp_O1[temp_O1 == play] <- stats[p, play]
      temp_O2[temp_O2 == play] <- stats[p, play]
    }
    
    # convert transition matrices to numeric
    temp_O1 <- matrix(sapply(temp_O1, as.numeric), nrow = k, ncol = k)
    temp_O2 <- matrix(sapply(temp_O2, as.numeric), nrow = k, ncol = k)
    
    # store results in lists
    run_vector[[p]] <- apply(temp_O1 * temp_R1, 1, sum) + apply(temp_O2 * temp_R2, 1, sum)
    bat_matrix[[p]] <- temp_O1 + temp_O2
    
    # fill in steal matrices with player probabilities
    temp_S[temp_S == "NSA"] <- 1 - stats[p, "SBA"] # no steal attempt
    temp_S[temp_S == "SB"] <- stats[p, "SBA"] * stats[p, "SB"] # attempted and successful
    temp_S[temp_S == "CS"] <- stats[p, "SBA"] * (1 - stats[p, "SB"]) # attempted and caught
    temp_S[is.na(temp_S)] <- 0
    
    # convert matrix to numeric
    temp_S <- matrix(sapply(temp_S, as.numeric), nrow = k, ncol = k)
    
    # Store results in list
    steal_matrix[[p]] <- temp_S
    
  }
  
  # also for each player, note the previous player of opposite fast type (for stealing)
  # if all players are FAST or all are slow, ignore previous_stealer
  # this is independent of cycling the lineup
  if (all(stats[['FAST']]) | all(!stats[['FAST']])) {
    previous_stealer <- NULL
  } else {
    previous_stealer <- vector(length = 9)
    slows <- which(!stats[['FAST']])
    fasts <- which(stats[['FAST']])
    for (p in 1:9) {
      if (stats[['FAST']][p]) {
        # player is FAST, find previous slow
        # if some slow player is less than p, use that, else use max 
        if (all(!slows < p)) { # no slow player less than p
          previous_stealer[p] <- max(slows)
        } else { # some slow player less than p
          previous_stealer[p] <- max(slows[slows < p])
        }
      } else {
        # player is slow, find previous fast
        if (all(!fasts < p)) { # no fast player less than p
          previous_stealer[p] <- max(fasts)
        } else { # some fast player less than p
          previous_stealer[p] <- max(fasts[fasts < p])
        }
      } # end of else statement
    } # end of for loop
  } # end of else statement
  
  full_exp <- list()
  loops <- ceiling(max_at_bats / 9)
  
  if (cycle) { # store results for all 9 possible lead-off batters
    lead_off <- 1:9
  } else { # only evaluate lineup with original lead-off batter
    lead_off <- 1
  }
  
  # evaluate run expectancy for each lead-off batter
  for (p in lead_off) {
    
    # generate bat order for given lead-off batter
    bat_order <- c(p:9, 1:(p - 1))[1:9]

    # generate vector - expected number of runs from each position
    full_exp[[p]] <- matrix(nrow = k, ncol = max_at_bats)
    trans_powered <- diag(k)
    score_sum <- rep(0, times = k)
  
    for (i in 1:loops) {
      for (batter in 1:9) {
        index <- batter + (9 * (i - 1))
      
        # if greater than max_at_bats, escape loop
        if (index > max_at_bats) break
      
        # add score_sum and store
        score_sum <- score_sum + trans_powered %*% run_vector[[bat_order[batter]]]
        full_exp[[p]][, index] <- score_sum
      
      # increase trans_powered
      trans_powered <- trans_powered %*% bat_matrix[[bat_order[batter]]] %*% steal_matrix[[bat_order[batter]]]
      # if previous_stealer stored, use it
      if (!is.null(previous_stealer))
        trans_powered <- trans_powered %*% steal_matrix[[bat_order[previous_stealer[batter]]]]
      } # end of "batter" for loop
    } # end of "i" for loop
  } # end of "p" for loop

  out$score_full <- full_exp
  out$score_state <- matrix(nrow = k - 1, ncol = length(lead_off))
  out$score <- vector(length = length(lead_off))
  
  # gather summary statistics
  for (p in lead_off) {
    out$score_state[, p] <- full_exp[[p]][-k, max_at_bats]
    out$score[p] <- full_exp[[p]][1, max_at_bats]
  }
  
  toc <- proc.time()[3]
  out$time <- toc - tic
  
  return(out)
}

# Plot Function -------------------------------------------------------------
#' @title Plots an object of S3 class "\code{chain}"
#' @description Acts on a "\code{chain}" object output from the "\code{chain}" 
#'     function.  Plots convergence of chain, expected score by state, and 
#'     expected score by lead-off batter (if applicable).
#'     
#' @details This function generates three types of plots:
#'     \itemize{
#'         \item Type 1: Plots chain convergence.  Each line corresponds to the 
#'         expected score from a specific initial base/out state as at-bats
#'         are accumulated.  If the chain has reached convergence, each line 
#'         should level off.
#'         \item Type 2: Plots expected score by initial base/out state.  This 
#'         plot can be used to compare different states (e.g. is it better to have
#'         a runner on second and one out or a runner on first and no outs?).  
#'         \item Type 3: Plots expected score for an inning based on lead-off batter.
#'         Requires a \code{chain} object that was created with \code{cycle = TRUE}.
#'         The average across all lead-off batters is the most holistic metric for 
#'         comparing different lineups.
#'     }
#'     Both type 1 and type 2 plots rely on the specification of a lead-off batter.
#'     In states with runners on base and/or outs, the lead-off batter refers to the
#'     first batter to come up to to the plate starting in that situation, not 
#'     the first batter to start the inning.  The "true" lead-off batter at the 
#'     start of the inning corresponds to the R0 (no runners) 0 out case.
#'     
#' @param x object of class "\code{chain}"
#' @param type denotes which type of plot to generate - 1, 2, 3, or any combination
#'        of these.  See details for plot descriptions.
#' @param lead_off an integer 1-9.  Denotes which lead-off batter to plot in type 1 
#'        and type 2 plots.  Lead-off batters 2-9 are only available if \code{chain}
#'        was calculated with \code{cycle = TRUE}.
#' @param fast logical indicating whether to plot additional fast player states in
#'        type 1 and type 2 plots.
#' @param ... NA
#' 
#' @return No return value, called to generate plots.
#'        
#' @examples 
#' # Expected score for single batter (termed "offensive potential")
#' chain1 <- chain("B", wku_probs)
#' plot(chain1)
#' 
#' # Expected score without cycling
#' lineup <- wku_probs$name[1:9]
#' chain2 <- chain(lineup, wku_probs)
#' plot(chain2)
#' 
#' # Expected score with cycling
#' chain3 <- chain(lineup, wku_probs, cycle = TRUE)
#' plot(chain3, type = 1:3)
#'         
#' @export

plot.chain <- function(x, type = 1:2, lead_off = 1, fast = FALSE, ...) {
  
  # check if object was calculated with chain = TRUE
  if (length(x$score_full) == 9) chain <- TRUE else chain <- FALSE
  
  # verify that specified lead_off is valid
  if ((lead_off != 1) & !chain)
    stop("Chain was not calculated for that lead-off batter")
  
  # adjust par if appropriate
  if (length(type) > 1) {
    # save and restore par settings
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mfrow = c(1, length(type)))
  }
  
  # Determine which states to plot based on fast
  if (fast) {
    states <- 1:(length(base_states) - 1)
  } else {
    states <- c(1, 2, 4, 6, 7, 11, 13, 15)
    states <- c(states, states + 18, states + (18 * 2))
  }
  
  # Plot type 1 - convergence plot for specified lead-off batter
  if (1 %in% type) {
    matplot(t(x$score_full[[lead_off]][states, ]), type = "l", col = "grey",
          xlab = "At Bats", ylab = "Expected Score",
          main = paste0("Markov Chain Convergence by State \n for Lead-off Batter ", lead_off))
  }
  
  # Plot type 2 - score by state for specified lead-off batter
  if (2 %in% type) {
    h <- length(states)
  
    plot(1:h, x$score_state[states, lead_off], type = 'h', lwd = 5, col ='grey50', lend = 1, 
         ylim = c(0, max(x$score_state[states, lead_off]) + 0.8),
         xlab = '', ylab = "Expected Score", xaxt = 'n',
        main = paste0("Expected Score by Base/Out State \n for Lead-off Batter ", lead_off))
    text(1:h, x$score_state[states, lead_off] + 0.2, 
        round(x$score_state[states, lead_off], digits = 2), pos = 3, srt = 90)
    axis(side = 1, at = 1:h, labels = base_states[states], las = 2)
    abline(v = h/3 + 0.5, col = 'grey', lty = 2, lwd = 2)
    abline(v = h*2/3 + 0.5, col = 'grey', lty = 2, lwd = 2)
    text(h/6 + 0.5, max(x$score_state[states, lead_off])+0.5, '0 outs', col = 'grey')
    text(h/2 + 0.5, max(x$score_state[states, lead_off])+0.5, '1 out', col = 'grey')
    text(h*5/6 + 0.5, max(x$score_state[states, lead_off])+0.5, '2 outs', col = 'grey')
  }
  
  # Plot type 3 - comparing lead-off batters, requires chain = TRUE
  if (3 %in% type) {
    if (chain) {
      plot(1:9, x$score, xlab = "Lead Off Batter", ylab = "Expected Score by Lead-off Batter")
      abline(h = mean(x$score), col = 2, lty = 2)
      text(2, mean(x$score) + 0.02, labels = paste0("Avg = ", round(mean(x$score), 2)),
           col = 2)
    } else stop("Plot type 3 requires chain = TRUE")
  }
}

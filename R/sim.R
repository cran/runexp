
# Contents --------------------------------------------------------------------
# Internal Functions
#   sim: run expectancy simulator

# Imported Functions ----------------------------------------------------------
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster detectCores stopCluster
#' @importFrom stats rbinom rmultinom

# sim Function ----------------------------------------------------------------

#' @title Softball run expectancy using multinomial random trial simulation
#' @description Utilizes a multinomial simulation to simulate a softball game scenario 
#'     with a specified number of innings (inn) per game over a specified number of games 
#'     (reps). Calculations depend on specified player probabilities (see details) and a 
#'     nine-player lineup.  Optionally incorporates attempted steals and "fast" players
#'     who are able to stretch bases.  Optionally utilizes SNOW parallelization.

#' @details In each simulation, we determine each batter's hit results
#'     through a multinomial random trial where the probability of walk (W), single (S), 
#'     double (D), triple (TR), home run (HR), and batter out (O) are assigned per input player 
#'     statistics. We incorporate the impact of "fast" players through the following assumptions: 
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
#'     Aside from these fast player assumptions, runners advance bases as expected (a single
#'     advances each runner one base, a double advances each runner two bases, etc.).
#'     
#'     
#'     Following softball norms, we only entertain steals of second base.  Steals are considered
#'     in cases when there is a runner on first and no runner on second.  In these situations,
#'     we use a bernoulli coin flip (based on the runner's SBA probability) to determine whether the
#'     runner on first will attempt a steal.  In practice, these decisions are commonly left up to coaches.
#'     If it is decided that the player will attempt a steal, a second bernoulli coin flip (based
#'     on the runner's SB probability) determines whether the steal was successful or whether the player was
#'     caught stealing.
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
#'     will be assigned based on each player's "SBA" probability.  Players who are more 
#'     likely to attempt steals are likely the fast players.
#'     
#'     As a default, simulations will be processed in parallel over all but one of the 
#'     maximum available cores.  Parallelization is recommended to reduce computation time.
#'     Interactive plotting (\code{graphic = TRUE}) requires no parallelization and will 
#'     override specified cores with \code{cores = 1}.
#'     
#' @param lineup either character vector of player names or numeric vector
#'        of player numbers.  Must be of length 1 or 9.  If lineup is of length 1, the 
#'        single player will be "duplicated" nine times to form a complete lineup.  
#' @param stats data frame of player statistics (see details)
#' @param inn   number of innings per rep (the default of 7 represents a typical softball game)
#' @param reps  number of times to repeat the softball game simulation. 
#'              Can be thought of as number of games.
#' @param graphic logical indicating on whether to plot the 
#'                player base movement.  Requires \code{reps < 4}.  Forces \code{cores = 1}.
#' @param waitTime the amount of time to pause before making 
#'                 next plot for play. Only relevant when \code{graphic = TRUE}.
#' @param cores number of cores to utilize in parallel.  Defaults to one less than maximum
#'              available nodes.
#' @return A list of the S3 class "\code{sim}" with the following elements:
#'     \itemize{
#'         \item \code{lineup}: copy of input lineup
#'         \item \code{stats}: copy of input stats
#'         \item \code{inn}: copy of input innings
#'         \item \code{score}: a vector containing the scores per each rep (game) 
#'         \item \code{score_avg_game}: the average expected score per rep (game). That is, \code{mean(score)}.
#'         \item \code{score_avg_inn}: the average expected score per rep (game) per inning. 
#'                That is, \code{mean(score)/inn}. If \code{inn = 1}, then \code{score_avg_game = score_avg_inn}.
#'         \item \code{time}: computation time in seconds 
#'     }
#'     
#' @examples 
#' # Short simulation (designed to run in less than 5 seconds)
#' sim1 <- sim("B", wku_probs, inn = 1, reps = 100, cores = 1)
#' \donttest{
#' # Simulation with interactive graphic
#' lineup <- wku_probs$name[1:9]
#' sim2 <- sim(lineup, wku_probs, inn = 7, reps = 1, graphic = TRUE)
#' 
#' # Simulation for entire game (recommended to increase cores)
#' sim3 <- sim(lineup, wku_probs, cores = 1)
#' boxplot(sim3$score)
#' points(1, sim3$score_avg_game)
#' 
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
#' boxplot(sapply(sim_grouped, mean), ylab = 'Expected Score for Game')
#' points(1, sim_score$score_avg_game, pch = 16, cex = 2, col = 2)
#' points(1, chain_avg * 7, pch = 18, cex = 2, col = 3)
#' }
#'        
#' @export


sim <- function(lineup, stats, inn = 7, reps = 100, graphic = FALSE, waitTime = 2,
                cores = NULL)
{
  ###################### -- PREPROCESSING STAGE --#########################
  
  # Decision to Parallelize the code
  if (graphic)
  {
    cores <- 1 
  }else if (is.null(cores))
  {
    cores <- detectCores() - 1
  }
  
  # Error Message: For printing interactive graphics for more than 3 reps
  if (reps > 3 & graphic == TRUE)
  {
    stop("Reps > 3 and graphic = TRUE. This results in slow calculation time
         and overload in Plot Window.")
  }
  
  # Message to User:
  print(paste("Simulator is running ", reps, " game reps. with ", inn, "innings."))
  
  time.check <- proc.time()[3] # Keeping track of run time
  p_stats <- check(lineup,stats)# run check function, store new cleaned stats
  
  ##  Defaults of Game Aspects #
  Score = 0; # Score
  Outs = 0;  # Number of Outs
  runners = matrix(c(0,0,0),nrow = 1); # Every Game Start with no runners on base.
  numberAtBat = 1; # Tracks the Number of Hits
  stTracker = fpTracker = matrix(c(0,0,0),nrow = 1);# Start of the Game, no ones on base
  
  hitResult = "O";
  
  # Finding the columns associated with the hitting probabilities: This 
  # variable is used in hitResults for atBat_prob[player_indx, prob_colums]
  prob_columns = c(which(colnames(p_stats) == "W"),which(colnames(p_stats) == "S"),
                   which(colnames(p_stats) == "D"),which(colnames(p_stats) == "TR"),
                   which(colnames(p_stats) == "HR"),which(colnames(p_stats) == "O"))
  ###############################################################################
  
  ###################### -- NO PARALLELIZATION (CORE = 1) --#########################
  if (cores == 1)
  { 
      score_rep = rep(NA,reps) # Allocating space to store score per rep (ie. game)
    
      ## Looping over the number of reps (ie. the number of games) 
      for(r in 1:reps) 
      {
          # Plotting - Start of GAME 
          if (graphic == TRUE)
          {
            emptyPlot(title="Softball Game Simulation", color="blue2")
            # Descriptive Text 
            text(0,1.00,label = paste('Game:',r, ' of ', reps),
                 family= 'serif', col = "blue2", font = 2) 
            if (numberAtBat == 0) # First Batter on Games
            {
              text(0,.85,label = paste('At Bat:',p_stats$NAME[1]),
                   family= 'serif', col = "blue2", font = 2) 
            }else{# After that Batter on Games
              text(0,.85,label = paste('At Bat:',p_stats$NAME[atBatTracker(numberAtBat)$atBat_indx]),
                   family= 'serif', col = "blue2", font = 2) 
            }
            Sys.sleep(waitTime) # Pause to see graphic
          }
        
        ## Looping over the number of innings
        for (i in 1:inn) 
        {
          # Plotting - STart of Inning 
          if (graphic == TRUE)
          {
            emptyPlot(title="Softball Game Simulation", color="forestgreen")
            # Descriptive Text 
            text(0,1.00,label = paste('Game:',r, '- Inning:',i),
                 family= 'serif', col = "forestgreen", font = 2) 
            if (numberAtBat == 0) # First Batter on Games
            {
              text(0,.85,label = paste('At Bat:',p_stats$NAME[1]),
                   family= 'serif', col = "forestgreen", font = 2) 
            }else{# After that Batter on Games
              text(0,.85,label = paste('At Bat:',p_stats$NAME[atBatTracker(numberAtBat)$atBat_indx]),
                   family= 'serif', col = "forestgreen", font = 2) 
            }
            Sys.sleep(waitTime) # Pause to see graphic
          }
          
          ## Looping over the Outs 
          while(Outs < 3) 
          {
            
            # Tracking who is atBat and nextAtBat
            abTrk_indx = atBatTracker(numberAtBat)
            # If there is any one on 1st and not 2nd, we can steal expect after when get a OUT. 
            # This assumption says, that if a player gets on 1st and the following player gets out
            # the player on first can not steal anymore.
            if(runners[1] == 1 & runners[2]!=1 & p_stats$SBA[abTrk_indx$prevAtBat_indx]>0 & hitResult != "O") # We can steal
            {
              # SBA: Tell us the probability the coach wants to steal with player on 1st?
              stolenBaseAttempt = rbinom(1,1,p_stats$SBA[abTrk_indx$prevAtBat_indx])
              # If stolenBaseAttempt = 1, then we attempt to steal.
              
              if (stolenBaseAttempt == 1) # Attempt to steal base.
              {
                
                # SB: Tells us the probability of successfully stealing the base.
                stealSuccess = rbinom(1,1,p_stats$SB[abTrk_indx$prevAtBat_indx]) 
                
                # If we successful steal base, then
                if (stealSuccess == 1)
                {
                  runners[1,1] = 0; runners[1,2] = 1;# 1st Base player moves to 2nd
                  stTracker[2] = 1; # indicate a stealer on 2nd
                  
                  # If the player who stole was also Fast, then remove fast indicator
                  if (fpTracker[1] == 1)
                  {fpTracker[1] = 0}
                  
                  # Plotting - Movement
                  if (graphic == TRUE)
                  {
                    movementPlot(atBat = p_stats$NAME[abTrk_indx$atBat_indx], 
                                 nextAtBat = p_stats$NAME[abTrk_indx$nextAtBat_indx],
                                 runners = runners,
                                 hitResult = "SB",O = Outs, score = Score, 
                                 numberAtBat = numberAtBat,
                                 fpTracker = fpTracker,stTracker = stTracker) 
                  }
                  
                  # If we were caught steal base, then
                }else{
                  
                  runners[1,1]=0; # runner on  1st ->Out.
                  Outs = Outs +1; 
                  
                  # If the person on 1st was fast and caught stealing,
                  # then remove fast indicator
                  if (fpTracker[1] == 1){fpTracker[1] = 0}
                  
                  # Plotting - Movement
                  if (graphic == TRUE)
                  {
                    movementPlot(atBat = p_stats$NAME[abTrk_indx$atBat_indx], 
                                 nextAtBat = p_stats$NAME[abTrk_indx$nextAtBat_indx],
                                 runners = runners,
                                 hitResult = "CS",O = Outs, score = Score, 
                                 numberAtBat = numberAtBat,
                                 fpTracker = fpTracker,stTracker = stTracker) 
                  }
                  
                }# END: If-Else - Stolen Base or Cuaght Stealing
                
              }else # Don't Attempt to steal base; 
              {
                play <- hitResults(atBat = p_stats$NAME[abTrk_indx$atBat_indx],
                                   atBat_prob = p_stats[abTrk_indx$atBat_indx,prob_columns], 
                                   fp_atBat = p_stats$FAST[abTrk_indx$atBat_indx], 
                                   nextAtBat = p_stats$NAME[abTrk_indx$nextAtBat_indx],
                                   runners = runners, 
                                   score = Score , O = Outs, 
                                   outcome = c("W","S",'D','TR','HR',"O"),
                                   numberAtBat = numberAtBat,
                                   fpTracker = fpTracker, stTracker = stTracker, 
                                   graphic = graphic)
                
                # Record the play results
                runners = play$runners; Score = play$score; Outs = play$O;
                hitResult = play$hitResult
                numberAtBat = play$numberAtBat
                fpTracker = play$fpTracker; stTracker = play$stTracker; 
                
              } # END: If-Else - Stolen Base Attempt or Nah
            } else
            {
              play <- hitResults(atBat = p_stats$NAME[abTrk_indx$atBat_indx],
                                 atBat_prob = p_stats[abTrk_indx$atBat_indx,prob_columns], 
                                 fp_atBat = p_stats$FAST[abTrk_indx$atBat_indx], 
                                 nextAtBat = p_stats$NAME[abTrk_indx$nextAtBat_indx],
                                 runners =runners, 
                                 score = Score , O = Outs, 
                                 outcome = c("W","S",'D','TR','HR',"O"),
                                 numberAtBat = numberAtBat,
                                 fpTracker = fpTracker, stTracker = stTracker, 
                                 graphic = graphic)
              
              # Record the play results
              runners = play$runners; Score = play$score; Outs = play$O;
              hitResult = play$hitResult
              numberAtBat = play$numberAtBat
              fpTracker = play$fpTracker; stTracker = play$stTracker;
              
            }  #END - stealing chances
            
            # For graphics, pause the code to see graphics.
            if(graphic == TRUE){Sys.sleep(waitTime)}
            
          }# Out Loop
          
          # End of Inning - Rest the base, outs
          Outs = 0 # Reset the number of Outs 
          runners = matrix(c(0,0,0),nrow = 1); # Every Game Start with no runners on base.
          stTracker = fpTracker = matrix(c(0,0,0),nrow = 1);# Start of the Game, no ones on base
          
          if(graphic == TRUE)
          {
            endInningPlot(inn = i, score = Score, numberAtBat = numberAtBat)
            Sys.sleep(waitTime)
          }
        } # END: Inning Loop
        
        score_rep[r] = Score # Store Score
        Score = 0; # Reset score for new rep. 
        numberAtBat = 0; # Resest the numberofAtBats
        
      
      } # END: for loop for core = 1
  
  ###################### -- ESLE: PARALLELIZATION (CORE > 1) -- #########################
  }else
  { 
        # Prepare parallel clusters
        if (cores > detectCores()) warning('cores is greater than available nodes')
        cl <- makeCluster(cores)
        registerDoParallel(cl)
      
      score_rep <- foreach(r = 1:reps) %dopar% 
      {
        ## Removed Plotting - for parallelization 
        
        ## Looping over the number of innings
        for (i in 1:inn) 
        {

          ## Looping over the Outs 
          while(Outs < 3) 
          {
            
            # Tracking who is atBat and nextAtBat
            abTrk_indx = atBatTracker(numberAtBat)
            # If there is any one on 1st and not 2nd, we can steal expect after when get a OUT. 
                      # This assumption says, that if a player gets on 1st and the following player gets out
                      # the player on first can not steal anymore.
            if(runners[1] == 1 & runners[2]!=1 & p_stats$SBA[abTrk_indx$prevAtBat_indx]>0 & hitResult != "O") # We can steal
            {
              # SBA: Tell us the probability the coach wants to steal with player on 1st?
              stolenBaseAttempt = rbinom(1,1,p_stats$SBA[abTrk_indx$prevAtBat_indx])
              # If stolenBaseAttempt = 1, then we attempt to steal.
              
              if (stolenBaseAttempt == 1) # Attempt to steal base.
              {
                
                # SB: Tells us the probability of successfully stealing the base.
                stealSuccess = rbinom(1,1,p_stats$SB[abTrk_indx$prevAtBat_indx]) 
                
                # If we successful steal base, then
                if (stealSuccess == 1)
                {
                  runners[1,1] = 0; runners[1,2] = 1;# 1st Base player moves to 2nd
                  stTracker[2] = 1; # indicate a stealer on 2nd
                  
                  # If the player who stole was also Fast, then remove fast indicator
                  if (fpTracker[1] == 1)
                  {fpTracker[1] = 0}
                  
                # If we were caught steal base, then
                }else{
                  
                  runners[1,1]=0; # runner on  1st ->Out.
                  Outs = Outs +1; 
                  
                  # If the person on 1st was fast and caught stealing,
                  # then remove fast indicator
                  if (fpTracker[1] == 1){fpTracker[1] = 0}
                  
                }# END: If-Else - Stolen Base or Cuaght Stealing
                
              }else # Don't Attempt to steal base; 
              {
                play <- hitResults(atBat = p_stats$NAME[abTrk_indx$atBat_indx],
                                   atBat_prob = p_stats[abTrk_indx$atBat_indx,prob_columns], 
                                   fp_atBat = p_stats$FAST[abTrk_indx$atBat_indx], 
                                   nextAtBat = p_stats$NAME[abTrk_indx$nextAtBat_indx],
                                   runners = runners, 
                                   score = Score , O = Outs, 
                                   outcome = c("W","S",'D','TR','HR',"O"),
                                   numberAtBat = numberAtBat,
                                   fpTracker = fpTracker, stTracker = stTracker, 
                                   graphic = FALSE)
                
                # Record the play results
                runners = play$runners; Score = play$score; Outs = play$O;
                hitResult = play$hitResult
                numberAtBat = play$numberAtBat
                fpTracker = play$fpTracker; stTracker = play$stTracker; 
                
              } # END: If-Else - Stolen Base Attempt or Nah
            } else
            {
              play <- hitResults(atBat = p_stats$NAME[abTrk_indx$atBat_indx],
                                 atBat_prob = p_stats[abTrk_indx$atBat_indx,prob_columns], 
                                 fp_atBat = p_stats$FAST[abTrk_indx$atBat_indx], 
                                 nextAtBat = p_stats$NAME[abTrk_indx$nextAtBat_indx],
                                 runners =runners, 
                                 score = Score , O = Outs, 
                                 outcome = c("W","S",'D','TR','HR',"O"),
                                 numberAtBat = numberAtBat,
                                 fpTracker = fpTracker, stTracker = stTracker, 
                                 graphic = FALSE)
              
              # Record the play results
              runners = play$runners; Score = play$score; Outs = play$O;
              hitResult = play$hitResult
              numberAtBat = play$numberAtBat
              fpTracker = play$fpTracker; stTracker = play$stTracker;
              
            }  #END - stealing chances
            
          }# Out Loop
          
          # End of Inning - Rest the base, outs
          Outs = 0 # Reset the number of Outs 
          runners = matrix(c(0,0,0),nrow = 1); # Every Game Start with no runners on base.
          stTracker = fpTracker = matrix(c(0,0,0),nrow = 1);# Start of the Game, no ones on base
          
        } # END: Inning Loop
        
        score_rep = Score # Store Score
        Score = 0; # Reset score for new rep. 
        numberAtBat = 0; # Resest the numberofAtBats
        
        return(score_rep)
        
      } # END: foreach Loop
      
      score_rep <- unlist(score_rep) # Unlist the foreach variable
      stopCluster(cl)# 
  
  } # END: PARALLELIZATION IF-ELSE
  
  time.check <- proc.time()[3] - time.check;
  # Output. 
  out <- list(lineup = lineup,
              stats = stats,
              inn = inn,
              score = score_rep,
              score_avg_game = mean(score_rep),
              score_avg_inn = mean(score_rep)/inn,
              time = time.check)
  class(out) <- "sim"
  
  return(out)  
}# Function Loop


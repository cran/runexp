# Contents --------------------------------------------------------------------
# External Functions
#     prob_calc (see documentation below)

#' @title Calculates player probabilities given players' game statistics.
#' @description Uses player statistics to calculate the probability of six
#'     possible at bat outcomes (walk, single, double, triple, homerun, or out).
#'     Also estimates the probability of a player attempting a steal (SBA) and
#'     succeeding in an attempted steal (SB).  Player game statistics are 
#'     commonly available on team's public webpages. 
#'
#' @param playerData data frame of the players statistics (details below)
#' 
#' @details The \code{playerData} data frame must contain the following columns 
#'     of player statistics:
#'     \itemize{
#'         \item Name: player name
#'         \item Number: player number
#'         \item AB: at bats
#'         \item BB: walks
#'         \item HBP: hit by pitch
#'         \item H: hits
#'         \item 2B: doubles
#'         \item 3B: triples
#'         \item HR: homeruns
#'         \item ATT: attempted steals
#'     }
#'     Plate appearances (PA) are calculated 
#'     as AB + BB + HBP.  The player probabilities are calculated as:
#'     \itemize{
#'         \item Walk probability: W = (BB + HBP) / (PA) 
#'         \item Single probability: S = (H - (2B + 3B + HR)) / (PA) 
#'         \item Double probability: D = 2B / PA
#'         \item Triple probability: TR = 3B / PA
#'         \item Home Run probability: HR = HR / PA
#'         \item Out probability: O = (PA - (H + BB + HBP)) / PA
#'     }
#'     Probabilities calculated from limited at bats will not be very useful.  Note, 
#'     this function does not assign TRUE/FALSE values for fast players.  These may
#'     be manually assigned or will be assigned based on SBA probability when \code{chain} 
#'     or \code{sim} functions are called.
#'     
#'     SBA (Stolen Base Attempt) is the probability a player will attempt to steal given they
#'     are on first base and there is no runner on second. As a default, we estimate a player's
#'     SBA probability using a rough thresholding rule based on the team's overall SB probability 
#'     and the player's SB probability.  Essentially, we group the players into three categories: 
#'      \itemize{
#'         \item Almost Always Attempt to Steal Group: These players receive the SBA  probability of 
#'              the team's overall SB probability which is calculated as (Team's total # of SB)/(Team's total # of SBA)
#'         \item 50/50 Attempt to Steal Group: These players receive SBA  probability of 0.50
#'         \item Never Attempt to Steal Group: These players receive SBA  probability of 0.0
#'     }
#'     We recommend reviewing these default probabilities before proceeding with run expectancy
#'     calculations.
#'
#' @return a dataframe of the players' probabilities for W, S, D, TR, HR, O, SBA, and SB
#' 
#' @examples
#' probs <- prob_calc(wku_stats)   # probs corresponds to wku_probs
#' 
#' @export

prob_calc = function(playerData)
{
  # uppercase column names in stats
  colnames(playerData) <- toupper(colnames(playerData))
  
  # check for name column in playerData
  if (!("NAME" %in% colnames(playerData))) 
  {
    stop("playerData must have a 'NAME' column")
  }
    
  # convert name to character if not
  if (!is.character(playerData$NAME)) 
  {
    playerData$NAME <- as.character(playerData$NAME)
  }
  
  # check for number column in playerData
  if (!("NUMBER" %in% colnames(playerData))) 
  {
    stop("playerData must have a 'NUMBER' column")
  }
    
  # convert number to numeric if not
  if (!is.numeric(playerData$NUMBER)) 
  {
    playerData$NUMBER <- as.numeric(playerData$NUMBER)
  }
  
  # If a player has 0 at bats (AB), we are making AB = 1,so that 
  # we are not dividing by 0 in our below calculations
  playerData$AB[which(playerData$AB == 0)] = 1 
  
  # Create Variable - PA - Plate Appearance
  #     PA = AB (at Bats) + BB (Walks) + HBP (Hit by Pitch)
  playerData$PA = playerData$AB + playerData$BB + playerData$HBP 
  
  # If a player has 0 Plate Appearances (PA), we are making 
  # PA = 1,so that we are not dividing by 0 in our below calculations
  playerData$PA[which(playerData$PA == 0)] = 1 
  
  # - - - - - Calculating the "Typical" Probability - - - - -
  W = (playerData$BB + playerData$HBP)/(playerData$PA) 
  S = (playerData$H - (playerData$`2B`+playerData$`3B` + playerData$HR))/(playerData$PA) 
  D = playerData$`2B`/playerData$PA
  TR = playerData$`3B`/playerData$PA
  HR = playerData$HR/playerData$PA
  O = (playerData$PA -(playerData$H + playerData$BB + playerData$HBP))/playerData$PA
  
  # ---- Probabilty of Successfully Stealing the Base (SB) ----
  SB_ = matrix(0,nrow=nrow(playerData),ncol=1)
  indx_attempt = which(playerData$ATT == 0)
  playerData$ATT[indx_attempt] = 1 # If a player has 0 attempted 
  # steals (ATT), we are making ATT = 1, so that we are not
  # dividing by 0 in our below calculations. 
  SB_ = round(playerData$SB/playerData$ATT,4)
  
  
  # ---- Calculate the SBA probabilty per player ----
  # This metric is the coaches probability of 
  # have a player attempt to steal. Since we do not know the
  # players or the coaches, doing a Arbitary threshold of SBA 
  # into three categories
  
  SBA = matrix(0,nrow=dim(playerData)[1],ncol=1)
  
  # We calculate the teams SBA probability as:
  # total # of SB/ total # of ATT. 
  Teams_SBA = sum(playerData$SB)/(sum(playerData$ATT) - length(indx_attempt))
  # The - length(indx_attempt)) is removing the ATT we had to
  # add in order to not divided by zero.
  
  # Threshold - Arbitary 
  for (ii in 1:dim(playerData)[1]) # For each player
  {
    if (SB_[ii] >= (Teams_SBA-.1) )
    { 
      # If the players SB, is >= Teams_SBA-.1, then give 
      # that player the teams, Teams_SBA probability
      # These players would be rough considered as our fast 
      # player
      SBA[ii] = Teams_SBA 
      
    }else if (SB_[ii] < (Teams_SBA-.1) & SB_[ii] > Teams_SBA-.3)
    {
      # If the players SB, is between Teams_SBA-.1 and Teams_SBA-.3,
      # then give that player a 50/50 probability of stealing
      # These would be the Meduim Players who might steal on slower teams
      
      SBA[ii] = .5
    }else{
      # If the players SB, lower than Teams_SBA-.3,
      # then give that player has  0 probability of attempting to steal
      # These would be the players who we would never want to steal
      SBA[ii] = 0
    }
  }
  
  # Saving Final Results
  PlayersProb = cbind(data.frame(playerData$NAME),data.frame(playerData$NUMBER),data.frame(W,S,D,TR,HR,O,SBA,SB_))
  colnames(PlayersProb)[1] =c('NAME')
  PlayersProb$NAME <- as.character(PlayersProb$NAME)
  colnames(PlayersProb)[2] =c('NUMBER')
  colnames(PlayersProb)[9] =c('SBA')
  colnames(PlayersProb)[10] =c('SB')
  
  # Outputs the players probability of:
  #     W, SG, D, TR, HR, O, SBA, SB
  return(PlayersProb)
}

# Data Documentation -------------------------------------------------------
#' @title Player statistics and probabilities for WKU softball
#'
#' @description Statistics and calculated probabilities for each player on 
#'     the 2013 Western Kentucky University softball team. Data is stored in 
#'     two data frames: \code{wku_stats} contains the game statistics for 
#'     each player and \code{wku_probs} contains the calculated probabilities 
#'     for each player.  Player names and numbers have been replaced with 
#'     random letters/numbers to preserve anonymity.
#' 
#' @details \code{wku_stats} contains the raw player data taken from the 2013 WKU
#'     softball team's webpage.  \code{wku_probs} contains the 
#'     player probabilities calculated using \code{prob_calc}.  Together, they provide
#'     an example of calculating probabilities from player statistics.  See 
#'     \code{?prob_calc} for more details on the columns of the two data frames.
#'     
#'     \code{wku_probs} is designed for use with \code{chain} and \code{sim}.  It 
#'     additionally contains a \code{fast} column that indicates whether each player is
#'     considered fast.  This column is not necessary for running \code{chain} 
#'     and \code{sim}, since it is equivalent to the default assignments.  
#'     
#' @source \url{https://wkusports.com/sports/softball/stats/}
#' 
#' @examples
#' probs <- prob_calc(wku_stats)   # probs corresponds to wku_probs
#' 
#' @rdname wku
"wku_stats"

#' @rdname wku
"wku_probs"
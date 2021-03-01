
# Package Documentation -------------------------------------------------------
#' @title Package runexp
#' @author Annie Sauer \email{anniees@vt.edu}
#' @author Sierra Merkes \email{smerkes@vt.edu}
#' @docType package
#' @name runexp-package
#'
#' @description Implements two methods of estimating runs scored in a softball 
#'     scenario: (1) theoretical expectation using discrete Markov chains and (2) 
#'     empirical distribution using multinomial random simulation. Scores are based 
#'     on player-specific input probabilities (out, single, double, triple, walk, and 
#'     homerun). Optional inputs include probability of attempting a steal, probability 
#'     of succeeding in an attempted steal, and an indicator of whether a player is 
#'     "fast" (e.g. the player could stretch home). These probabilities may be 
#'     calculated from common player statistics that are publicly available on 
#'     team's webpages. Scores are evaluated based on a nine-player lineup and may 
#'     be used to compare lineups, evaluate base scenarios, and compare the offensive 
#'     potential of individual players. Manuscript forthcoming. See Bukiet & Harold (1997) 
#'     <doi:10.1287/opre.45.1.14> for implementation of discrete Markov chains.
#' 
#' @section Important Functions:
#' \itemize{
#'   \item \code{\link[runexp]{chain}}: calculates run expectancy using discrete Markov chains
#'   \item \code{\link[runexp]{sim}}: estimates run expectancy using multinomial simulation
#'   \item \code{\link[runexp]{plot.chain}}: S3 method for plotting \code{chain} output objects 
#'   \item \code{\link[runexp]{prob_calc}}: calculates player probabilities from commonly 
#'         available stats 
#'   \item \code{\link[runexp]{scrape}}: scrapes player statistics from a given URL
#' }
#' 
#' @section Data Files:
#' \itemize{
#'     \item \code{\link[runexp]{wku_stats}}: player statistics for the 2013 Western Kentucky
#'           University softball team
#'     \item \code{\link[runexp]{wku_probs}}: calculated player probabilities for the 2013
#'           Western Kentucky University softball team
#' }
#' 
#' @references
#' B. Bukiet, E. R. Harold, and J. L. Palacios, “A Markov Chain Approach to Baseball,” 
#'     Operations Research 45, 14–23 (1997).
#' 
#' @examples 
#' # see "?scrape", "?prob_calc", "?chain" and "?sim" for relevant examples

NULL
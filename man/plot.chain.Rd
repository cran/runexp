% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markov.R
\name{plot.chain}
\alias{plot.chain}
\title{Plots an object of S3 class "\code{chain}"}
\usage{
\method{plot}{chain}(x, type = 1:2, lead_off = 1, fast = FALSE, ...)
}
\arguments{
\item{x}{object of class "\code{chain}"}

\item{type}{denotes which type of plot to generate - 1, 2, 3, or any combination
of these.  See details for plot descriptions.}

\item{lead_off}{an integer 1-9.  Denotes which lead-off batter to plot in type 1 
and type 2 plots.  Lead-off batters 2-9 are only available if \code{chain}
was calculated with \code{cycle = TRUE}.}

\item{fast}{logical indicating whether to plot additional fast player states in
type 1 and type 2 plots.}

\item{...}{NA}
}
\value{
No return value, called to generate plots.
}
\description{
Acts on a "\code{chain}" object output from the "\code{chain}" 
    function.  Plots convergence of chain, expected score by state, and 
    expected score by lead-off batter (if applicable).
}
\details{
This function generates three types of plots:
    \itemize{
        \item Type 1: Plots chain convergence.  Each line corresponds to the 
        expected score from a specific initial base/out state as at-bats
        are accumulated.  If the chain has reached convergence, each line 
        should level off.
        \item Type 2: Plots expected score by initial base/out state.  This 
        plot can be used to compare different states (e.g. is it better to have
        a runner on second and one out or a runner on first and no outs?).  
        \item Type 3: Plots expected score for an inning based on lead-off batter.
        Requires a \code{chain} object that was created with \code{cycle = TRUE}.
        The average across all lead-off batters is the most holistic metric for 
        comparing different lineups.
    }
    Both type 1 and type 2 plots rely on the specification of a lead-off batter.
    In states with runners on base and/or outs, the lead-off batter refers to the
    first batter to come up to to the plate starting in that situation, not 
    the first batter to start the inning.  The "true" lead-off batter at the 
    start of the inning corresponds to the R0 (no runners) 0 out case.
}
\examples{
# Expected score for single batter (termed "offensive potential")
chain1 <- chain("B", wku_probs)
plot(chain1)

# Expected score without cycling
lineup <- wku_probs$name[1:9]
chain2 <- chain(lineup, wku_probs)
plot(chain2)

# Expected score with cycling
chain3 <- chain(lineup, wku_probs, cycle = TRUE)
plot(chain3, type = 1:3)
        
}

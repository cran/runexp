# Script Name: 
#   sim_batter_funcs.R

# Script Purpose:
#   This script contains the following functions for the invoking 
#   a batters' hitResult and tracking the runners movement of after 
#   the hitResult. 

#   runnerMovement - Updates runner on base situation based on the outcome of 
#                     hitResults function. nested inside hitResults function

#   hitResults - After inputting the players probabilities for "W"-Walk, 
#               "S"-Single, "D" Double, "TR"-Triple, "HR"- Homerun and, "O"-out,
#               this function outputs the Batters' hitResult, runner movement, and
#               score and outs.
#               #+ Note: Has PlotRunMove function. If graphic = FALSE, this will 
#                             be skipped

#   atBatTracker - trackers the batter index give the number of atBats nested 
#                 in the  sim functions

# -----------------------------------------------------------------
# Function Name: 
#   runnerMovement

# Function Description: 
#   Updates runner on base situation based on the outcome of hitResults 
#   function. nested inside hitResults function

# Function Argument:
#   runners   : current runners on base situation. A 1 x 3 vector of 0 and 1 
#               where 1 denotes player on base, 0 denotes no player on 1st, 2nd, 
#               and 3rd base.
#   score     : the Score of the game. integer/numeric DEFAULT = 0
#   hitResult : the type of play that occurred for atBat player. character string 
#               from hitResults function

# Function Output:
#   Outputs the updated runners on base situation and the score.

# Function Examples:
#runnerMovement(hitResult = "S", runners = matrix(c(1,0,0),nrow = 1))
#runnerMovement(hitResult = "D", runners = matrix(c(1,0,0),nrow = 1),score = 2)


runnerMovement = function(hitResult,runners,score=0)
{
  # Allocate variable for the new runners on base situation.
  new_runners = matrix(rep(0,3),nrow = 1,ncol = 3) 
  
  # If there are NO RUNNERS ON BASE,
  if (sum(runners) == 0)
  { 
      if(hitResult == "W" | hitResult == "S")
      { # Walk and S (Single) result in atBat Player to 1st base
        new_runners[1,1] = 1 
      }else if(hitResult == "D")
      { # D (Double) result in atBat Player to 2nd base
        new_runners[1,2] = 1 
      }else if(hitResult == "TR")
      { # TR (Triple) result in atBat Player to 3rd base
        new_runners[1,3] = 1 
      }else if(hitResult == "HR")
      {
        score = score + 1; # score increases by 1
        # new_runners (no one on base) does not change.
      }
    
  # ELSE, there is at least one runner on base 
  }else
  { 
      run_idx = which(runners == 1) # Find and store current bases with players
      
      if(hitResult == "S")
      {   # HITS A SINGLE (S),
          new_runners[1,1] = 1 # atBat player moves to 1st Base
          new_run_idx = run_idx + 1 # On-Base runners moves 1 base.
          
      }else if(hitResult == "D")
      {# HITS A DOUBLE (D),
          new_runners[1,2] = 1 # atBat player moves to 2nd Base
          new_run_idx = run_idx + 2 # On-Base runners moves 2 base.
          
      }else if(hitResult == "TR")
      {# HITS A TRIPLE (TR),
          new_runners[1,3] = 1 # atBat player moves to 3rd Base
          new_run_idx = run_idx + 3 # On-Base runners moves 3 base.
        
      }else if(hitResult == "HR")
      {# HITS A HOME RUN (HR),
            score = score + 1; # add 1 for atBat player
            # atBat player moves to HOME PLATE resulting in no one onBase.
            new_run_idx = run_idx + 4 # On-Base runners moves 4 base.
            
      }else if(hitResult == "W")
      {# WALKED
          # Runners on 1st and 3rd, 
          if (is.element(1,run_idx) & is.element(3,run_idx) & !(is.element(2,run_idx)))
          { 
            new_runners = matrix(c(1,1,1),nrow = 1); # walk results in everyone on base
    
          # No runner on 1st, then 
          }else if (!(is.element(1,run_idx)))
          {
            new_runners = runners; # players on base stay, and 
            new_runners[1,1] = 1; # atBat player on 1st base
            
          # Runner on 1st Base, then
          }else
          { 
            new_runners[1,1] = 1  # atBat Player moves to 1st base
            new_run_idx = run_idx + 1 # On-Base Players moves up one base.
          }
      }
      
      if (!exists("new_run_idx"))
      { # If new_run_idx doesn't exist, then
        # no change in run_idx.
        new_run_idx = run_idx
      }
      
      # Update Score 
      if(any(new_run_idx >= 4)) 
      { # A new_run_idx >= 4 means runners scored, 
        # add those number of runners to score.
        score = score + sum(new_run_idx >= 4)
      }
      
      # Update Runners on Base
      runStill_OnBase = new_run_idx < 4; # runners still on base
      # New runners on Base situation
      new_runners[1,new_run_idx[runStill_OnBase]]=rep(1,sum(runStill_OnBase))
  }
  
  return(list(runners = new_runners, score = score))
}


# -----------------------------------------------------------------
# Function Name: 
#   hitResults

# Function Description: 
#   Given players probabilities for "W"-Walk, "S,-Single "D"-Double, 
#   "TR"-Triple, "HR"- Homerun and, "O"-out, does a multinomial coin-flip
#   to decide hitResult, update runner movement (runnerMovement function),
#   and scores and outs. 

# Function Argument:
#   atBat     : the current at Bat player name. character string
#   atBat_prob: the atBat players' probability vector
#   fp_atBat  : if the current player is a fast player (1) or not fast 
#               player (0). DEFAULT = 0.

#   nextAtBat  : the next at Bat player. character string
#   runners    : current runners on base situation. A 1 x 3 vector of 0 and 1 
#               where 1 denotes player on base, 0 denotes no player on 1st, 2nd, 
#               and 3rd base.

#   O          : the number of outs. integer/numeric. DEFAULT = 0
#   score      : the Score of the game. integer/numeric DEFAULT = 0
#   numberAtBat: represents the number of hits (or at bats) 
#                 in the inning/game. integer/numeric. DEFAULT = 0

#   fpTracker: : tracks the location of fast players. A 1 x 3 vector of 0 and 1 
#                 where 1 denotes fast player on base, 0 denotes no fast player on base.
#                 DEFAULT = matrix(0,ncol = 3).
#   stTracker   : tracks the location of players who stole base. A 1 x 3 vector of 0 and 1 
#                 where 1 denotes stealer on base, 0 denotes no stealer on base.
#                 DEFAULT = matrix(0,ncol = 3).

#   outcome    : corresponding outcomes to the batter probabilities; 
#                     DEFAULT = c("W", "S", "D", "TR", "HR", "O")
#   graphic    : represent whether to plot after runnerMovement after the atBat 
#                players hitResult. DEFAULT = TRUE.

# Function Output:
#   Outputs the updated runners on base situation, the score, the outs, and the hitResult.

# Function Examples:
#hitResults(atBat= "P1",atBat_prob = c(0.2,0.2,0.2,0.1,0.1,.2),fp_atBat = 0,
#           nextAtBat,runners = matrix(c(1,0,0),nrow = 1))

hitResults = function(atBat, atBat_prob, fp_atBat = 0, 
                      nextAtBat,runners, score = 0 , O = 0, numberAtBat = 0,
                      fpTracker = matrix(0,ncol = 3), stTracker = matrix(0,ncol = 3), 
                      outcome = c("W", "S", "D", "TR", "HR", "O"),
                      graphic = TRUE)
{
  # Check consistency between runners, fpTracker,
  for (ii in 1:3)
  {
    if ((fpTracker[ii] == 1) & (runners[ii] != 1))
    {
        stop("hitResults: Fast Player Tracker (fpTracker) does 
           not match the Runners on Base (runners)")
    }
  }
  
  if (stTracker[1]==1)
  {
    stop("hitResults: stTracker - cannot steal 1st base.")
  }
  
  for (ii in 2:3)
  {
    if ((stTracker[ii] == 1) & (runners[ii] != 1))
    {
      stop("hitResults: Stealer Tracker (stTracker) does 
           not match the Runners on Base (runners)")
    }
  }
  
  # Increase the numberAtBat
  numberAtBat = numberAtBat + 1; 
  
  # hitResult 
  hitResult_numeric = rmultinom(1, 1, prob=atBat_prob) # Multi-nomial Distribution for hitResult.
  hitResult = outcome[which(hitResult_numeric == 1)] # Assign outcome label to hitResult_numeric 
  
  if (hitResult == "S")
  {# HIT SINGLE (S)
    gameDescr = "S"; # Game Description:
    
    if(stTracker[1,2] == 1 | fpTracker[1,2] == 1)
    { # A Stealer/Fast Player on 2nd Base,
      
      # score - 3rd Base player (if exists) and Stealer/Fast Player
      score = score + sum(runners[1,2:3]);
      
      # Game Description: Updated
      if (stTracker[1,2] == 1)
      {
        if(sum(runners[1,2:3]) > 1)
        { gameDescr = 'S & 2nd Base (st) \n & 3rd Base Scored';
        }else { gameDescr = 'S & 2nd Base (st) Scored';}
      }else
      {
        if(sum(runners[1,2:3]) > 1)
        { gameDescr = 'S & 2nd Base (fp) \n & 3rd Base Scored';
        }else { gameDescr = 'S & 2nd Base (fp) Scored';}
      }
      
      runners[1,2:3] = rep(0,2); # 2nd and 3rd Base cleared
      
      # Adjust stTracker and fpTracker
      stTracker[1,2:3] = rep(0,2); # 2nd Base Stealer off
      fpTracker[1,2:3] = rep(0,2); # 2nd Base Fast Player is off 
      # Notes: Do not want to clear all FP_tracker because the single 
      # could have been a Fast Player
      
      # Adjust fpTracker: If we have a fp on 1st Base, then S results in fp to 2nd base
      if (fpTracker[1,1] == 1) {fpTracker[1,2] = 1}
      
      
    }
    
    if(fpTracker[1,3] == 1 | stTracker[1,3] == 1 )
    {
      # Adjust fpTracker: If we have a fp on 3rd Base, 
      #                   then S results in fp to HR
      fpTracker[1,3] = 0; 
      stTracker[1,3] = 0;
    }
    
    if (fpTracker[1,1] == 1)
    {
      # Adjust fpTracker: If we have a fp on 1st Base, 
      #                   then S results in fp to 2nd base
      fpTracker[1,2] = 1; fpTracker[1,1] = 0; 
    }

    # If atBat player hits S is fp, then fp on 1st Base
    if(fp_atBat == 1)
    { fpTracker[1,1]=1 
    }else
    { fpTracker[1,1]=0
    }
    
    # Adjust remaining runners in accordance to S.
    r = runnerMovement(hitResult = "S", runners = runners,score=score); 
    runners = r$runners; 
    score = r$score
    
    
  }else if (hitResult == "D")
  {# HIT DOUBLE (D)
    gameDescr = "D";
    if (fpTracker[1] == 1 & O > 0)
    {# Fast player on 1st Base and Outs > 0, then fp goes home
      # score - runners on 1st (fp), 2nd, & 3rd (if exist) score
      score = score + sum(runners); 
      runners[1,]=rep(0,3); # Bases cleared
      
      gameDescr = "D & 1st Base (fp) Scored";
      
      # Adjust fpTracker: Completely cleared
      fpTracker[1,]=rep(0,3) # All Fast Player off base
      stTracker[1,] = rep(0,3); # All Stealers off base 
    }else if (fpTracker[1] == 1 & O == 0)
    {
      fpTracker[1,3] = 1
      fpTracker[1,1] = 0
    }else
    {
      fpTracker[1,2:3] = rep(0,2)
      stTracker[1,2:3] = rep(0,2)
    }
    
    # Adjust remaining runners in accordance to D.
    r = runnerMovement(hitResult = "D",runners = runners,score=score) 
    runners = r$runners; 
    score = r$score;

     
    # If atBat player hits D is fp, then fp on 2md Base
    if(fp_atBat == 1)
    { fpTracker[1,2]=1 
    }else
    { fpTracker[1,2]=0
    }
    
  }else if (hitResult == "TR")
  {# HITS TRIPLE (TR)
    
    gameDescr = "TR";
    
    r = runnerMovement(hitResult = "TR",runners = runners, score=score) 
    runners = r$runners; 
    score = r$score;
    
    stTracker[1,] = rep(0,3); # All Stealers off base 
    fpTracker[1,] = rep(0,3); # All Fast Players off Base
    
    # If atBat player hits TR is fp, then fp on 3rd Base
    if(fp_atBat == 1)
    { fpTracker[1,3]=1 
    }else
    { fpTracker[1,3]=0
    }
    
  }else if (hitResult == "HR")
  {# HITS HOME RUN (HR)
    gameDescr = "HR";
    r = runnerMovement(hitResult = "HR",runners = runners, score=score) 
    runners = r$runners; 
    score = r$score;
    
    stTracker[1,] = rep(0,3); # All Stealers off base 
    fpTracker[1,] = rep(0,3); # All Fast Players off Base
    
  }else if (hitResult == "W")
  { # WALKED
    gameDescr = "W";
    
    # Tracking Stealer 
    if (stTracker[1,2] == 1 & runners[1,1] == 0)
    {# Stealer on 2nd Base and No runner on 1st.

        stTracker[1,2] = 1; # Account for W, but  stealer on 2nd Base.
      
    }else if (stTracker[1,2] == 1 & runners[1,1] == 1 )
    {
      # Stealer moves to 3rd base.
      stTracker[1,2] = 0; stTracker[1,3] = 1;
      if (fpTracker[1,1] == 1)
      {
        fpTracker[1,2] = 1
      }
      
    }else if (stTracker[1,3] == 1 & all(runners==1)) 
    {# Stealer on 3rd, bases loaded.
      
        stTracker[1,3] = 0;# Stealer on 3rd Base moves to HR.
        if (fpTracker[1,2] == 1)
        {
          fpTracker[1,3] = 1; # fp on 2nd bases goes to 3rd
          fpTracker[1,2] = 0; # fp on 2nd bases goes away
        } # fp on 2nd bases goes to 3rd
        if (fpTracker[1,1] == 1)
        {
          fpTracker[1,2] = 1 # fp on 1st bases goes to 2nd
        } 
      
    }else
    {
      # Tracking OnBase - fp 
      if (all(fpTracker == 1) | (fpTracker[1,1] == 1 & fpTracker[1,2] == 1))
      { # If runners on 1st and 2nd base runners  
        fpTracker[1,2:3] = rep(1,2) 
      }else if (all(runners == 1) & fpTracker[1,1] == 1 & fpTracker[1,3] == 1)
      { # Bases-Load, 1st and 3rd - fp
        fpTracker[1,2] = 1;
        fpTracker[1,3] = 0;
      } else if (fpTracker[1,1] == 1)
      { # If fp runner on 1st Base, then moves to 2nd base
        fpTracker[1,2] = 1
      }else if (fpTracker[1,2] == 1 & runners[1,1]==1)
      { # If runner on 2nd Base, then moves to 3rd base
        fpTracker[1,3] = 1; fpTracker[1,2] = 0;
      }else if (fpTracker[1,3] == 1 & all(runners == 1))
      {
        fpTracker[1,3] = 0;
      }
    }
    
    # Adjust remaining runners in accordance to W.
    r = runnerMovement(hitResult = "W", runners = runners, score = score)
    runners = r$runners; 
    score = r$score;
    
    # If atBat player W is fp, then fp on 1st Base
    if(fp_atBat == 1)
    { fpTracker[1,1]=1 
    }else
    { fpTracker[1,1]=0
    }
    
  }else if (hitResult == "O")
  {# OUT
    gameDescr = "O";
    O = O + 1; # Increase outs by 1
  }
  
  # Adjust stealWalkCt. 
  if (hitResult != "W")
  {
    stealWalkCt = 0;
  }
  
  if(isTRUE(graphic))
  {
    movementPlot(atBat, nextAtBat,runners=runners, gameDescr, O , score, numberAtBat,
                 fpTracker,stTracker) 
  }
  
  return(list(runners = runners,score = score,O = O,hitResult=hitResult,numberAtBat=numberAtBat,
              gameDescr = gameDescr,fpTracker=fpTracker,stTracker=stTracker))
  
}


# ----------------------------------------------
# Function Name: 
#   atBatTracker

# Function Description: 
#   Given the number of AtBats, the function outputs the current 
#   atBat player and nextAtBat player index.

# Function Argument: 
#    numberAtBat (numeric) : The number of hits, that is at Bats.

# Function Output:
#   atBat_indx      : Current AtBat Players index
#   nextAtBat_indx  : Next AtBat Players index
#   prevAtBat_indx  : Previous AtBat Players index


atBatTracker <- function(numberAtBat)
{
  if (numberAtBat == 1)# Represent first player atBat
  { 
    atBat_indx = 1; nextAtBat_indx = 2;
    prevAtBat_indx = 1; # Techincally, not a player, but need input for algo.
    
  }else if (numberAtBat %% 9 == 0) # Represent the 9th player atBat
  {
    atBat_indx = 9; nextAtBat_indx = 1; prevAtBat_indx = 8;
  }else 
  {
    atBat_indx = numberAtBat - 9*floor(numberAtBat/9);
    nextAtBat_indx = atBat_indx + 1;
    if (atBat_indx == 1)
    {
      prevAtBat_indx = 9
    }else
    {
      prevAtBat_indx = atBat_indx - 1;
    }
  }
  
  # Output Arguments
  return(list(atBat_indx = atBat_indx, nextAtBat_indx = nextAtBat_indx,
              prevAtBat_indx = prevAtBat_indx))
  
}

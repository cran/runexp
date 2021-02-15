# Script Name: 
#   sim_plot_funcs.R

# Script Purpose:
#   This script contains the following plotting functions for the 
#   simulator: 
#
#   emptyPlot     : Plots empty softball field.
#   movementPlot  : Plots the runner on base situation 
#                   along with other various game details. 
#   newInningPlot : Plots empty softball field with various game 
#                   details to denote the beginning of a new 
#                   inning.
#   endInningPlot : Plots empty softball field with various game 
#                   details to denote the end of an inning.


# -----------------------------------------------------------------
# Function Name: 
#   emptyPlot

# Function Description: 
#   In the plots window, plots empty softball field.

# Function Argument:
#   title : A character string denoting the title of the graphic. 
#           DEFAULT = "Softball Game Simulation"

#   color : A color to indicate the color of plot. DEFAULT = "black".


# Function Output:
#   Plots an empty softball field with ot Outs and atBat hitting to
#   denote the start of an inning.

# Function Examples:
#  emptyPlot() #DEFAULT
#  emptyPlot(color='blue')

emptyPlot= function(title="Softball Game Simulation", color="black")
{
  
  # Drawing the Empty Plot with Title 
  plot(0,type='n',xlim = c(-1.20,1.20),ylim = c(-.10, 2.10),xaxt='n',yaxt='n',
       main = title, family ='serif',xlab = '',ylab='')
  
  # Drawing Home Plate
  rect(xleft= -.15, ybottom = -.10, xright=.15,ytop = .10,border = color,lwd=2) # HomePlate
  text(0,0,label = c('HP'),family = 'serif',col=color)
  
  # Drawing 1st Base 
  rect(xleft = (1-.15),ybottom = (1-.10),xright = (1+.15), ytop = (1+.10),border = color,lwd=2)
  text(1,1,label = c('1B'),family='serif',col=color)
  
  # Drawing 2nd Base
  rect(xleft = (-.15),ybottom = (2-.10),xright = (.15), ytop = (2+.10),border = color,lwd=2)
  text(0,2,label = c('2B'),family='serif',col=color)
  
  # Drawing 3rd Base 
  rect(xleft = (-1-.15),ybottom = (1-.10),xright = (-1+.15), ytop = (1+.10),border = color,lwd =2)
  text(-1,1,label = c('3B'),family='serif',col = color)
  
  # Connecting the Bases with lines
  segments(.15,.10,1-.15,1-.10,col=color) # HP -> 1B
  segments((1-.15),(1+.10),.15,(2-.10),col=color) # 1B -> 2B
  segments((-.15),(2-.10),(-1+.15),(1+.10),col=color ) # 2B -> 3B
  segments((-1+.15),(1-.10),-.15,.10,col=color) # 3B - > HP
  
}


# -----------------------------------------------------------------
# Function Name: 
#   movementPlot

# Function Description: 
#   In the Plots window, plots the runners on base situation (black), 
#   atBat player hit result, next atBat player, number of outs, score, 
#   number of atBats. Additionally, tracks steal player and fast 
#   players with different colors.

# Function Argument:
#   atBat     : the current at Bat player. character string
#   nextAtBat : the next at Bat player. character string
#   runners   : tracks the runner on base situation. A 1 x 3 vector of 0 and 1 
#               where 1 denotes player on base, 0 denotes no player on 1st, 2nd, 
#               and 3rd base
#   hitResult: the type of play that occurred for atBat player. character string

#   O        : the number of outs. integer/numeric. DEFAULT = 0
#   score    : the Score of the game. integer/numeric DEFAULT = 0
#   numberAtBat: represents the number of hits (or at bats) 
#                              in the inning/game. integer/numeric. DEFAULT = 0

#   fpTracker: tracks the location of fast players. A 1 x 3 vector of 0 and 1 
#               where 1 denotes fast player on base, 0 denotes no fast player on base.
#               DEFAULT = matrix(0,ncol = 3).
#   stTracker: tracks the location of players who stole base. A 1 x 3 vector of 0 and 1 
#               where 1 denotes fast player on base, 0 denotes no fast player on base.
#               DEFAULT = matrix(0,ncol = 3).
                            
#   color     : the overall color of the outputted figure including non-stealer 
#               and non-fast player location; DEFAULT = "black"
#   color_fp  : the color that denotes a fast player on base. DEFAULT = "darkorange1"
#   color_st  : the color that denotes a player stole base. DEFAULT = "deepskyblue"
#   ...       : Other plotting functions provided by empty plot

# Function Output:
#   Plots the runner situation with the specific game updates inputted.

# Function Examples:
# movementPlot(atBat = "Player 1", nextAtBat = "Player 2",runners = matrix(c(1,0,0),ncol=3),
#            hitResult = "1B") #DEFAULT

# movementPlot(atBat = "Player 3", nextAtBat = "Player 4",runners = matrix(c(1,1,0),ncol=3),
#             hitResult = "1B",O = 1, score = 2, numberAtBat = 3,
#             fpTracker = matrix(c(1,0,0),ncol = 3),stTracker = matrix(c(0,1,0),ncol = 3)) 

###################### (2) PlotRunMove ######################################
movementPlot = function(atBat,nextAtBat,runners,hitResult,
                        O = 0,score = 0,numberAtBat = 0,
                        fpTracker = matrix(0,ncol = 4), stTracker = matrix(0,ncol = 4), 
                        color="black",color_st = "deepskyblue",color_fp = "darkorange1",...)
{
  # Plotting an empty softball field
  emptyPlot(color = color,...); 
  
  # Adding Descriptive Text: Play Results, Next at Bat, 
  # # of Outs, Score, and # of At Bats. 
  text(0,1.25,label = paste('Player - Result: \n',atBat,'-',hitResult),
       family= 'serif', col = color, font = 2) 
  
  text(0,.95,label = paste('\n At Bat:',nextAtBat,'\n  Outs:',O,'| Score:',score),
       family= 'serif',col = color ,font = 2)
  text(-.8,0,label =paste('# of At Bats:',numberAtBat),family='serif',col= color,font = 2)
  
  
  # Shading the Bases based on the Runner on Base (RB) variable 
  #   and whether the player stolen, the fast player, or regular player.
  
  # Runner on 1st Base
  if (runners[1,1] == 1)
  {
    if (fpTracker[1,1]==1)
    { # If Fast Player on Base,
      rect(xleft = (1-.15),ybottom = (1-.10),xright = (1+.15), ytop = (1+.10),col =color_fp) 
      text(1,1,label = c('F'),family='serif')
    }else
    { # If non-fast player
      rect(xleft = (1-.15),ybottom = (1-.10),xright = (1+.15), ytop = (1+.10),col =color)
    }
  }
  
  # Runner on 2nd Base
  if (runners[1,2] == 1)
  { 
    if(stTracker[1,2] == 1)
    { # If Player Stolen Base,
      rect(xleft = (-.15),ybottom = (2-.10),xright = (.15), ytop = (2+.10),col =color_st)
      text(0,2,label = c('S'),family='serif')
    }else if(fpTracker[1,2] == 1)
    { # If Fast Player 
      rect(xleft = (-.15),ybottom = (2-.10),xright = (.15), ytop = (2+.10),col =color_fp)
      text(0,2,label = c('F'),family='serif')
    }else
    { # If non-stealer or non-fast player
      rect(xleft = (-.15),ybottom = (2-.10),xright = (.15), ytop = (2+.10),col = color)
    }
  }  
  
  # Runner on 3rd Base
  if (runners[1,3] == 1)
  {
    if(stTracker[1,3] == 1)
    { # If Player Stolen Base,
      rect(xleft = (-1-.15),ybottom = (1-.10),xright = (-1+.15), ytop = (1+.10),col = color_st)  
      text(-1,1,label = c('S'),family='serif')
    }else if (fpTracker[1,3] == 1)
    { # If Fast Player
      rect(xleft = (-1-.15),ybottom = (1-.10),xright = (-1+.15), ytop = (1+.10),col =color_fp)
      text(-1,1,label = c('F'),family='serif')
    }else
    { # If non-stealer or non-fast player
      rect(xleft = (-1-.15),ybottom = (1-.10),xright = (-1+.15), ytop = (1+.10),col =color)
    }
  }
}

# -----------------------------------------------------------------
# Function Name: 
#   newInningPlot

# Function Description: 
#   In the Plots window, plots an empty softball field with various game 
#   details to denote the beginning of a new inning. Using in the game 
#   simulation function.

# Function Argument:
#   inn       : the inning number. integer/numeric
#   atBat     : the current at Bat player. character string
#   score     : the Score of the game. integer/numeric DEFAULT = 0
#   color     : the overall color of the plot. DEFAULT = "blue3"
#   ...       : Other plotting functions provided by empty plot

# Function Output:
#   Plots an empty softball field with the specific inning, atBat player, score 

# Function Examples:
# newInningPlot(inn = 3, atBat = "Anne Francis") # DEFAULT
# newInningPlot(inn = 4, atBat = "Anne Francis", score = 10, color="red") 

newInningPlot = function(inn,atBat,score = 0,color='blue3',...)
{
  
  # Plotting an empty softball field
  emptyPlot(color = color,...);
  
  # Adding Descriptive Text: Inning Number, score, at Bat players
  text(0,1,label = paste('Inning #:',inn,'\n Score:',score,'\n At Bat:',atBat),
       family= 'serif',col = color,font = 2)
  
}

# -----------------------------------------------------------------
# Function Name: 
#   endInningPlot

# Function Description: 
#   In the Plots window, plots an empty softball field with various game 
#   details to denote the end of an inning. Using in the game 
#   simulation function.

# Function Argument:
#   inn       : the inning number. integer/numeric
#   score     : the Score of the game. integer/numeric DEFAULT = 0
#   numberAtBat: represents the number of hits (or at bats) in the inning/game. 
#               integer/numeric. DEFAULT = 0
#   color     : the overall color of the plot. DEFAULT = "firebrick2"
#   ...       : Other plotting functions provided by empty plot

# Function Output:
#   Plots an empty softball field with the specific inning, atBat player, 
#   and score for the end of an inning. 

# Function Examples:
# endInningPlot(inn = 3, score = 10) # DEFAULT
# endInningPlot(inn = 4, score = 3, numberAtBat = 4, color="cyan") 

endInningPlot = function(inn,score,numberAtBat = 0,color='firebrick2',...)
{
  
  # Plotting an empty softball field
  emptyPlot(color = color,...);
  
  # Adding Descriptive Text: Inning, Score, # of AtBats,
  text(0,1,label = paste('END of Inning:',inn,'\n Score:',score,'\nNumber of Hits:',numberAtBat),
       family= 'serif',col=color,font=2)
  
}






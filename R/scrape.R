# Contents --------------------------------------------------------------------
# External Functions
#   scrape: web-scraper

# scrape Function ----------------------------------------------------------------

#' @title Softball Webscraper
#' @description Scrapes the player statistics from a given URL. 
#'
#' @param url the web address of the teams' data to scrape
#' 
#' @return A dataframe consisting of each player's Number, NAME, AVG, 
#' OPS, AB, R, H, 2B, 3B, HR, RBI, TB, SLG% BB, HBP, SO, GDP, OB% SF, SH,
#' SB, ATT, GP, and GS.
#'     
#' @examples 
#' url <-"https://wmubroncos.com/sports/softball/stats/2019"
#' test <- scrape(url)
#' test_probs <- prob_calc(test)
#'         
#' @export


scrape <- function(url)
{
  ## Libraries Needed: 
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Package \"rvest\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package \"xml2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  ########## Webscraping ############
  url <- xml2::read_html(url)
  table <- rvest::html_nodes(url, "table")
  sbData <- rvest::html_table(table)[[1]]
  
  ########## Cleaning Data ############
  
  # Remove Bio Links Column
  if ("Bio Link" %in% colnames(sbData) )
  {
    sbData$`Bio Link` <- NULL
  }
  
  # Change # column to Number
  if ("#" %in% colnames(sbData) )
  {
    colnames(sbData)[which("#" == colnames(sbData))] = "Number"
  }
  
  # Clean the Player Names:
  name <- strsplit(sbData$Player,split = '\r\n') # String Split
  for (i in 1:dim(sbData)[1])
  {
    sbData$Player[i]<- gsub(" ","",name[[i]][1],fixed = TRUE)
  }
  
  ## Break GP-GS into two columns and Break SB-ATT into two columns
  GP = GS = rep(0,dim(sbData)[1]) # Allocate new variables
  gp_gs = strsplit(sbData$`GP-GS`,split = "-") # String Split
  
  SB = ATT = rep(0,dim(sbData)[1]) # Allocate new varibles
  sb_att = strsplit(sbData$`SB-ATT`,split = "-") # String Split
  
  for (i in 1:dim(sbData)[1])
  {
    GP[i]<- as.numeric(gp_gs[[i]][1])
    GS[i]<- as.numeric(gp_gs[[i]][2])
    
    SB[i]<- as.numeric(sb_att[[i]][1])
    ATT[i]<- as.numeric(sb_att[[i]][2])
  }
  
  ## Remove SB-ATT from sbData and add SB and ATT
  sbData$`GP-GS` <- NULL
  sbData$`SB-ATT` <- NULL
  sbData = cbind(sbData,SB,ATT,GP,GS)
  
  # Change Player Column Name to NAME
  colnames(sbData)[which("Player" == colnames(sbData))] = "NAME"
  # Return data.frame
  return(sbData)
}


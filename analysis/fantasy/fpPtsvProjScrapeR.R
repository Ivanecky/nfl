library(rvest)
library(httr)
library(tidyr)
library(stringr)
library(dplyr)
library(kit)

# Function to get player links
getPlyrLnks <- function(url, scoring_method = "") {
  # Set scoring method
  scoring_method <- case_when(
    scoring_method == "PPR" ~ "?scoring=PPR",
    scoring_method == "Half" ~ "?scoring=HALF",
    scoring_method == "Standard" ~ "?scoring=STD",
    T ~ "" # Default case for QBs
  )
  
  # Get html
  html <- url %>%
    read_html()
  
  # Get player links
  plyrLnks <- html %>%
    html_nodes(xpath = "//tbody/tr/td/a") %>%
    html_attr("href")
  
  # Manipulate strings
  for ( i  in 1:length(plyrLnks) )
  {
    temp = plyrLnks[i]
    #temp = substring(temp, 3)
    temp = paste0("https://www.fantasypros.com", temp, scoring_method)
    plyrLnks[i] = temp
  }
  
  # Keep only those with for players
  plyrLnks <- plyrLnks[grepl("nfl/projections", plyrLnks)]
  
  # Return data
  return(plyrLnks)
}

# Function to get player data
getPlyrData <- function(url) {
  # Get html tables
  plyrHtml <- url %>%
    read_html() 
  
  plyrTbl <- plyrHtml %>%
    html_table()
  
  # Get proj v actual table
  projAct <- as.data.frame(plyrTbl[3])
  
  # Get player name
  plyrName <- plyrHtml %>%
    html_node(xpath = "//div/div[1]/div/div[1]/div[1]/div[1]/div[2]/h1") %>%
    html_text()
  
  # Create dataframe
  # Rename columns for scoring
  # Check num cols 
  if(ncol(projAct) == 0) {
    print(paste0("Error getting data for :", url))
    return(NA)
  }
  
  names(projAct) <- c("Week", "Proj.Pts", "Act.Pts", "Var", "Exceeded")
  # Add player name
  projAct <- projAct %>%
    mutate(
      Name = plyrName
    )
  
  # Return data
  return(projAct)
}

# Function to get overall links
getOverallList <- function(url) {
  # Get html
  html <- url %>%
    read_html()
  
  # Get player links
  plyrLnks <- html %>%
    html_nodes(xpath = "//tbody/tr/td/a") %>%
    html_attr("href")
  
  # Get table
  plyrTbl <- html %>%
    html_table()
  
  plyrTbl <- as.data.frame(plyrTbl[1])
  
  # Extract position vector
  pos <- plyrTbl$Position
  
  # Manipulate strings
  for ( i  in 1:length(plyrLnks) )
  {
    # Set scoring method
    scoring_method <- case_when(
      pos[i] %in% c("RB", "WR", "TE") ~ "?scoring=PPR",
      T ~ "" # Default case for QBs
    )
    
    temp <- plyrLnks[i]
    #temp = substring(temp, 3)
    temp <- paste0("https://www.fantasypros.com", temp, scoring_method)
    temp <- gsub("players", "projections", temp)
    plyrLnks[i] <- temp
  }
  
  # Filter to only projections
  plyrLnks <- plyrLnks[grepl("projections", plyrLnks)]
  
  # Return
  return(plyrLnks)
}

# Define URLs to scrape
qb <- "https://www.fantasypros.com/nfl/projections/qb.php"
rb <- "https://www.fantasypros.com/nfl/projections/rb.php?scoring=STD" # The type of scoring here doesn't matter.
wr <- "https://www.fantasypros.com/nfl/projections/wr.php?scoring=STD"
te <- "https://www.fantasypros.com/nfl/projections/te.php?scoring=STD"
dst <- "https://www.fantasypros.com/nfl/projections/dst.php?scoring=STD"
k <- "https://www.fantasypros.com/nfl/projections/k.php?scoring=STD"

# Overall players list
overall <- "https://www.fantasypros.com/nfl/reports/leaders/?year=2021"

# Group links
links <- c(qb, rb, wr, te, dst, k)

# Iterate over and get links
plyrLnks <- getOverallList(overall)
plyrLnks <-funique(plyrLnks)

# Get DST links
plyrLnks <- append(plyrLnks, getPlyrLnks(dst))

# for( i in 1:length(links) ) {
#   if(i >= 2 & i <= 4) {
#     # Get links
#     tempLinks <- getPlyrLnks(links[i], scoring_method = "PPR")
#   } else {
#     # Get links
#     tempLinks <- getPlyrLnks(links[i])
#   }
#   # Append to vector
#   plyrLnks <- append(plyrLnks, tempLinks)
# }

# Get data
for( i in 1:length(plyrLnks) ) {
  if(!exists("plyrDf")) {
    plyrDf <- getPlyrData(plyrLnks[i])
  } else {
    plyrDf <- rbind(plyrDf, getPlyrData(plyrLnks[i]))
  }
}

# Clean up df
plyrDf <- plyrDf %>%
  mutate(
    Name = sub("^(\\S*\\s+\\S+).*", "\\1", Name),
    Week = as.numeric(gsub("Week ", "", Week)),
    Proj.Pts = as.numeric(Proj.Pts),
    Act.Pts = as.numeric(Act.Pts),
    Var = as.numeric(Var)
  ) 

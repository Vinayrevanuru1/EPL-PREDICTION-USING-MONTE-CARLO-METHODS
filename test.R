
dat = read.csv("EPL_set.csv")


# find all the teams in the EPL

teams = unique(dat$HomeTeam)

teams

length(teams)

#calculating the home win percentage for each team

homeWin = function(team) {

  # find all the games where the team is the home team

  homeGames = dat[dat$HomeTeam == team, ]

  # find the number of games where the team won

  homeWins = sum(homeGames$FTR == "H")

  # find the total number of games

  totalGames = nrow(homeGames)

  # calculate the percentage of wins

  homeWinPct = homeWins / totalGames

  return(homeWinPct)

}


#calculating the away win percentage for each team

awayWin = function(team) {

  # find all the games where the team is the away team

  awayGames = dat[dat$AwayTeam == team, ]

  # find the number of games where the team won

  awayWins = sum(awayGames$FTR == "A")

  # find the total number of games

  totalGames = nrow(awayGames)

  # calculate the percentage of wins

  awayWinPct = awayWins / totalGames

  return(awayWinPct)

}

awayWin("Arsenal")

#calculating the Home draw percentage for each team

homeDraw = function(team) {

  # find all the games where the team is the home team

  homeGames = dat[dat$HomeTeam == team, ]

  # find the number of games where the team won

  homeDraws = sum(homeGames$FTR == "D")

  # find the total number of games

  totalGames = nrow(homeGames)

  # calculate the percentage of wins

  homeDrawPct = homeDraws / totalGames

  return(homeDrawPct)

}

#calculating the Away draw percentage for each team

awayDraw = function(team) {

  # find all the games where the team is the away team

  awayGames = dat[dat$AwayTeam == team, ]

  # find the number of games where the team won

  awayDraws = sum(awayGames$FTR == "D")

  # find the total number of games

  totalGames = nrow(awayGames)

  # calculate the percentage of wins

  awayDrawPct = awayDraws / totalGames

  return(awayDrawPct)

}

#calculating home win percentage for each team against each team

homeWinPct = function(team1, team2) {

  # find all the games where team1 is the home team

  homeGames = dat[dat$HomeTeam == team1, ]

  # find all the games where team2 is the away team

  homeGames = homeGames[homeGames$AwayTeam == team2, ]

  # find the number of games where team1 won

  homeWins = sum(homeGames$FTR == "H")

  # find the total number of games

  totalGames = nrow(homeGames)

  # calculate the percentage of wins

  homeWinPct = homeWins / totalGames

  return(homeWinPct)

}

#calculating away win percentage for each team against each team

awayWinPct = function(team1, team2) {

  # find all the games where team1 is the away team

  awayGames = dat[dat$AwayTeam == team1, ]

  # find all the games where team2 is the home team

  awayGames = awayGames[awayGames$HomeTeam == team2, ]

  # find the number of games where team1 won

  awayWins = sum(awayGames$FTR == "A")

  # find the total number of games

  totalGames = nrow(awayGames)

  # calculate the percentage of wins

  awayWinPct = awayWins / totalGames

  return(awayWinPct)

}


#calculating home draw percentage for each team against each team

homeDrawPct = function(team1, team2) {

  # find all the games where team1 is the home team

  homeGames = dat[dat$HomeTeam == team1, ]

  # find all the games where team2 is the away team

  homeGames = homeGames[homeGames$AwayTeam == team2, ]

  # find the number of games where team1 won

  homeDraws = sum(homeGames$FTR == "D")

  # find the total number of games

  totalGames = nrow(homeGames)

  # calculate the percentage of wins

  homeDrawPct = homeDraws / totalGames

  return(homeDrawPct)

}




# create a matrix with team names as row and column names

homewinMatrix = matrix(0, nrow = length(teams), ncol = length(teams))

colnames(homewinMatrix) = teams

rownames(homewinMatrix) = teams

# fill in the matrix with the home win percentages

for (i in 1:length(teams)) {

  for (j in 1:length(teams)) {

    homewinMatrix[i, j] = homeWinPct(teams[i], teams[j])

  }

}

homewinMatrix

# fill in the matrix with the away win percentages

awaywinMatrix = matrix(0, nrow = length(teams), ncol = length(teams))

colnames(awaywinMatrix) = teams

rownames(awaywinMatrix) = teams

for (i in 1:length(teams)) {

  for (j in 1:length(teams)) {

    awaywinMatrix[i, j] = awayWinPct(teams[i], teams[j])

  }

}

awaywinMatrix

# fill in the matrix with the home draw percentages

homedrawMatrix = matrix(0, nrow = length(teams), ncol = length(teams))

colnames(homedrawMatrix) = teams

rownames(homedrawMatrix) = teams

for (i in 1:length(teams)) {

  for (j in 1:length(teams)) {

    homedrawMatrix[i, j] = homeDrawPct(teams[i], teams[j])

  }

}

homedrawMatrix






# create dataframe with team1 and team2 as two columns and the home win percentage as the third column

PctMatrix = data.frame(team1 = 0 , team2 = 0, homeWinPct = 0, awayWinPct = 0, homeDrawPct = 0)



# fill in the home win percentage for each team against each team

for ( i in teams) {

  for ( j in teams) {

    PctMatrix = rbind(PctMatrix, data.frame(team1 = i, team2 = j, homeWinPct = homeWinPct(i, j), awayWinPct = awayWinPct(i, j), homeDrawPct = homeDrawPct(i, j)))

  }

}
# save the dataframe to a csv file

write.csv(PctMatrix, "PctMatrix.csv")

# read in the data
epl= read.csv("epl-2022-utc.csv")
# add a column for the home win percentage
# add a column for the away win percentage
# add a column for the home draw percentage
epl$homeWinPct = 0
epl$awayWinPct = 0
epl$homeDrawPct = 0
#add columns with 
for(i in 1:380) {
  epl$homeWinPct[i] = PctMatrix$homeWinPct[PctMatrix$team1 == epl$HomeTeam[i] & PctMatrix$team2 == epl$AwayTeam[i]]
  epl$awayWinPct[i] = PctMatrix$awayWinPct[PctMatrix$team1 == epl$AwayTeam[i] & PctMatrix$team2 == epl$HomeTeam[i]]
  epl$homeDrawPct[i] = PctMatrix$homeDrawPct[PctMatrix$team1 == epl$HomeTeam[i] & PctMatrix$team2 == epl$AwayTeam[i]]
}
#impute missing values with average win percentage for each team

for(i in 1:380) {
  
  if(is.na(epl$homeWinPct[i])) {
    epl$homeWinPct[i] = mean(epl$homeWinPct[epl$HomeTeam[i] == epl$HomeTeam])
  }
  if(is.na(epl$awayWinPct[i])) {
    epl$awayWinPct[i] = mean(epl$awayWinPct[epl$AwayTeam[i] == epl$AwayTeam])
  }
  if(is.na(epl$homeDrawPct[i])) {
    epl$homeDrawPct[i] = mean(epl$homeDrawPct[epl$HomeTeam[i] == epl$HomeTeam])
  }
}
#impute missing values with average win percentage for each team  
i = 2
for( i in 1:380)
{
if(epl$homeWinPct[i]=='NaN')
{
  print(i)
}
}

for(i in 1:380) {
  
  if((epl$homeWinPct[i]=='NaN')) {
    epl$homeWinPct[i] = homeWin(team = epl$HomeTeam[i])
  }
  if((epl$awayWinPct[i]=='NaN')) {
    epl$awayWinPct[i] = awayWin(team = epl$AwayTeam[i])
  }
  if((epl$homeDrawPct[i]=='NaN')) {
    epl$homeDrawPct[i] = homeDraw(team = epl$HomeTeam[i])
  }
}

write.csv(epl, "epl-2022-utc-2.csv")




epl$homeWinPct = as.double(epl$homeWinPct)
epl$awayWinPct = as.double(epl$awayWinPct)
epl$homeDrawPct = as.double(epl$homeDrawPct)


epl$homeWinPct=round((epl$homeWinPct+1)/4,2)
epl$awayWinPct=round((epl$awayWinPct+1)/4,2)
epl$homeDrawPct=round((epl$homeDrawPct+1)/4,2)

epl$homeOdds = 0
epl$awayOdds = 0
epl$drawOdds = 0

epl$homeOdds = round(as.double(1/epl$homeWinPct),2)
epl$awayOdds = round(as.double(1/epl$awayWinPct),2)
epl$drawOdds = round(as.double(1/epl$homeDrawPct),2)


# save the dataframe to a csv file

write.csv(epl, "epl-2022.csv")





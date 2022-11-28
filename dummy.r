library(data.table)
library(stringr)
library(DT)


dat = read.csv("epl-2022.csv")

set.seed(12345) # set seed for reproducibility

sample_dat = dat[sample(nrow(dat), 5), ]

sample_dat
datatable(sample_dat)

pm_fix = read.csv('epl', header = TRUE, sep = ',')

team_names = unique((pm_fix$`HomeTeam`))

calculate_points = function(pl_fix = pl_fix){
    
    round_table = data.table(team = team_names, point = 0, matches_played = 0)
    
    for (row in c(1:nrow(pl_fix))){
      
      
      
      result = pl_fix$hda[row]
      result = as.character(result)
      result
      
      home_team_name = pl_fix$`HomeTeam`[row]
      home_team_name
      home_team_matched_played = max(round_table[team == home_team_name]$matches_played)
      home_team_matched_played
      home_team_point_before = round_table[team == home_team_name & matches_played == home_team_matched_played]$point
      home_team_point_before
      home_team_point = ifelse(is.na(result), home_team_point_before + 0,
                                ifelse(result == "h", home_team_point_before + 3,
                                       ifelse(result == "d", home_team_point_before + 1,
                                              home_team_point_before + 0)))
      home_team_point    
      away_team_name = pl_fix$`AwayTeam`[row]
      away_team_name  
      away_team_matched_played = max(round_table[team == away_team_name]$matches_played)
      away_team_point_before = round_table[team == away_team_name & matches_played == away_team_matched_played]$point
      
      away_team_point = ifelse(is.na(result), away_team_point_before + 0,
                                ifelse(result == "a", away_team_point_before + 3,
                                       ifelse(result == "d", away_team_point_before + 1, away_team_point_before + 0)))
      away_team_point    
      round_table =rbindlist(list(round_table, data.table(matches_played = home_team_matched_played + 1,
                                                               team = home_team_name,
                                                           point = home_team_point)),
                              fill = T)
      
      round_table =rbindlist(list(round_table, data.table(matches_played = away_team_matched_played + 1,
                                                           team = away_team_name,
                                                           point = away_team_point)),
                              fill = T)
      
      
    }
    
    return(round_table)

  }


simulate_matches = function(fixture_table = pm_fix, times = 100){
  output_table = data.table()
  
  for (simulation_number in c(1:times)){
    
    for(row_number in c(1:nrow(fixture_table))){
      
      match_row = fixture_table[row_number,]
      match_row 
      outcome = sample(c("h", "d", "a"),
                        size = 1,
                        replace = TRUE,
                        prob = c(100*match_row$homeWinPct, 100*match_row$awayWinPct[], 100*match_row$homeDrawPct))
      100*match_row$homeWinPct
      match_row$hda = outcome
      match_row$simulation_id = simulation_number
      # match_row$simulation_number = simulation_number
      
      
      output_table = rbindlist(list(output_table, match_row), fill = T)
      
    }
    
  }
  
  return(output_table)
  
}

nr_simlation = 300

#run it
simulated_matches_data = simulate_matches(times = nr_simlation)

#show table
datatable(simulated_matches_data[1000:1200])


simulate_standings = function(all_matches = simulated_matches_data){
  
  standings_all = data.table()
  
  for (round in c(1:max(all_matches$simulation_id))){
    
    one_simulation = all_matches[simulation_id == round]
    
    simulated_fixtures = calculate_points(one_simulation)
    simulated_fixtures[, simulation_id:= round]
    
    standings_all = rbindlist(list(standings_all, simulated_fixtures), fill = T)
    
  }
  
  return(standings_all)
  
}

simulated_standings = simulate_standings()

datatable(simulated_standings[1000:1200])



last_round_standing = simulated_standings[matches_played == 38]
last_round_standing = last_round_standing[order(simulation_id, -point)]
#write the standings to each of the simulations
last_round_standing[, standing:= c(1:.N), by = simulation_id]

number_of_occ = table(last_round_standing$team, last_round_standing$standing)
number_of_occ



percent_table = round(table(last_round_standing$team, last_round_standing$standing)/nr_simlation * 100, 1)
percent_table


odds_table = (1 / ((table(last_round_standing$team, last_round_standing$standing)/ nr_simlation)))
odds_table

liv_city = data.table(odds_table)[V1 %in% team_names & V2 == 1]
setnames(liv_city, old = c("V1", "V2", "N"), new = c("Team", "Position", "Implied_odds_for_first"), skip_absent = T)
liv_city
#sort  the data by third column
liv_city = liv_city[order(Implied_odds_for_first)] 
#add a column with the rank
liv_city[, rank := c(1:.N)]


#show the table
datatable(liv_city)

#plot the data

library(ggplot2)

ggplot(liv_city, aes(x = Team, y = Implied_odds_for_first)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Implied odds for first place in the Premier League",
       subtitle = "Based on 300 simulations",
       x = "Team",
       y = "Implied odds for first place")

library(jsonlite)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(stringr)
library(rvest)
library(XML)
library(RCurl)
library(rjson)
library(png)
library(baseballr)
library(dplyr)
library(lubridate)

if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("BillPetti/baseballr")

date <- Sys.Date()

#TeamAbb <- read.csv("https://gist.githubusercontent.com/purp/cb71552e6313a832ec3c/raw/48204f9a270e5e12e2105e7ab82859de8800b24c/schedule_urls.csv")


AL_Standings <- bref_standings_on_date(date, "AL Overall", from = FALSE) %>% 
  mutate(League = "AL")
NL_Standings <- bref_standings_on_date(date, "NL Overall", from = FALSE) %>% 
  mutate(League = "NL")

AL_East_Standings <- bref_standings_on_date(date, "AL East", from = FALSE)

MLB_Standings <- rbind(NL_Standings, AL_Standings) %>% 
  mutate(DIFF = RS - RA)

BlueJays_TeamStats <- bref_daily_batter("2022-04-07", "2022-10-05") %>% 
  filter(Team == "Toronto") %>% 
  mutate(Verifed = PA / G) %>% 
  #filter(Verifed >= 3.10) %>% 
  select(Name, Age, G, PA, AB, R, H, X1B,
         X2B, X3B, HR, RBI, BB, IBB, uBB,
         SO, HBP, GDP, SB, CS, BA, OBP,
         SLG, OPS) %>% 
  filter(PA > 0)
BlueJays_TeamStats[is.na(BlueJays_TeamStats)] <- 0

RedSox_TeamStats <- bref_daily_batter("2022-04-07", "2022-10-05") %>% 
  filter(Team == "Boston") %>% 
  mutate(Verifed = PA / G) %>% 
  #filter(Verifed >= 3.10) %>% 
  select(Name, Age, G, PA, AB, R, H, X1B,
         X2B, X3B, HR, RBI, BB, IBB, uBB,
         SO, HBP, GDP, SB, CS, BA, OBP,
         SLG, OPS) %>% 
  filter(PA > 0)
RedSox_TeamStats[is.na(RedSox_TeamStats)] <- 0

write.csv(BlueJays_TeamStats,
          file = "BlueJays_2022_PlayerStats.csv",
          row.names = F)

NewYorkYankees_TeamStats <- bref_daily_batter("2022-04-07", "2022-10-05") %>%
  filter(Level == "Maj-AL") %>% 
  filter(Team == "New York") %>% 
  mutate(Verifed = PA / G) %>% 
  #filter(Verifed >= 3.10) %>% 
  select(Name, Age, G, PA, AB, R, H, X1B,
         X2B, X3B, HR, RBI, BB, IBB, uBB,
         SO, HBP, GDP, SB, CS, BA, OBP,
         SLG, OPS)
NewYorkYankees_TeamStats[is.na(NewYorkYankees_TeamStats)] <- 0


Dodgers_TeamStats <- bref_daily_batter("2022-04-07", "2022-10-05") %>% 
  filter(Level == "Maj-NL") %>% 
  filter(Team == "Los Angeles")

BlueJays_Record <- bref_team_results("TOR", 2022) %>% 
  mutate(Total_Runs = R + RA) %>% 
  select(Date, Gm, Tm, Opp, H_A, R, RA, Total_Runs, Result, Record,
         GB, Win, Loss, Save, Time, `D/N`, Attendance,
         Streak)
BlueJays_Record[is.na(BlueJays_Record)] <- 0

CLE_Record <- bref_team_results("CLE", 2022) %>% 
  mutate(Total_Runs = R + RA) %>% 
  select(Date, Gm, Tm, Opp, H_A, R, RA, Total_Runs, Result, Record,
         GB, Win, Loss, Save, Time, `D/N`, Attendance,
         Streak)
CLE_Record[is.na(CLE_Record)] <- 0

write.csv(BlueJays_Record,
          file = "blue_jays_record_2022.csv",
          row.names = F)

BlueJays_HomeRecord <- BlueJays_Record %>% 
  filter(H_A == "H")
  
Dodgers_Record <- bref_team_results("LAD", 2022)


MLB_Player_Stats <- bref_daily_batter("2022-04-07", "2022-10-05") %>% 
  mutate(Verifed = PA / G) %>% 
  filter(Verifed >= 3.10) %>%
  #filter(G >= 100) %>% 
  select(Name, Verifed, Age, Level, 
         Team, G, PA, AB, BA, OBP, 
         SLG, OPS, R, H, X1B, X2B, 
         X3B, HR, RBI, BB, IBB, SO, 
         HBP, SB, CS)

MLB_RBI_Leaders <- MLB_Player_Stats %>% 
  select(Name, Level, Team, RBI)

MLB_H_Leaders <- MLB_Player_Stats %>% 
  select(Name, Level, Team, H)

MLB_HR_Leaders <- MLB_Player_Stats %>% 
  select(Name, Level, Team, HR)

MLB_AL_Player_Stats <- MLB_Player_Stats %>% 
  filter(Level == "AL")

write.csv(MLB_AL_Player_Stats,
          file = "MLB_AL_Player_Stats.csv",
          row.names = F)

MLB_NL_Player_Stats <- MLB_Player_Stats %>% 
  filter(Level == "NL")

write.csv(MLB_NL_Player_Stats,
          file = "MLB_NL_Player_Stats.csv",
          row.names = F)

MLB_Player_Stats$Level[MLB_Player_Stats$Level == "Maj-NL"] <- "NL"
MLB_Player_Stats$Level[MLB_Player_Stats$Level == "Maj-AL"] <- "AL"

write.csv(MLB_Player_Stats,
          file = "MLB_Stats_2022.csv",
          row.names = F)


MLB_Pitcher_Stats <- bref_daily_pitcher("2022-04-07", "2022-10-05") %>% 
  select(Name, Age, Level, Team, G, GS, W, L, SV, IP, H, 
         R, ER, WHIP, uBB, BB, SO, SO9, HR, ERA, AB, X1B, X2B, X3B, IBB)

BlueJays_Pitcher_Stats <- MLB_Pitcher_Stats %>% 
  filter(Team == "Toronto") %>% 
  filter(GS > 0)
BlueJays_Pitcher_Stats[is.na(BlueJays_Pitcher_Stats)] <- 0

write.csv(MLB_Pitcher_Stats,
          file = "MLB_Pitcher_Stats_2022.csv",
          row.names = F)


x <- get_probables_mlb(663249)

#game info (weather etc...)
xx <- mlb_game_info(game_pk = 663249)

#game batting order
Toronto <- mlb_batting_orders(game_pk = 663249) %>% 
  filter(teamName == "Toronto Blue Jays") %>% 
  select(fullName, abbreviation, batting_order,
         teamName) %>% 
  rename(Name = fullName, Position = abbreviation,
         `Batting Order` = batting_order,
         Team = teamName) %>% 
  select(`Batting Order`, Name, Position)

RedSox <- mlb_batting_orders(game_pk = 663249) %>% 
  filter(teamName == "Boston Red Sox") %>% 
  select(fullName, abbreviation, batting_order,
         teamName) %>% 
  rename(Name = fullName, Position = abbreviation,
         `Batting Order` = batting_order,
         Team = teamName) %>% 
  select(`Batting Order`, Name, Position)
  

# pitch by pitch stats
xyx <- mlb_pbp(game_pk = 663249) %>% 
  filter(type == "pitch") %>% 
  filter(batting_team == "New York Yankees")

write.csv(xyx,
          file = "BlueJay_Recent_PitchData.csv",
          row.names = F)


xyxy <- mlb_schedule(season = 2022, level_ids = "1") %>% 
  filter(game_type == "R") %>% 
  select(date, game_pk, teams_away_score, teams_away_team_name,
         teams_home_score, teams_home_team_name) %>% 
  rename(Date = date, GameID = game_pk,
         `Away Score` = teams_away_score,
         `Away Team` = teams_away_team_name,
         `Home Score` = teams_home_score,
         `Home Team` = teams_home_team_name) %>% 
  select(Date, GameID, `Away Team`, `Away Score`, `Home Team`,
         `Home Score`) %>% 
  filter(Date == date)


jays_home_reg_schedule <- mlb_schedule(season = 2022, level_ids = "1") %>% 
  filter(game_type == "R") %>% 
  filter(teams_home_team_name == "Toronto Blue Jays")

jays_away_reg_schedule <- mlb_schedule(season = 2022, level_ids = "1") %>% 
  filter(game_type == "R") %>% 
  filter(teams_away_team_name == "Toronto Blue Jays")

jays_reg_schedule <- rbind(jays_away_reg_schedule, jays_home_reg_schedule) %>% 
  select(date, teams_away_team_name, teams_home_team_name, venue_name) %>% 
  rename(Date = date, Away_Team = teams_away_team_name, 
         Home_Team = teams_home_team_name, Stadium = venue_name)


dodgers_home_reg_schedule <- mlb_schedule(season = 2022, level_ids = "1") %>% 
  filter(game_type == "R") %>% 
  filter(teams_home_team_name == "Los Angeles Dodgers")



dodgers_away_reg_schedule <- mlb_schedule(season = 2022, level_ids = "1") %>% 
  filter(game_type == "R") %>% 
  filter(teams_away_team_name == "Los Angeles Dodgers")


dodgers_reg_schedule <- rbind(dodgers_away_reg_schedule, dodgers_home_reg_schedule) %>% 
  select(date, teams_away_team_name, teams_home_team_name, venue_name) %>% 
  rename(Date = date, Away_Team = teams_away_team_name, 
         Home_Team = teams_home_team_name, Stadium = venue_name)





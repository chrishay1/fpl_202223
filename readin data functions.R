#functions for reading in the data
library(tidyverse)
library(lubridate)
library(RCurl)

#list of teams and results
#source for europe flags; https://en.wikipedia.org/wiki/English_football_clubs_in_international_competitions
#source for premier league tables; premierleague.com
#source for club elos ; clubelo.com
#source for player data; fantasy.premierleague.com. MOst of the data is sourced from an archive.
#see update_fpl_datanew.r for an example of how this data is scraped

team_names_full <- read_csv("C:/Users/pc/Dropbox/FPLData/team_names.csv")
team_names_full$Season <- as.character(team_names_full$Season)

read_input_data <-  function(x){
return( read_csv(paste0("C:/Users/pc/Dropbox/FPLData/player_data",x,".csv")))
}

read_metadata <-  function(x){
    return(read_csv(paste0("C:/Users/pc/Dropbox/FPLData/player_metadata",x,".csv")))
}

read_teamdata <-  function(x){
    return(read_csv(paste0("C:/Users/pc/Dropbox/FPLData/team_data",x,".csv")))
}


apply_player_positions <- function(season_x,meta,player){
    
    player_metadata <- meta
    player_data <- player
    
    player2 <- player_metadata[,c("id","element_type")]
    position_listx <- as.data.frame(c("Goalkeeper","Defender","Midfielder","Forward"))
    position_list <- cbind(position_listx,c(1:4))
    colnames(position_list) <- c("position","element_type")
    
    player_pos_team <- player_metadata[,c("id","element_type","web_name")]
    
    player_pos_team2 <-player_pos_team %>% inner_join(position_list,by="element_type")%>%rename(newid=id)
    
    player_data_new <- player_data %>% inner_join(player_pos_team2,by="newid")
    return(player_data_new)
}

apply_team_names_diffs <- function(season_x,meta,team,player){
    player_metadata <- meta
    team_names <- team_names_full %>% filter(Season=={season_x}) %>%select(team_name,team)
    team_data <- team
    player_data <-player
    
    team_codes <- unique(player_metadata$team_code)
    player_team_index <- lapply(team_codes,function(x){player_metadata$web_name[player_metadata$team_code==x]})
 
    team_name_codes <- team_names 
        
    team_name_codes_alt <- team_names
    colnames(team_name_codes_alt) <- c("opp_name","opponent_team")
    
    ##join on opponent difficulty
    home_team_diffs <- team_data[,c("gw","home_team","away_team","away_diff")]
    away_team_diffs <- team_data[,c("gw","away_team","home_team","home_diff")]
    colnames(away_team_diffs)  <-colnames(home_team_diffs)
    team_diffs <-rbind(home_team_diffs,away_team_diffs)
    colnames(team_diffs) <- c("round","opponent_team","team","opp_difficulty") 
    player_data_new <- player_data %>% inner_join(team_diffs,by=c("opponent_team","round"))
    
    
    ##I'm also going to join on the team playings difficulty rank; this might be useful in some of the
    ##stats.
    home_team_diffs2 <- team_data[,c("gw","home_team","away_team","home_diff")]
    away_team_diffs2 <- team_data[,c("gw","away_team","home_team","away_diff")]
    colnames(away_team_diffs2)  <- colnames(home_team_diffs2)
    team_diffs2 <-rbind(home_team_diffs2,away_team_diffs2)
    colnames(team_diffs2) <- c("round","opponent_team","team","team_difficulty") 
    
    player_data_new_2 <- player_data_new %>% inner_join(team_diffs2,by=c("opponent_team","round","team"))
    ##join on team names (just to check)
    
    player_data_new_3 <- player_data_new_2 %>% inner_join(team_name_codes,by=c("team")) %>%
        inner_join(team_name_codes_alt,by=c("opponent_team"))
    
    #some code to deal with multiple opponents in the same round
    
    multiple_teams_round <- player_data_new_3 %>%group_by(newid,round) %>%
        summarise(multiple_teams=n_distinct(team)) 
    
    major_team_count <- player_data_new_3 %>% group_by(newid,team)  %>%
        summarise(n=n()) %>%filter(n==max(n)) %>%rename(max_team=team)
    
    player_data_new_4 <- player_data_new_3 %>%left_join(multiple_teams_round,by=c("newid","round")) %>% 
        left_join(major_team_count,by="newid") %>% 
        filter((multiple_teams > 1 & team==max_team)|multiple_teams == 1)
    return(player_data_new_4)
}

apply_lag_data <- function(season_x,meta,team,player) {
    player_metadata <- meta
    team_names <- team_names_full %>%filter(Season=={{season_x}}) %>%select(team_name,team)
    team_data <- team
    player_data <-player
    
    player_data_new <- player_data %>% mutate(team_goals_scored=case_when(was_home==TRUE ~ team_h_score,
                                                                          was_home==FALSE ~ team_a_score),
                                              team_goals_conceded=case_when(was_home==TRUE ~ team_a_score,
                                                                            was_home==FALSE ~ team_h_score),
                                              win_flag=if_else(team_goals_scored > team_goals_conceded,1,0),
                                              draw_flag=if_else(team_goals_scored == team_goals_conceded,1,0),
                                              loss_flag=if_else(team_goals_scored < team_goals_conceded,1,0),
                                              game_date = as.Date(kickoff_time),
                                              value_minutes = value * minutes) 
    
    
    player_split <- split(player_data_new,player_data_new$newid)
    y <-player_split[[1]]
    
    lag_player_data  <- lapply(player_split,function(y){
        
        temp_data2 <- as.data.frame(matrix(data=NA,nrow=nrow(y),ncol=ncol(y)*2))
        for(i in c(1:length(y))){
            j <- colnames(y)[i]
            y1 <- y %>% select(all_of(j))
            if(sapply(y1,class)=="numeric"){
                
                temp_data2[,i] <- lag(y[,i],1) + lag(y[,i],2) + lag(y[,i],3) +
                    lag(y[,i],4) + lag(y[,i],4)
                temp_data2[,i+ncol(y)] <- lag(y[,i],1) 
                colnames(temp_data2)[i] <- paste0("lag5",colnames(y)[i])
                colnames(temp_data2)[i+ncol(y)] <- paste0("lag1",colnames(y)[i])         
            }
        }
        temp_data2$round <- y$round
        temp_data2$newid <- y$newid
        temp_data2$team_name <- y$team_name
        temp_data2$opp_name <- y$opp_name
        
        #some more recent data to help identify players who are likely to start
        
        
        vars_to_join <- y %>%  
            mutate( last_five_games_diff = difftime(game_date,lag(game_date,5),units=c("days")),
                    last_game_diff = difftime(game_date,lag(game_date,1),units=c("days"))
            ) %>%
            select(round,newid,web_name,team_name,opp_difficulty,opponent_team,opp_name,
                   value,position,
                   total_points,goals_scored,minutes,value,was_home,
                   assists,goals_conceded,clean_sheets,game_date,team,
                   bps,influence,creativity,threat,
                   team_name,opponent_team,opp_name,team_difficulty,
                   team_goals_scored,team_goals_conceded,
                   transfers_in,transfers_out,transfers_balance,
                   last_five_games_diff,last_game_diff)
        temp_data3 <- vars_to_join %>% inner_join(temp_data2,by=c("round","newid","team_name","opp_name")) %>%
             select(-(ends_with("0")),
                       -(ends_with("1")),
                       -(ends_with("2")),
                       -(ends_with("3")),
                       -(ends_with("4")),
                       -(ends_with("5")),
                       -(ends_with("6")),
                       -(ends_with("7")),
                       -(ends_with("8")),
                       -(ends_with("9")),
                       -(contains("transfers")),
                       -(contains("ea_index")),
                       -(contains("lag1")))
    
        
        return(temp_data3)
    })
    
    lag_player_data_new <- bind_rows(lag_player_data)  %>%mutate(
        goal_or_assist = ifelse(goals_scored >0|assists >0 ,1,0),
        goal_flag = ifelse(goals_scored >0 ,1,0),
        assist_flag  =  ifelse(assists >0 ,1,0)) %>%mutate(Season =season_x)
    return(lag_player_data_new)
}


read_data_fn <-function(season){
    print(season)
    player_data <- read_input_data(season)
    player_metadata <- read_metadata(season)
    team_data <- read_teamdata(season)
    
    player_data2 <- apply_player_positions(season,player_metadata,player_data)
    player_data3 <- apply_team_names_diffs(season,player_metadata,team_data,player_data2)
    player_data_final <- apply_lag_data(season,player_metadata,team_data,player_data3)
    return(player_data_final)
}

season_vec <- unique(team_names_full$Season)
player_data_list <- lapply(season_vec,read_data_fn)
player_data_table <- do.call(bind_rows,player_data_list)  
#fix team names
fix_names <- team_names_full %>% select(team_name,fixed_name) %>%rename(team_name_fixed = fixed_name) 
fix_opps <- team_names_full %>% select(team_name,fixed_name) %>%rename(opp_name = team_name,
                                                                       opp_name_fixed = fixed_name) 

player_data_table <- player_data_table %>% inner_join(fix_names,by="team_name") %>%
            inner_join(fix_opps,by="opp_name") %>%select(-team_name,-opp_name) %>%
            rename(team_name = team_name_fixed,opp_name = opp_name_fixed)

write_csv(player_data_table,"C:/Users/pc/Dropbox/FPLData/player_data_table.csv")

#add on ClubElo for each game date


unique_teams <- as.data.frame(unique(player_data_table$team_name))
colnames(unique_teams) = c("team_name")

elo_names <- team_names_full %>% select(fixed_name,Elo_name) %>%rename(team_name = fixed_name) %>%unique()

elo_opp_names <- elo_names %>% rename(Elo_opp_name = Elo_name,opp_name = team_name)

unique_teams <-  unique_teams %>% inner_join(elo_names) %>% mutate(url = (paste0("http://api.clubelo.com/",Elo_name)))

 
    elos_week_list <- lapply(unique_teams$Elo_name,function(x){

    elos <- read_csv(paste0("C:/Users/pc/Dropbox/FPLData/Elo/",x,".csv")) %>%select(Club,Elo,From,To)
    })


elos_full <- do.call(bind_rows,elos_week_list)

elos_full$Club <- gsub(" ","",elos_full$Club)
    
    game_dates <- player_data_table %>% select(Season,team_name,opp_name,game_date) %>% unique() %>%
                    inner_join(elo_names,by="team_name") %>%inner_join(elo_opp_names,by="opp_name")

team_elos <- elos_full %>%rename(Elo_name = Club, team_elo = Elo)

opp_elos <- elos_full  %>%rename(Elo_opp_name = Club, opponent_elo = Elo)

game_date_elos <- game_dates %>% inner_join(team_elos,by="Elo_name") %>% filter(game_date - days(7) >= From,
                                                                                game_date - days(7) <= To) %>%
                    select(-From,-To) %>% inner_join(opp_elos,by="Elo_opp_name") %>%
                filter(game_date - days(7) >= From,
                game_date - days(7) <= To) %>%select(-From,-To,-Season)


nrow(game_dates)
nrow(game_date_elos)

colnames(game_date_elos)
colnames(player_data_table)

player_data_table <- player_data_table %>% inner_join(game_date_elos,by=c("game_date","team_name","opp_name")) %>%
    select(-Elo_name,-Elo_opp_name)
write_csv(player_data_table,"C:/Users/pc/Dropbox/FPLData/player_data_table.csv")

#ok, now create a season-long data set 
season_start_elos_inp  <- game_dates %>% group_by(team_name,Season) %>% summarise(game_date = min(game_date))
season_start_elos <- season_start_elos_inp %>% inner_join(game_date_elos,by=c("team_name","game_date")) %>% select(team_name,Season,team_elo)

View(season_start_elos)
team_names_w_elo <- team_names_full %>%inner_join(season_start_elos,by=c("team_name","Season"))
View(team_names_w_elo)

write_csv(team_names_w_elo,"C:/Users/pc/Dropbox/FPLData/Team data with starting Elo.csv")

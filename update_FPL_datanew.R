##This R scripts scrapes the FPL data and puts it into a nice CSV file that we can point the 
##shiny app at

##needs library(jsonlite)  !

##Useful links;
##https://www.reddit.com/r/FantasyPL/comments/5q59h8/fantasypl_api/
##https://www.reddit.com/r/FantasyPL/comments/4tpd2r/an_updated_link_for_the_fantasypl_web_api/

library(shiny)
library(curl)
library(jsonlite)
library(dplyr)
library(knitr)
library(DT)
library(stringi)

options(timeout= 10000000) 

json_players <-fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
json_players_det <- json_players$elements
###download the data from the FPL website
json_player_list <- list()
ply_func <- function(i){
    x1 <-fromJSON(paste0("https://fantasy.premierleague.com/api/element-summary/",i,"/"))
    x2 <- x1$history 
    return(x2)}
ply_func(100)
for (i in 1:nrow(json_players_det)){
    
    json_player_list[[i]]<-tryCatch(ply_func(i),error=function(e){return()})
    }


json_players_det$web_name <- as.character(json_players_det$web_name)
json_players_det$web_name <- stri_encode(json_players_det$web_name, "", "UTF-8") 
json_players_det$web_name <- stri_trans_general(json_players_det$web_name, "Latin-ASCII")
json_players_det$web_name <- enc2native(json_players_det$web_name)


json_players_det$web_name <- iconv(json_players_det$web_name,"latin1", "ASCII//TRANSLIT")
json_players_det$second_name <- iconv(json_players_det$second_name,"latin1", "ASCII//TRANSLIT")
json_players_det$first_name <- iconv(json_players_det$first_name,"latin1", "ASCII//TRANSLIT")


write.csv(json_players_det,"C:/Users/pc/Dropbox/FPLData/player_metadata.csv")


unlist_json_players <- bind_rows(json_player_list,.id="newid")

unlist_json_players$influence <- as.numeric(unlist_json_players$influence)
unlist_json_players$creativity <- as.numeric(unlist_json_players$creativity)
unlist_json_players$threat <- as.numeric(unlist_json_players$threat)
unlist_json_players$newid <- as.numeric(unlist_json_players$element)#was newid
unlist_json_players$ict_index <- as.numeric(unlist_json_players$ict_index)

unlist_json_players <- unlist_json_players %>% mutate(round= ifelse(round >=39,round -9,round ))

# colnames(unlist_json_players) <-c("newid","id","kickoff_time","kickoff_time_formatted","Home team score",
#                                    "Away team score","Was home","round","Total points","Value","Transfers balance",
#                                    "Selected","Transfers in","Transfers out","Loaned in","Loaned out","Minutes",
#                                    "Goals scored","Assists","Clean sheets","Goals conceded","Own goals","Penalties saved",
#                                    "Penalties missed","Yellow cards","Red cards","Saves","Bonus points","Bonus points system",
#                                    "Influence","Creativity","Threat","ICT index","Clearances blocks interceptions","Recoveries",
#                                    "Key passes","Completed passes","Penalties conceded","Big chances missed","Errors leading to goal",
#                                    "Errors leading to goal attempts","Tackled","Offsides","Target missed","Fouls","Dribbles","element",
#                                    "fixture","opponent_team")
write.csv(unlist_json_players,"C:/Users/pc/Dropbox/FPLData/player_data.csv")

#teams

fpl_team_listx <- as.data.frame(c("Arsenal","Aston Villa","Brentford","Brighton","Burnley","Chelsea","Crystal Palace","Everton",
                                   "Leicester","Leeds Utd","Liverpool","Man City","Man Utd","Newcastle","Norwich","Southampton",
                                   "Tottenham","Watford","West Ham","Wolves"))

fpl_team_list <- cbind(fpl_team_listx,c(1:20))
colnames(fpl_team_list) <- c("team_name","team")
fixtures <- list()
#needs to be extended to 47 to include added game weeks
#should usually be 38 

gameweeks <- c((1:38))

for(j in gameweeks){
    json_fixtures <- fromJSON(paste0("https://fantasy.premierleague.com/api/fixtures/?event=",j))
    fixtures_x <- as.data.frame(cbind(json_fixtures$team_h,json_fixtures$team_a,json_fixtures$team_h_difficulty,json_fixtures$team_a_difficulty))
    fixtures_x$gw <-ifelse(j>=39,c(rep(j-9,nrow(fixtures_x))), c(rep(j,nrow(fixtures_x))))

    fixtures[[j]] <- fixtures_x
    
}

#fixtures_x
unlist_fixtures <- bind_rows(fixtures,.id="id")
colnames(unlist_fixtures)[2:5] <- c("home_team","away_team","home_diff","away_diff")
write.csv(unlist_fixtures,"C:/Users/pc/Dropbox/FPLData/team_data.csv")


# utility file containing function that grabs variables from files like data2022.RData
makehrdata.year <- function(season){
fn <- paste0("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/data",season,".RData")
load(fn)
data.yr <- get(eval(paste0("data",season)))
data.yr %>% filter(BAT_EVENT_FL==TRUE) %>% 
  mutate(hr=(EVENT_CD==23),park=substr(GAME_ID,1,3),season=season) %>%
  select(GAME_ID,park,season,BAT_ID,PIT_ID,EVENT_CD,hr,park,BAT_HAND_CD,PIT_HAND_CD)  -> 
data.12vars
data.12vars
}

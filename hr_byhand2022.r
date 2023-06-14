# Rscript to obtain observed and fitted HR freqs for parks
# and batterhands (one for LHB, one for RHB) for 2022.

library(emmeans)

makehrdata.year <- function(season){
  #this function retrieves pbp retrosheet data and selects 12 vars
  fn <- paste0("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/data",season,".RData")
  load(fn)
  data.yr <- get(eval(paste0("data",season)))
  data.yr %>% filter(BAT_EVENT_FL==TRUE) %>%
    mutate(hr=(EVENT_CD==23),park=substr(GAME_ID,1,3),season=season) %>%
    select(GAME_ID,park,season,BAT_ID,PIT_ID,EVENT_CD,hr,park,BAT_HAND_CD,PIT_HAND_CD,BAT_ID,PIT_ID)  ->
    data.12vars
  data.12vars
}

# loops over years
for(yr in c(2012:2022)){
  dfr <- makehrdata.year(yr)
  assign(paste0("data",yr,".12vars"),dfr)
  rm(dfr)
}

makehrtable_hand.yr <- function(dfr,yr){
  timestamp()
  fityr <- glm(hr ~ park:BAT_HAND_CD:PIT_HAND_CD,data=dfr,family="binomial")
  fityr.emmeans <- emmeans(fityr,~park:BAT_HAND_CD,type="response")
  fityr.emmeans %>% summary -> fityr.emmeans.summary
dfr %>% select(hr,BAT_HAND_CD,park) %>% table %>% prop.table(margin=c("BAT_HAND_CD","park")) %>% as.data.frame %>% filter(hr==TRUE) -> tableyear.1
#  dfr %>% select(hr,park,BAT_HAND_CD) %>%
#    table %>% prop.table(margin=2) -> tableyear.parks
#  as.data.frame(t(tableyear.parks)) %>% filter(hr==TRUE) %>% select(park,BAT_HAND_CD,Freq)  -> tableyear.1
  tableyear.1 %>% add_column(fitted=fityr.emmeans.summary$prob) -> tableyear.1
  tableyear.1 %>% mutate(season=yr) -> tableyear.1
  timestamp()
  tableyear.1
}

# plot this one year
# first have to pivot_longer
# LHB first

table2022 <- makehrtable_hand.yr(data2022.12vars, 2022) 
table2022 %>% filter(BAT_HAND_CD=="L") -> table2022.L
table2022.L$park <-  factor(table2022.L$park,
  levels=table2022.L$park[order(table2022.L$fitted)])

table2022.L %>% pivot_longer(cols=c(Freq,fitted),values_to="probhr") -> 
  table2022.L.tall

ggplot(table2022.L.tall) + geom_point(aes(y=probhr,x=park,color=name)) +
  ggtitle("LHB: fitted P(HR) \n Pr(HR) ~ BH*BH*Park") + 
  labs(x="Ballpark",y="Pr(HR|park)") + 
  theme(axis.text.x=element_text(angle=50))

table2022 %>% filter(BAT_HAND_CD=="R") -> table2022.R
table2022.R$park <-  factor(table2022.R$park,
  levels=table2022.R$park[order(table2022.R$Freq)])

table2022.R %>% pivot_longer(cols=c(Freq,fitted),values_to="probhr") -> 
  table2022.R.tall

ggplot(table2022.R.tall) + geom_point(aes(y=probhr,x=park,color=name)) +
  ggtitle("RHB: fitted P(HR) \n Pr(HR) ~ BH*BH*Park") + 
  labs(x="Ballpark",y="Pr(HR|park)") + 
  theme(axis.text.x=element_text(angle=50))



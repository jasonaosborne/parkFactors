# This file contains some exploratory code, investigating/explaining
# differences between adj and observed hr freqs by comparing
# team-specific bpcombo frqs w league avg.
# 
# Report means adjusted to league wide frequencies of bpcombos
# a) year-specific frequencies
# b) era frequencies

# Start off by computing year-specific and era frequencies.
# report cv for each freq.

# glue together data2012.12vars, ..., data2021.12vars into
# one data set

# This is nothing more than reweighting hrfrqs the four bpcombos
# for each park by the league-wide bpcombo freqs.

# data frames with names like data2012.12vars live in 
# /home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet

#fn <- paste0("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/data",yr,".RData")

#load(fn)

makehrdata.year <- function(season){
  fn <- paste0("/home/jaosborn/research/sports/mlb/parkfactors/datafiles/retrosheet/data",season,".RData")
  load(fn)
  data.yr <- get(eval(paste0("data",season)))
  data.yr %>% filter(BAT_EVENT_FL==TRUE) %>%
    mutate(hr=(EVENT_CD==23),park=substr(GAME_ID,1,3),season=season) %>%
    select(GAME_ID,park,season,EVENT_CD,hr,park,BAT_HAND_CD,PIT_HAND_CD,BAT_ID,PIT_ID)  ->
    data.12vars
  data.12vars
}

allyrs.12vars <- makehrdata.year(2012)

# loop over years
for(yr in 2013:2021){
dfr <- makehrdata.year(yr)
#assign(paste0("data",yr,".12vars"),dfr)
allyrs.12vars %>% add_row(dfr) -> allyrs.12vars
rm(dfr)
}

bhunknown <- which(allyrs.12vars$BAT_HAND_CD=="?")
allyrs.12vars %>% slice(-bhunknown) -> allyrs.12vars

## BPcombo freqs, by season and for the whole era

allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table -> bpfrqs.era 

bpfrqs.era %>% rowSums()
bpfrqs.era %>% colSums()
bpfrqs.era[1,1]/sum(bpfrqs.era[1,])
bpfrqs.era[2,1]/sum(bpfrqs.era[2,])
bpfrqs.era[1,1]/sum(bpfrqs.era[,1])
bpfrqs.era[1,2]/sum(bpfrqs.era[,2])

allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD,season) %>% table -> bpfrq.array

bpfrq.array %>% prop.table(margin=c("season")) -> bpfrq.byyear
plot(names(bpfrq.byyear[2,2,]),bpfrq.byyear[2,2,],ylim=c(0,0.7),type="b",lty=1,
     xlab="season",ylab="BPCombo Frequency",main="Batter/pitcher combo frqs over time",lwd=1.5)
lines(names(bpfrq.byyear[1,2,]),bpfrq.byyear[1,2,],type="b",lty=2)
lines(names(bpfrq.byyear[2,1,]),bpfrq.byyear[2,1,],type="b",lty=1,col="red")
lines(names(bpfrq.byyear[1,1,]),bpfrq.byyear[1,1,],type="b",lty=2,col="red")
legend(2012,0.7,c("RHBRHP","LHBRHP","RHBLHP","LHBLHP"),lty=c(1,2,1,2),col=c("black","black","red","red"),lwd=1.5)

allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(count=n()) %>% 
  mutate(relfreq=count/sum(count)) -> bpfrq.vec

dev.copy2pdf(file="bpcombo-season.pdf")

allyrs.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% 
  summarize(hrfrq=mean(hr)) %>% pivot_wider(values_from=hrfrq,names_from=c("BAT_HAND_CD","PIT_HAND_CD")) -> hrsummary.wide

hrsummary.wide %>% mutate(adjhr = L_L*0.081+L_R*0.341+0.197*R_L+0.382*R_R) -> 
  hrsummary.wide

allyrs.12vars %>% group_by(park) %>% summarize(rawhr=mean(hr)) -> hrfrqs.bypark

hrsummary.wide %>% inner_join(hrfrqs.bypark) -> hrsummary.wide

hrsummary.wide$park <- factor(hrsummary.wide$park,
                                 levels=hrsummary.wide$park[order(hrsummary.wide$adjhr)])

hrsummary.wide %>% pivot_longer(cols=c("adjhr","rawhr"),values_to="phr")-> tall.hrsummary

ggplot(tall.hrsummary) + geom_point(aes(y=phr,x=park,color=name)) +
 theme(axis.text.x=element_text(angle=60)) 

hrsummary.wide %>% arrange(adjhr,rawhr) %>% mutate(rank1=rank(adjhr),rank2=rank(rawhr))

allyrs.12vars %>% select(park,hr,BAT_HAND_CD,PIT_HAND_CD) %>% table-> allyrs.parks.table

allyrs.parks.table %>% prop.table(margin=c("park","BAT_HAND_CD","PIT_HAND_CD")) ->
  hr.ptable.park.bpcombo

# 2021 stuff 
#data2022.12vars <- makehrdata.year(2022)
data2022.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table -> bpfrqs.2022 

data2022.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% summarize(hrsum=sum(hr),pa=n()) -> hr.bypark.2022
data2022.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(hrsum=sum(hr),pa=n()) -> hr.2022

bpfrqs.2022 %>% rowSums()
bpfrqs.2022 %>% colSums()
bpfrqs.2022[1,1]/sum(bpfrqs.2022[1,])
bpfrqs.2022[2,1]/sum(bpfrqs.2022[2,])
bpfrqs.2022[1,1]/sum(bpfrqs.2022[,1])
bpfrqs.2022[1,2]/sum(bpfrqs.2022[,2])

data2022.12vars %>% group_by(park) %>% summarize(hrs=sum(hr),pa=n(),hrppa=mean(hr)) -> hrtotals.2022
data2022.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD)  %>% 
  summarize(rawhr=mean(hr)) %>% 
  pivot_wider(values_from=rawhr,names_from=c("BAT_HAND_CD","PIT_HAND_CD")) -> 
  hr2022.wide
hr2022.wide %>% mutate(adjhr=bpfrqs.2022[1,1]*L_L + bpfrqs.2022[1,2]*L_R + bpfrqs.2022[2,1]*R_L + bpfrqs.2022[2,2]*R_R) -> hr2022.wide

hr2022.wide %>% inner_join(hrtotals.2022) -> hr2022.wide

hr2022.wide$park <- factor(hr2022.wide$park, 
                           levels=hr2022.wide$park[order(hr2022.wide$adjhr)])
hr2022.wide %>% pivot_longer(cols=c("adjhr","hrppa"), values_to="phr") -> tallhr.2022

ggplot(tallhr.2022) + geom_point(aes(y=phr,x=park,color=name)) + 
  theme(axis.text.x=element_text(angle=60)) + 
  ggtitle("HR frequencies, 2022 \n adjhr = .07*LLHR+.20RLHR+.32*RLHR+.40RRHR") + 
  theme(legend.title=element_text("raw v weighted"))

hr2022.wide %>% mutate(adjustment=adjhr-hrppa) -> hr2022.wide

data2022.12vars %>% select(hr,BAT_HAND_CD,PIT_HAND_CD) %>% table %>% 
  prop.table(margin=c("BAT_HAND_CD","PIT_HAND_CD")) %>% as.data.frame %>%
  filter(hr==TRUE) %>% print

data2022.12vars %>% filter(park=="PIT") %>% 
  select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table

data2022.12vars %>% filter(park=="PIT") %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>%
  summarize(hrppa=mean(hr)) %>% print

data2022.12vars %>% filter(park=="SFN") %>% 
  select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table -> bpfrq.sfn

data2022.12vars %>% filter(park=="SFN") %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>%
  summarize(hrppa=mean(hr)) %>% print

data2022.12vars %>% filter(park=="CHA") %>% 
  select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table -> bpfrq.CHA

data2022.12vars %>% filter(park=="CHA") %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>%
  summarize(hrppa=mean(hr)) %>% print

bpfrq.CHA/bpfrqs.2022

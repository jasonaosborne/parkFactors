# Adjusted hr frequencies, 2022.

# Let us see how the batter-pitcher combination (`bpcombo`) frequencies have
# varied over time

load("./allyrs.12vars.RData")
allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table -> bpfrqs.era
# all four combo frqs
bpfrqs.era
# batter hand
bpfrqs.era %>% rowSums()
# pitcher hand
bpfrqs.era %>% colSums()
# conditionally on batter hand, P(LHP|LHB), P(LHP|RHB)
bpfrqs.era[1,1]/sum(bpfrqs.era[1,])
bpfrqs.era[2,1]/sum(bpfrqs.era[2,])
# conditionally on pitcher hand, P(LHB|LHP), P(LHB|RHP)
bpfrqs.era[1,1]/sum(bpfrqs.era[,1])
bpfrqs.era[1,2]/sum(bpfrqs.era[,2])

# These four bpcombo frequencies have changed little over time, though the preference for LHB when facing RHP may have decreased slightly.

allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD,season) %>% table -> bpfrq.array

bpfrq.array %>% prop.table(margin=c("season")) -> bpfrq.byyear
plot(names(bpfrq.byyear[2,2,]),bpfrq.byyear[2,2,],ylim=c(0,0.7),type="b",lty=1,
     xlab="season",ylab="BPCombo Frequency",main="Batter/pitcher combo frqs over time",lwd=1.5)
lines(names(bpfrq.byyear[1,2,]),bpfrq.byyear[1,2,],type="b",lty=2)
lines(names(bpfrq.byyear[2,1,]),bpfrq.byyear[2,1,],type="b",lty=1,col="red")
lines(names(bpfrq.byyear[1,1,]),bpfrq.byyear[1,1,],type="b",lty=2,col="red")
legend(2012,0.7,c("RHBRHP","LHBRHP","RHBLHP","LHBLHP"),lty=c(1,2,1,2),col=c("black","black","red","red"),lwd=1.5)

# make vector of bpcombo freqs
allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% 
summarize(count=n()) %>% mutate(relfreq=count/sum(count)) -> bpfrq.vec

# interaction effect of Batter and Hitter handedness for 2012-2021:
allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(hrfrq=mean(hr)) -> hrfrq.bp.era
hrfrq.bp.era
ggplot(hrfrq.bp.era,aes(y=hrfrq,x=BAT_HAND_CD,color=PIT_HAND_CD)) + 
  geom_line(aes(group=PIT_HAND_CD)) + geom_point() + 
  ggtitle("BP Combo effect on P(HR), 2012-2021")

#Looking over this 10 year period, it can be seen that home runs are least 
#likely when a LHB is facing a LHP.  Remarkably, the effect of the batter 
#hand only appears to matter when facing lefties.  Wow!


#Adjust to leaguewide frequencies for whole era
allyrs.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% 
  summarize(hrfrq=mean(hr)) %>% pivot_wider(values_from=hrfrq,
                                names_from=c("BAT_HAND_CD","PIT_HAND_CD")) ->
  hrsummary.wide
hrsummary.wide %>% mutate(adjhr=bpfrqs.era[1,1]*L_L + bpfrqs.era[1,2]*L_R + 
                                bpfrqs.era[2,1]*R_L + bpfrqs.era[2,2]*R_R) ->
  hrsummary.wide 
# unadjusted
allyrs.12vars %>% group_by(park) %>% summarize(obshr=mean(hr)) -> hrfrqs.bypark
hrsummary.wide %>% inner_join(hrfrqs.bypark) -> hrsummary.wide

# First order levels according to adjusted hr freq, then 
# ggplot both versus park
hrsummary.wide$park <- factor(hrsummary.wide$park,
                              levels=hrsummary.wide$park[order(hrsummary.wide$adjhr)])
hrsummary.wide %>% pivot_longer(cols=c("adjhr","obshr"),values_to="phr") ->
  hrsummary.tall

ggplot(hrsummary.tall) + geom_point(aes(y=phr,x=park,color=name)) + 
  theme(axis.text.x=element_text(angle=50)) + 
  ggtitle("HR Freqs, adj and observed \n 2012-2021")

# Look at how team bp combos (10year avg) varied
hrsummary.wide %>% mutate(adjmnt=adjhr-obshr) %>% 
  arrange(abs(adjmnt)) -> hrsummary.wide
hrsummary.wide %>% tail

# These differences between the observed relative frequencies are small, but the number of plate appearances is large:
allyrs.12vars %>% group_by(park) %>% summarize(pa=n(),hr=sum(hr)) %>% 
  inner_join(hrsummary.wide) %>% mutate(hrdiff=adjmnt*pa) %>% 
  arrange(abs(hrdiff)) -> hrsummary.wide
hrsummary.wide %>% tail

#Were bpcombo freqs for `CLE` different from those of the rest of the league?
as.vector(bpfrqs.era)
allyrs.12vars %>% select(park,BAT_HAND_CD,PIT_HAND_CD) %>% table %>%
  prop.table(margin=c("park")) -> bp.park
as.data.frame(bp.park) %>%  pivot_wider(values_from=Freq,
                                        names_from=c("BAT_HAND_CD","PIT_HAND_CD"),
                                        names_prefix="BP_") -> parks.wide
parks.wide %>% filter(park %in% c("SLN","MIN","SEA","NYA","LAN")) -> parks.adjup
parks.wide %>% filter(park %in% c("DET","CLE","HOU","TOR","BAL")) -> parks.adjdown

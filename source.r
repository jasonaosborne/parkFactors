load("./allyrs.12vars.RData")
allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table -> bpfrqs.era
# all four combo frqs
bpfrqs.era
# batter hand
print("by Batter Hand (row sums)")
bpfrqs.era %>% rowSums()
# pitcher hand
print("by Pitcher Hand (col sums)")
bpfrqs.era %>% colSums()
# conditionally on batter hand
print("Conditionally on Batter Hand")
allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table(margin="BAT_HAND_CD")  -> bhtable; bhtable
# conditionally on pitcher hand
print("Conditionally on Pitcher Hand")
allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table(margin="PIT_HAND_CD") 
# later, we'll look at these frs as a vector
print("All four relative freqs as a vector")
bpfrqs.era %>% as.data.frame  -> bpfrqs.era.vec ; bpfrqs.era.vec %>% print

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

allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(hrfrq=mean(hr)) -> hrfrq.bp.era
hrfrq.bp.era
ggplot(hrfrq.bp.era,aes(y=hrfrq,x=BAT_HAND_CD,color=PIT_HAND_CD)) + 
  geom_line(aes(group=PIT_HAND_CD)) + geom_point() + 
  ggtitle("BP Combo effect on P(HR), 2012-2021")

allyrs.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% 
  summarize(hrfrq=mean(hr)) %>% pivot_wider(values_from=hrfrq,
                                names_from=c("BAT_HAND_CD","PIT_HAND_CD")) ->
  hrsummary.wide
hrsummary.wide %>% mutate(adjhr=bpfrqs.era[1,1]*L_L + bpfrqs.era[1,2]*L_R + 
                                bpfrqs.era[2,1]*R_L + bpfrqs.era[2,2]*R_R) ->
  hrsummary.wide 

allyrs.12vars %>% group_by(park) %>% summarize(obshr=mean(hr)) -> hrfrqs.bypark
hrsummary.wide %>% inner_join(hrfrqs.bypark) -> hrsummary.wide

hrsummary.wide$park <- factor(hrsummary.wide$park,
                              levels=hrsummary.wide$park[order(hrsummary.wide$adjhr)])
hrsummary.wide %>% pivot_longer(cols=c("adjhr","obshr"),values_to="phr") ->
  hrsummary.tall

ggplot(hrsummary.tall) + geom_point(aes(y=phr,x=park,color=name)) + 
  theme(axis.text.x=element_text(angle=50)) + 
  ggtitle("HR Freqs, adj and observed \n 2012-2021")

hrsummary.wide %>% mutate(adjmnt=adjhr-obshr) %>% 
  arrange(abs(adjmnt)) -> hrsummary.wide; hrsummary.wide %>% tail

allyrs.12vars %>% group_by(park) %>% summarize(pa=n(),hr=sum(hr)) %>% 
  inner_join(hrsummary.wide) %>% mutate(hrdiff=adjmnt*pa) %>% 
  arrange(abs(hrdiff)) -> hrsummary.wide ; hrsummary.wide %>% tail

allyrs.12vars %>% select(park,BAT_HAND_CD) %>% table %>% prop.table(margin="park") %>%
  as.data.frame() %>% pivot_wider(values_from=Freq,names_from=BAT_HAND_CD) %>% 
  arrange(L) -> BHbyPark
BHbyPark %>% head

BHbyPark %>% tail

allyrs.12vars %>% select(park,PIT_HAND_CD) %>% table %>% prop.table(margin="park") %>%
  as.data.frame() %>% pivot_wider(values_from=Freq,names_from=PIT_HAND_CD) %>% 
  arrange(L) -> PHbyPark
PHbyPark %>% head
PHbyPark %>% tail

allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>%
  prop.table(margin="BAT_HAND_CD") -> bhtable

# compute observed hr frqs by park
allyrs.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% 
 summarize(hrfrq=mean(hr)) %>% 
  pivot_wider(values_from=hrfrq,
              names_from=c("BAT_HAND_CD","PIT_HAND_CD")) ->
  hrsummary.wide
# compute weighted HR freq for LHB and for RHB
hrsummary.wide %>% 
  mutate(adjhrLHB=bhtable[1,1]*L_L+bhtable[1,2]*L_R,
         adjhrRHB=bhtable[2,1]*R_L+bhtable[2,2]*R_R) ->
hrsummary.wide

# hrfrs by hand not adjusted for pitcher hand
allyrs.12vars %>% group_by(park,BAT_HAND_CD) %>% 
  summarize(hrfrq=mean(hr)) %>% 
  inner_join(hrsummary.wide) -> hrsummary.wide

# make LHB tall
hrsummary.wide %>% filter(BAT_HAND_CD=="L") -> hrsummary.wide.LHB
hrsummary.wide.LHB$park <- factor(hrsummary.wide.LHB$park,
 levels=hrsummary.wide.LHB$park[order(hrsummary.wide.LHB$adjhrLHB)])

hrsummary.wide.LHB %>%
  pivot_longer(cols=c("adjhrLHB","hrfrq"),values_to="phr") ->
  hrsummary.tall.LHB

# plot LHB
ggplot(hrsummary.tall.LHB) + geom_point(aes(y=phr,x=park,color=name))+
  theme(axis.text.x=element_text(angle=50)) +
  ggtitle("HR Freqs for LHB, adjusted and observed \n 2012-2021")

hrsummary.wide %>% filter(BAT_HAND_CD=="R") -> hrsummary.wide.RHB
hrsummary.wide.RHB$park <- factor(hrsummary.wide.RHB$park,
 levels=hrsummary.wide.RHB$park[order(hrsummary.wide.RHB$adjhrRHB)])

hrsummary.wide.RHB %>%
  pivot_longer(cols=c("adjhrRHB","hrfrq"),values_to="phr") ->
  hrsummary.tall.RHB

# plot RHB
ggplot(hrsummary.tall.RHB) + geom_point(aes(y=phr,x=park,color=name)) +
  theme(axis.text.x=element_text(angle=50)) +
  ggtitle("HR Freqs for RHB, adjusted and observed \n 2012-2021")
hrsummary.wide.era <- hrsummary.wide

bpfrqs.2021 <- bpfrq.byyear[,,10] # computed earlier
allyrs.12vars %>% filter(season==2021) %>% 
  group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% 
  summarize(hrfrq=mean(hr)) %>% pivot_wider(values_from=hrfrq,
                                names_from=c("BAT_HAND_CD","PIT_HAND_CD")) ->
  hrsummary.wide.2021
hrsummary.wide.2021 %>% mutate(adjhr=bpfrqs.2021[1,1]*L_L + bpfrqs.2021[1,2]*L_R + 
                                bpfrqs.2021[2,1]*R_L + bpfrqs.2021[2,2]*R_R) ->
  hrsummary.wide.2021 
# unadjusted
allyrs.12vars %>% filter(season==2021) %>% group_by(park) %>% summarize(obshr=mean(hr)) -> hrfrqs.bypark.2021
hrsummary.wide.2021 %>% inner_join(hrfrqs.bypark.2021) -> hrsummary.wide.2021
hrsummary.wide.2021$park <- factor(hrsummary.wide.2021$park,
                     levels=hrsummary.wide.2021$park[order(hrsummary.wide.2021$adjhr)])
hrsummary.wide.2021 %>% pivot_longer(cols=c("adjhr","obshr"),values_to="phr") ->
  hrsummary.tall.2021

ggplot(hrsummary.tall.2021) + geom_point(aes(y=phr,x=park,color=name)) + 
  ggtitle("HR Freqs, adj and observed \n only the 2021 season") + 
  theme(axis.text.x=element_text(angle=50))

hrsummary.tall.2021 %>% print(n=5)

allyrs.12vars %>% filter(season==2021) %>% 
  select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>%
  prop.table(margin="BAT_HAND_CD") -> bhtable

allyrs.12vars %>% filter(season==2021) %>% 
  group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>%
  summarize(hrfrq=mean(hr)) %>% 
  pivot_wider(values_from=hrfrq,
              names_from=c("BAT_HAND_CD","PIT_HAND_CD")) ->
  hrsummary.wide.2021
hrsummary.wide.2021 %>%
  mutate(adjhrLHB=bhtable[1,1]*L_L + bhtable[1,2]*L_R,
         adjhrRHB=bhtable[2,1]*R_L + bhtable[2,2]*R_R) ->
hrsummary.wide.2021

allyrs.12vars %>% filter(season==2021) %>% 
  group_by(park,BAT_HAND_CD) %>%
  summarize(hrfrq=mean(hr)) %>% 
  inner_join(hrsummary.wide.2021) -> hrsummary.wide.2021

hrsummary.wide.2021 %>% filter(BAT_HAND_CD=="L") -> hrsummary.wide.2021.LHB
hrsummary.wide.2021 %>% filter(BAT_HAND_CD=="R") -> hrsummary.wide.2021.RHB

hrsummary.wide.2021.LHB$park <- factor(hrsummary.wide.2021.LHB$park,
 levels=hrsummary.wide.2021.LHB$park[order(hrsummary.wide.2021.LHB$adjhrLHB)])

hrsummary.wide.2021.RHB$park <- factor(hrsummary.wide.2021.RHB$park,
 levels=hrsummary.wide.2021.RHB$park[order(hrsummary.wide.2021.RHB$adjhrRHB)])

hrsummary.wide.2021.LHB %>% 
  pivot_longer(cols=c("adjhrLHB","hrfrq"),values_to="phr") -> 
  hrsummary.tall.2021.LHB
hrsummary.wide.2021.RHB %>% 
  pivot_longer(cols=c("adjhrRHB","hrfrq"),values_to="phr") -> 
  hrsummary.tall.2021.RHB

ggplot(hrsummary.tall.2021.LHB) + geom_point(aes(y=phr,x=park,color=name)) + 
  theme(axis.text.x=element_text(angle=50)) + 
  ggtitle("HR Freqs for LHB, adj and observed in 2021 only")

ggplot(hrsummary.tall.2021.RHB) + geom_point(aes(y=phr,x=park,color=name)) + 
  theme(axis.text.x=element_text(angle=50)) + 
  ggtitle("HR Freqs for RHB, adj and observed in 2021 only")

hrsummary.wide.2021 %>% rename(hrfrq.2021 = hrfrq,
                               adjLHB.2021=adjhrLHB,
                               adjRHB.2021=adjhrRHB) -> hrsummary.wide.2021

hrsummary.wide.era %>% select(-4:-7) %>% inner_join(hrsummary.wide.2021) ->
  hrsummary.wide.both; print(hrsummary.wide.both,n=5)

print(cor(hrsummary.wide.both["BAT_HAND_CD"=="L",3:8]))
print(cor(hrsummary.wide.both["BAT_HAND_CD"=="R",3:8]))


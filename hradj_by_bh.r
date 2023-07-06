# separately plot phr v park for LHB and for RHB

allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>%
  prop.table(margin="BAT_HAND_CD") -> bhtable

# compute observed hr frqs by park
allyrs.12vars %>% group_by(park,BAT_HAND_CD,PIT_HAND_CD) %>% 
 summarize(hrfrq=mean(hr)) %>% 
  pivot_wider(values_from=hrfrq,
              names_from=c("BAT_HAND_CD","PIT_HAND_CD")) ->
  hrsummary.wide
hrsummary.wide %>% 
  mutate(adjhrLHB=bhtable[1,1]*L_L+bhtable[1,2]*L_R,
         adjhrRHB=bhtable[2,1]*R_L+bhtable[2,2]*R_R) ->
hrsummary.wide

# hrfrs by hand not adjusted for pitcher hand
allyrs.12vars %>% group_by(park,BAT_HAND_CD) %>% 
  summarize(hrfrq=mean(hr)) %>% 
  inner_join(hrsummary.wide) -> hrsummary.wide

allyrs.12vars %>% group_by(park,BAT_HAND_CD) %>% 
  summarize(hrfreq=mean(hr),hrsum=sum(hr),count=n())

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

# make RHB tall
hrsummary.wide %>% filter(BAT_HAND_CD=="R") -> hrsummary.wide.RHB
hrsummary.wide.RHB$park <- factor(hrsummary.wide.RHB$park,
 levels=hrsummary.wide.RHB$park[order(hrsummary.wide.RHB$adjhrRHB)])

hrsummary.wide.RHB %>%
  pivot_longer(cols=c("adjhrRHB","hrfrq"),values_to="phr") ->
  hrsummary.tall.RHB

# plot RHB
ggplot(hrsummary.tall.RHB) + geom_point(aes(y=phr,x=park,color=name))+
  theme(axis.text.x=element_text(angle=50)) +
  ggtitle("HR Freqs for RHB, adjusted and observed \n 2012-2021")

# need to facet this?

# For which park is the asymmetry the greatest, using
# - observed HR Frq
# - adjusted HR Frq


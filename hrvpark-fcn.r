# plot phr v park separately for LHB and RHB
# weighted phr is given by
# wphr = 

---
title:  "Adjusted Home Run Frequencies" 
author: Jason Osborne and Rich Levine
output:
  pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE,eval=TRUE,message=FALSE,cache=TRUE,fig.align="center",out.width="50%")
```

# Adjusted hr frequencies, 2012-2021.

Let us see how the batter-pitcher combination (`bpcombo`) frequencies have
varied over time

```{r plot1, out.width="50%", fig.align="center",cache=TRUE,echo=FALSE}
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
allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table(margin="BAT_HAND_CD") 
# conditionally on pitcher hand
print("Conditionally on Pitcher Hand")
allyrs.12vars %>% select(BAT_HAND_CD,PIT_HAND_CD) %>% table %>% prop.table(margin="PIT_HAND_CD") 
# later, we'll look at these frs as a vector
print("All four relative freqs as a vector")
bpfrqs.era %>% as.data.frame  -> bpfrqs.era.vec ; bpfrqs.era.vec %>% print
```

\newpage

These four bpcombo frequencies have changed little over time, though the preference
for LHB when facing RHP may have decreased slightly.
```{r plot2, echo=FALSE}
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
```
The effects of bpcombo on home run frequency can be investigated with an interaction plot, generated with data from 2012-2021:
```{r bpfrqs.era}
allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(hrfrq=mean(hr)) -> hrfrq.bp.era
hrfrq.bp.era
ggplot(hrfrq.bp.era,aes(y=hrfrq,x=BAT_HAND_CD,color=PIT_HAND_CD)) + 
  geom_line(aes(group=PIT_HAND_CD)) + geom_point() + 
  ggtitle("BP Combo effect on P(HR), 2012-2021")
```
Looking over this 10 year period, it can be seen that home runs are least likely when
a LHB is facing a LHP.  Remarkably, the effect of the batter hand only appears 
to matter when facing lefties.  Wow!

For a given park, the frequencies of the four combinations can vary dramatically 
from one season to the next, depending upon the personnel of the home team and with
the unbalanced schedules of years past, upon the personnel of other teams in the
division.  In light of bpcombo effects, home run frequencies for each park can
be adjusted to league-wide bpcombo frequencies simply by reweighting the
four conditional home run rates to the these frequencies.  

```{r adjplot1}
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
```
These adjusted frequencies can be plotted against park, along with the unadjusted
frequencies.  Further investigation of changes over time is warranted though,
as a glance at 10-year averages still shows considerable variability in 
bpcombo frequencies across parks.  It must be kept in mind that many players
reside with the same team for long periods of time, so these 10 years are not
at all independent.  However, we average anyway ...

A technique worth mentioning in the construction of this plot is to achieve
an ordering of parks on the horizontal axis according to either the observed
or adjusted home run rate by so ordering the levels of park as a factor.
```{r orderpark, cache=TRUE}
hrsummary.wide$park <- factor(hrsummary.wide$park,
                              levels=hrsummary.wide$park[order(hrsummary.wide$adjhr)])
hrsummary.wide %>% pivot_longer(cols=c("adjhr","obshr"),values_to="phr") ->
  hrsummary.tall
```
Now `ggplot` can be used ...
```{r parkplot}
ggplot(hrsummary.tall) + geom_point(aes(y=phr,x=park,color=name)) + 
  theme(axis.text.x=element_text(angle=50)) + 
  ggtitle("HR Freqs, adj and observed \n 2012-2021")
```

\newpage

Ok, let us consider those teams for which the observed and adjusted hr freqs were different.
```{r rankdiffs}
hrsummary.wide %>% mutate(adjmnt=adjhr-obshr) %>% 
  arrange(abs(adjmnt)) -> hrsummary.wide; hrsummary.wide %>% tail
```
These differences between the observed relative frequencies are small, but the number of plate appearances is large:
```{r pacount}
allyrs.12vars %>% group_by(park) %>% summarize(pa=n(),hr=sum(hr)) %>% 
  inner_join(hrsummary.wide) %>% mutate(hrdiff=adjmnt*pa) %>% 
  arrange(abs(hrdiff)) -> hrsummary.wide ; hrsummary.wide %>% tail
```
Were bpcombo freqs for `CLE` different from those of the rest of the league?
```{r bpcombos}
allyrs.12vars %>% select(park,BAT_HAND_CD,PIT_HAND_CD) %>% table %>%
  prop.table(margin=c("park")) %>% as.data.frame -> teambpfrqs.tall
teambpfrqs.tall %>% filter(park=="CLE") %>% print
# Glancing back at era frequencies:
bpfrqs.era.vec %>% print 
```
For whatever reason, there were considerably more PA involving Pitchers and Batters
of the same hand (RHBRHP or LHBLHP) at Jacobs Field, resulting in upward
adjustment to era frequencies (so long as CLE conditional HR rates not too different.)

\newpage

For other teams, we compute team combo frequencies relative era combo frequencies

```{r team2era}
allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(count=n()) %>%
  ungroup %>% mutate(relFreq=count/sum(count)) -> bptotals
teambpfrqs.tall %>% inner_join(bptotals) %>% 
  mutate(team2era = Freq/relFreq) -> team2era 
team2era %>%  arrange(park,BAT_HAND_CD,PIT_HAND_CD)  %>% 
  filter(park %in% c("CLE","SLN","LAN","DET","MIN","TOR")) %>% print
```
A reminder of which combos lead to the most HRs
```{r hrfrq}
allyrs.12vars %>% group_by(BAT_HAND_CD,PIT_HAND_CD) %>% summarize(hrfrq=mean(hr),hrsum=sum(hr))
```
Now the other adjusteds

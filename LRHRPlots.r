# this code generates separate plots, one for LHB, one for RHB
# of fitted and observed HR freqs against parks, with ordering
# by fitted value.  These "fitted" values come from 
# glm(hr ~ 
# 

allhrtables %>% dim
allhrtables %>% head
#allhrtables %>% filter(BAT_HAND_CD=="R") ->  allhrtables.R
#allhrtables %>% filter(BAT_HAND_CD=="R") ->  allhrtables.L

allhrtables.avgs %>% filter(BAT_HAND_CD=="R") -> allhrtables.avgs.R
allhrtables.avgs %>% filter(BAT_HAND_CD=="L") -> allhrtables.avgs.L

allhrtables.avgs.R$park <- factor(allhrtables.avgs.R$park,levels=allhrtables.avgs.R$park[order(allhrtables.avgs.R$fitted_mean)])

allhrtables.avgs.L$park <- factor(allhrtables.avgs.L$park,levels=allhrtables.avgs.L$park[order(allhrtables.avgs.L$fitted_mean)])


allhrtables.avgs.R %>% 
  pivot_longer(cols=c(Freq_mean,fitted_mean),values_to="ProbHR",names_to="Obs_fitted") ->
allhrtables.avgs.R.wide 

ggplot(allhrtables.avgs.R.wide,aes(y=ProbHR,x=park,color=Obs_fitted)) + 
  geom_point() + 
  ggtitle("Fitted and observed HR freqs \n Pr(HR) ~ BH*PH*Park \n RHB, 10 seasons") + 
  theme(axis.text.x=element_text(angle=60))

dev.copy2pdf(file="hrfrqVparkRHB_12_22.pdf")

allhrtables.avgs.L %>% 
  pivot_longer(cols=c(Freq_mean,fitted_mean),values_to="ProbHR",names_to="Obs_fitted") ->
allhrtables.avgs.L.wide 

ggplot(allhrtables.avgs.L.wide,aes(y=ProbHR,x=park,color=Obs_fitted)) + 
  geom_point() + 
  ggtitle("Fitted and observed HR freqs \n Pr(HR) ~ BH*PH*Park \n LHB, 10 seasons") + 
  theme(axis.text.x=element_text(angle=60))

dev.copy2pdf(file="hrfrqVparkLHB_12_22.pdf")

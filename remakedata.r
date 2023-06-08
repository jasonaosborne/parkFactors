# Something wrong with the data used to produce
# plots in the Markdown file: "plotHRFreqsByPark.rmd"

# here I will try to regenerate, from scratch, data 
# frames with observed and fitted home run freqs

# 1. A quick check does not indicate any problem
# with objects named like "datayyyy.12vars", so we'll
# use those.  Even the observation from 2015
# with BAT_HAND_CD = "?" seems to have been removed.
#
# These data sets were created using the makehrdata.year()
# below

library(emmeans)

#and the loop that assembles all 12 years:

#for(yr in 2010:2022){
#dfr <- makehrdata.year(yr)
#assign(paste0("data",yr,".12vars"),dfr)
#rm(dfr)
#}

#Now obtain observed HR freqs and also fitted HR freqs using
#the model logit(Pr(HR))  ~ BH*PH*park for each year separately.
#These operations will be carried out by the function `makehrtable.yr`

makehrtable.yr <- function(dfr,yr){
timestamp()
fityr <- glm(hr ~ park:BAT_HAND_CD:PIT_HAND_CD,data=dfr,family="binomial")
#fityr.emmeans <- emmeans(fityr,~park:BAT_HAND_CD,type="response")
fityr.emmeans <- emmeans(fityr,~park,type="response")
fityr.emmeans %>% summary -> fityr.emmeans.summary
#dfr %>% select(hr,park,BAT_HAND_CD) %>%
dfr %>% select(hr,park) %>%
table %>% prop.table(margin=2) -> tableyear.parks
#as.data.frame(t(tableyear.parks)) %>% filter(hr==TRUE) %>% select(park,BAT_HAND_CD,Freq)  -> tableyear.1

as.data.frame(t(tableyear.parks)) %>% filter(hr==TRUE) %>% select(park,Freq) -> tableyear.1
tableyear.1 %>% add_column(fitted=fityr.emmeans.summary$prob) -> tableyear.1
tableyear.1 %>% mutate(season=yr) -> tableyear.1
timestamp()
tableyear.1
}

for(yr in 2012:2021){
dfr <- get(eval(paste0("data",yr,".12vars")))
tbl <- makehrtable.yr(dfr,yr)
assign(paste0("table_nohand",yr),tbl)
rm(dfr)
rm(tbl)
}

# now glue tableavg2012, tableavg2013, ... , tableavg, together
hrtableNohand <- rbind(
table_nohand2012,
table_nohand2013,
table_nohand2014,
table_nohand2015,
table_nohand2016,
table_nohand2017,
table_nohand2018,
table_nohand2019,
table_nohand2020,
table_nohand2021)

save(hrtableNohand,file="hrtableNohand.RData")

# now average over years

hrtableNohand %>% group_by(park) %>% 
  summarize(Freq_mean=mean(Freq),fitted_mean=mean(fitted)) ->
  hrtableNohand.avgs

hrtableNohand.avgs$park <- factor(hrtableNohand.avgs$park,
levels=hrtableNohand.avgs$park[order(hrtableNohand.avgs$fitted_mean)])

hrtableNohand.avgs %>% pivot_longer(cols=c(Freq_mean,fitted_mean),
  values_to="ProbHR",names_to="Obs_fitted") ->
  hrtableNohand.avgs

ggplot(hrtableNohand.avgs,aes(y=ProbHR,x=park,color=Obs_fitted)) + 
  geom_point() +
  ggtitle("original plot of fit/obs HR v park \n years 2012-2021 \n Pr(HR) ~ BH*PH*park") + 
  theme(axis.text.x=element_text(angle=60)) 

dev.copy2pdf(file="HRvparkObsFit_12_21.pdf")


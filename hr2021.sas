
/* compare three links: logit, probit, cloglog */
/* modelling hr freq using hitter and park effects */
/* z = hr/pa by batter from side and against pitcher hand
       specific to plate appearance.
so, for a given RHB, z should depend on handedness of pitcher */



libname pf "/home/jaosborn/research/sports/mlb/parkfactors/datafiles";
%let nocrp=nocol norow nopercent;

options ls=140 nocenter nodate ps=1000;
/*  
data allyrs;
   length game_id $20;
   infile "allyrs12vars.csv" dsd firstobs=2;
   input j $ game_id $ park $ season bat_id $ event_cd hrtf $ bat_hand_cd $ pit_hand_cd $ pit_id $ ;
   hr=(hrtf="TRUE");
   call streaminit(123);
   *train = rand("unif") < 0.5;
   *test = 1- train; 
   *if event_cd in (19,20,21,22,23,2); *add 13, 18?;
   if event_cd in (13,18,19,20,21,22,23,2); 
run;
*/
data allyrs4;
   length bat_hand_cd pit_hand_cd $2;
   set pf.condallyrs4;
   if event_cd in (13,18,19,20,21,22,23,2) and season=2021;  
   drop j hrtf ;
run;
ods trace on;
data allyrs4;
   set allyrs4;
   xBHRs=(batter_hrmean*batter_pa + 0.04255*100)/(batter_pa+100);
   xPHRs=(pitcher_hrmean*pitcher_pa + 0.04255*100)/(pitcher_pa+100);
   xBHR=batter_hrmean;
   xPHR=pitcher_hrmean;
run;
/*proc contents data=allyrs4; run;*/

/*
proc print data=allyrs4(obs=5) noobs;
   var game_id hr park bat_hand_cd pit_hand_cd bat_id batter_hrmean batter_pa 
       pit_id pitcher_hrmean pitcher_pa xBHR xBHRs xPHR xPHRs season;
run; 
*/
/* */
*%macro linklog(link);
ods listing close;
proc logistic data=allyrs4 descending; *hrmean*park effects?;
   title "conditional on event_cd in (2,19,20,21,22,23)";
   class park PIT_HAND_CD bat_hand_cd season /param=glm; 

*   model hr=park*bat_hand_cd*pit_hand_cd xBHR xPHR season /lackfit; 
   model hr=park*bat_hand_cd*pit_hand_cd xBHR xPHR season / link = logit lackfit; 
*   model hr=park*bat_hand_cd*pit_hand_cd xBHR xPHR season /lackfit; 

   *lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0357356 at xPHR=.0349822 ;*slice=park;  *e?;
   *lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0435783 at xPHR=.0424085 ;*slice=park;  *e?;
   *lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0454692 at xPHR=.0443257 ;*slice=park;  *e?;
   *lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0438477 at xPHR=.0422641 ;*slice=park;  *e?;

   lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0393032 at xPHR=.0358618 ;*slice=park;  *e?; *LL;
   lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0486574 at xPHR=.0475480 ;*slice=park;  *e?; *LR;
   lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0500402 at xPHR=.0490041 ;*slice=park;  *e?; *RL;
   lsmeans park*bat_hand_cd*pit_hand_cd / ilink at xBHR=.0480719 at xPHR=.0469968 ;*slice=park;  *e?; *RR;
   *ods output lackfitpartition=pf.lfp lackfitchisq=pf.lfc lsmeans=pf.condlsm;
   *ods output lackfitpartition=pf.lfp4 lackfitchisq=pf.lfc4 lsmeans=pf.condlsm4;
   *ods output lackfitpartition=pf.lf&link lackfitchisq=pf.lfc&link lsmeans=pf.lsm&link;
   ods output lsmeans=lsm;
run;
ods listing ;
*%mend linklog;
/* 
%linklog(logit)
%linklog(cloglog)
%linklog(probit)
*/
proc sort data=lsm;
   by park pit_hand_cd bat_hand_cd;
run;
/*
proc print data=lsm;
   format xBHR xPHR 7.5;
run;
*/
data lmLL lmLR lmRL lmRR;
*data condlsm4;
   *set pf.condlsm4;
   set lsm;
   rxBHR=round(xBHR,0.00001);
   rxPHR=round(xPHR,0.00001);
*        if ((round(xBHR,0.0001)=0.0357) and (round(xPHR,0.0001)=0.0350) and (bat_hand_cd="L") and (pit_hand_cd="L")) then output lmLL;
*   else if ((round(xBHR,0.0001)=0.0436) and (round(xPHR,0.0001)=0.0424) and (bat_hand_cd="L") and (pit_hand_cd="R")) then output lmLR;
*   else if ((round(xBHR,0.0001)=0.0455) and (round(xPHR,0.0001)=0.0443) and (bat_hand_cd="R") and (pit_hand_cd="L")) then output lmRL;
*   else if ((round(xBHR,0.0001)=0.0438) and (round(xPHR,0.0001)=0.0423) and (bat_hand_cd="R") and (pit_hand_cd="R")) then output lmRR;
        if ((rxBHR=0.03930) and (rxPHR=0.03586) and (bat_hand_cd="L") and (pit_hand_cd="L")) then output lmLL;
   else if ((rxBHR=0.04866) and (rxPHR=0.04755) and (bat_hand_cd="L") and (pit_hand_cd="R")) then output lmLR;
   else if ((rxBHR=0.05004) and (rxPHR=0.04900) and (bat_hand_cd="R") and (pit_hand_cd="L")) then output lmRL;
   else if ((rxBHR=0.04807) and (rxPHR=0.04700) and (bat_hand_cd="R") and (pit_hand_cd="R")) then output lmRR;
run;   
data condlsm4; 
   set lmll lmlr lmrl lmrr;
run;
proc sort data=condlsm4;
   by descending mu;
   *by park pit_hand_cd bat_hand_cd;
run;
/*
proc print data=condlsm4; 
   *title "condlsm4, link=&link";
   title "condlsm4";
   format _numeric_ 7.5;
   var park bat_hand_cd pit_hand_cd xBHR rxBHR xPHR rxPHR estimate stderr mu stderrmu;
run;
*/
*raw frequencies;
proc means data=allyrs4 noprint nway;
   class park bat_hand_cd pit_hand_cd;
   var hr xBHR xPHR;
   output out=rawfreqs mean=hrmean xBHRmean xPHRmean n=pa sum=hrsum;  
run; 
*merge raw and fitted frequencies;
proc sql;
   create table both as select * from condlsm4 natural join rawfreqs;
*   select park, bat_hand_cd, pit_hand_cd, xBHR format=6.5, xBHRmean, xPHR format=6.5, 
*          xPHRmean, hrmean, Mu, pa, (hrmean-Mu)*pa as residhr 
*          from both order by Mu desc; 
quit;
proc sort data=both; 
   by bat_hand_cd pit_hand_cd;
run;
proc rank data=both out=rboth;
   by bat_hand_cd pit_hand_cd;
   var hrmean mu;
   ranks rhrmean rmu;
run;
*need to add fitted homerun totals;
proc sql; 
   title "conditional on event_cd in (2,19,20,21,22,23)";
   title2 "glossary: hrmean=observed hr/pa";
   title3 "xBHR and xPHR - hand-specific hrfreq among hitters,pitchers respectively";
   title4 "Mu=estimated Pr(HR), rMu=rank (1-30) of Mu, by bpcombo"; 
   title5 "hrmean=observed relfreq of HR, hrsum=observed HRsum, pa=plate apperances";
   title6 "rhrmean=rank (1-30) of hrmean, by bpcombo";
   title7 "fitted_hr_sum=estimated HR sum";
   title8 "sorted by estimated Pr(HR)";
   select park, bat_hand_cd as bh, pit_hand_cd as ph, xBHR format=5.4, xBHRmean, xPHR format=6.5, 
          xPHRmean, hrmean, rhrmean label="rank hrmean", Mu label="Mu", rMu, hrsum, Mu*pa as fitted_hr_sum, pa, 
          (hrmean-Mu)*pa as residhr 
          from rboth order by Mu desc; 
   title9 "sorted by estimated difference between ranks of fitted, observed Pr(HR) within bpcombo";
   select park, bat_hand_cd as bh, pit_hand_cd as ph, xBHR format=5.4, xBHRmean, xPHR format=6.5, xPHRmean, hrmean, 
          rhrmean label="rank_hrmean", Mu label="Mu", rMu label="rank_Mu", hrsum, Mu*pa as fitted_hr_sum, pa, 
          (hrmean-Mu)*pa as residhr, rhrmean-rmu as rankdiff
          from rboth order by rankdiff  desc; 
quit;
proc means data=allyrs4;
   class bat_hand_cd pit_hand_cd;
   var xBHR xBHRs xPHR xPHRs hr;
run;
endsas; 
proc means data=allyrs4;
   class bat_hand_cd pit_hand_cd;
   var hr xBHR xPHR;
run;
endsas;
proc print data=both;
run;
endsas;

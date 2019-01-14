

devtools::install_github('rstudio/rmarkdown')

######## Intervene: Clustered SE: MODEL 1 & MODEL 2 ########
start_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
               data=start_stack))

start_mod_demplus<-lm(selected~strength+casualties+interest+reputation+public+economy+
                                demAge+demEdu+demGender+demRace+demPID+demInpoli, 
                        data=start_stack)
#start_mod_cl<-cluster.vcov(start_mod, start_stack$resp_id)
#coeftest(start_mod,start_mod_cl)
library(sandwich)
cov1        <- vcovHC(start_mod, type = "HC1")
robust.se   <- sqrt(diag(cov1))

cov2        <- vcovHC(start_mod_demplus, type = "HC1")
robust.se2   <- sqrt(diag(cov2))

stargazer(start_mod, start_mod_demplus ,style="apsr",  se = list(robust.se, robust.se2, NULL),
          notes="Baseline Levels: Strength:Strong; Casualties:High; U.S. Interest:Little; U.S. Reputation:Litte; U.S. Public Opinion:Against; Economy:In Recession; Age:18-24;
          Education:Did not finish high school; Gender:Male; Race:American Indian; Political Party: Republican; Interest in Politics:Very interested",
          dep.var.labels.include=FALSE, single.row=TRUE,
          covariate.labels=c("Strength: As Strong As the U.S.", "Strength: Weaker than the U.S." ,"Casualties: Low", "U.S. Interest: Long-term Interest",
                             "Interest: High","U.S. Reputation at Stake: High",
                             "Public Opinion: Evenly Split","U.S. Public Opinion: Support",
                             "Economy: Sliding into Recession","Economy: Recovering from Recession", 
                             "Economy: In Good Shape", "Age:25-34","Age:35-44","Age:45-54","Age:55-64","Age:64 or older",
                             "Education:High school diploma or equivalent","Technical or vocational school",
                             "Some college, no degree","Associate's or two-year college degree","Four-year college degree",
                             "Graduate or professional school, no degree","Graduate or professional degree",
                             "Gender:Female","Race:Asian American","Race:Black","Race:White","Race:Other",
                             "Political Party:Democrat","Independent","Something else","Interest in Politics:Somewhat interested",
                             "Slightly interested","Not at all interested"
                             ),
          column.labels=c("Intervene", "Intervene"))

#stargazer(start_mod, end_mod, se=list(coeftest(start_mod, start_mod_cl),se=coef(end_mod,end_mod_cl)), style="apsr" )
#stargazer(start_mod,se=list(coeftest(start_mod,start_mod_cl)),type="html")

########################################
######## Back Down: Clustered SE: MODEL1 & MODEL2 ########

end_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
             data=end_stack))
#end_mod_cl<-cluster.vcov(end_mod,end_stack$resp_id)
#coeftest(end_mod,end_mod_cl)
cov3        <- vcovHC(end_mod, type = "HC1")
robust.se3   <- sqrt(diag(cov3))

end_mod_demplus<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation+
                             demAge+demEdu+demGender+demRace+demPID+demInpoli,
             data=end_stack))
cov4        <- vcovHC(end_mod_demplus, type = "HC1")
robust.se4   <- sqrt(diag(cov4))


stargazer(end_mod, end_mod_demplus ,style="apsr",  se = list(robust.se3, robust.se4, NULL),
          notes="Baseline Levels: Spending:Low; Casualties:Low; U.S. Interest:High; Personal Reputation:High; U.S. Public Opinion:Support; U.S. Reputation:High; Age:18-24;
          Education:Did not finish high school; Gender:Male; Race:American Indian; Political Party: Republican; Interest in Politics:Very interested",
          dep.var.labels.include=FALSE, single.row=TRUE,
          covariate.labels=c("Spending: High", "Casualties: High", "Interest: Long-term Interest",
                             "Interest: Little","Personal Reputation: Little",
                             "Public Opinion: Evenly Split","Public Opinion: Against",
                             "U.S. Reputation: Little", "Age:25-34","Age:35-44","Age:45-54","Age:55-64","Age:64 or older",
                             "Education:High school diploma or equivalent","Technical or vocational school",
                             "Some college, no degree","Associate's or two-year college degree","Four-year college degree",
                             "Graduate or professional school, no degree","Graduate or professional degree",
                             "Gender:Female","Race:Asian American","Race:Black","Race:White","Race:Other",
                             "Political Party:Democrat","Independent","Something else","Interest in Politics:Somewhat interested",
                             "Slightly interested","Not at all interested"),
          column.labels=c("Backing Down", "Backing Down"))








##############################
### Interaction Terms #######
#############################
start_stack <- within(start_stack, casualties*public<-relevel(casualties*public, ref="High:Against"))

start_casualties_public_mod<-amce(selected~casualties*public, 
                data=start_stack, cluster=TRUE,respondent.id="resp_id")

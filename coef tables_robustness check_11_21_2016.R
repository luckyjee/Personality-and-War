install.packages("stargazer")
library(stargazer)


# Intervention Coef
stargazer(start_agrhi_mod, start_agrlo_mod, start_exthi_mod, start_extlo_mod, 
          covariate.labels = c("Military: as strong as the US","Military: Weaker","Casualties: Low",
                               "Long-term interest","High Interest",
                               "High Reputation",
                               "Public Opinion: Split", "Public Opinion: Support", 
                               "Economy: Into Recession", "Economy: Recovering","Economy: In good shape"),
          dep.var.labels   = "Intervention",
          column.labels = c("Agreeableness:High", "Agreeableness:Low","Extraversion:High","Extraversion:Low")
)
          



# Backing Down Coef
stargazer(end_exthi_mod, end_extlo_mod, end_neuhi_mod,end_neulo_mod,
          covariate.labels = c("Spending: High","Casualties: High","Interest: Long-term", "Interest: Little",
                               "Personal: Little","Public Opinion: Split","Public Opinion: Against","Reputation: Little"),
          dep.var.labels   = "Backing Down",
          column.labels = c("Extraversion:High", "Extraversion:Low","Neuroticism:High","Neuroticsm:Low")
)


#####################################
# Stability and no carryover effect
#####################################
        # regress task-wise: task_profile: 1+2; 3+4; 5+6
        # no dispositional distinction

#Intervention
start_stack$task<-"0"
start_stack$task<-ifelse(start_stack$task_profile_start %in% c(1,2),"1",start_stack$task)
start_stack$task<-ifelse(start_stack$task_profile_start %in% c(3,4),"2",start_stack$task)
start_stack$task<-ifelse(start_stack$task_profile_start %in% c(5,6),"3",start_stack$task)

# task 1
start_mod_task1<-lm(selected~strength+casualties+interest+reputation+public+economy, 
              data=start_stack[start_stack$task==1,])
# cluster se by "resp_id"

start_mod_task1$clse<-cluster.vcov(start_mod_task1,start_stack[start_stack$task==1,]$resp_id)
s1<-coeftest(start_mod_task1,start_mod_task1$clse)
s1

# task 2
start_mod_task2<-lm(selected~strength+casualties+interest+reputation+public+economy, 
                    data=start_stack[start_stack$task==2,])
# cluster se by "resp_id"

start_mod_task2$clse<-cluster.vcov(start_mod_task2,start_stack[start_stack$task==2,]$resp_id)
s2<-coeftest(start_mod_task2,start_mod_task2$clse)
s2

# task 3
start_mod_task3<-lm(selected~strength+casualties+interest+reputation+public+economy, 
                    data=start_stack[start_stack$task==3,])
# cluster se by "resp_id"

start_mod_task3$clse<-cluster.vcov(start_mod_task3,start_stack[start_stack$task==3,]$resp_id)
s3<-coeftest(start_mod_task3,start_mod_task3$clse)
s3

stargazer(s1,s2,s3,
          covariate.labels = c("Military: as strong as the US","Military: Weaker","Casualties: Low",
                               "Long-term interest","High Interest",
                               "High Reputation",
                               "Public Opinion: Split", "Public Opinion: Support", 
                               "Economy: Into Recession", "Economy: Recovering","Economy: In good shape"),
          dep.var.labels   = "Intervention")



# Backing Down
end_stack$task<-"0"
end_stack$task<-ifelse(end_stack$task_profile_end %in% c(7,8),"1",end_stack$task)
end_stack$task<-ifelse(end_stack$task_profile_end %in% c(9,10),"2",end_stack$task)
end_stack$task<-ifelse(end_stack$task_profile_end %in% c(11,12),"3",end_stack$task)
#task 1
end_mod_task1<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
             data=end_stack[end_stack$task==1,]))

end_mod_task1$clse<-cluster.vcov(end_mod_task1,end_stack[end_stack$task==1,]$resp_id)
e1<-coeftest(end_mod_task1,end_mod_task1$clse)
e1

#task 2
end_mod_task2<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$task==2,]))

end_mod_task2$clse<-cluster.vcov(end_mod_task2,end_stack[end_stack$task==2,]$resp_id)
e2<-coeftest(end_mod_task2,end_mod_task2$clse)
e2

#task 3
end_mod_task3<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$task==3,]))

end_mod_task3$clse<-cluster.vcov(end_mod_task3,end_stack[end_stack$task==3,]$resp_id)
e3<-coeftest(end_mod_task3,end_mod_task3$clse)
e3


stargazer(e1,e2,e3,
          covariate.labels=  c("Spending: High","Casualties: High","Interest: Long-term", "Interest: Little",
            "Personal: Little","Public Opinion: Split","Public Opinion: Against","Reputation: Little"),
          dep.var.labels   = "Backing Down")



#####################################
# No Profile-order effect
#####################################
# regress profile-wise: 1+3+5;  2+4+6
# no dispositional distinction

#Intervention
start_stack$profile<-"0"
start_stack$profile<-ifelse(start_stack$task_profile_start %in% c(1,3,5),"1",start_stack$profile)
start_stack$profile<-ifelse(start_stack$task_profile_start %in% c(2,4,6),"2",start_stack$profile)


# profile 1
start_mod_profile1<-lm(selected~strength+casualties+interest+reputation+public+economy, 
                    data=start_stack[start_stack$profile==1,])
# cluster se by "resp_id"

start_mod_profile1$clse<-cluster.vcov(start_mod_profile1,start_stack[start_stack$profile==1,]$resp_id)
s1<-coeftest(start_mod_profile1,start_mod_profile1$clse)
s1

# profile 2
start_mod_profile2<-lm(selected~strength+casualties+interest+reputation+public+economy, 
                    data=start_stack[start_stack$profile==2,])
# cluster se by "resp_id"

start_mod_profile2$clse<-cluster.vcov(start_mod_profile2,start_stack[start_stack$profile==2,]$resp_id)
s2<-coeftest(start_mod_profile2,start_mod_profile2$clse)
s2

stargazer(s1,s2,
          covariate.labels = c("Military: as strong as the US","Military: Weaker","Casualties: Low",
                               "Long-term interest","High Interest",
                               "High Reputation",
                               "Public Opinion: Split", "Public Opinion: Support", 
                               "Economy: Into Recession", "Economy: Recovering","Economy: In good shape"),
          dep.var.labels   = "Intervention")



#Backing Down
end_stack$profile<-"0"
end_stack$profile<-ifelse(end_stack$task_profile_end %in% c(7,9,11),"1",end_stack$profile)
end_stack$profile<-ifelse(end_stack$task_profile_end %in% c(8,10,12),"2",end_stack$profile)


# profile 1
end_mod_profile1<-lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                       data=end_stack[end_stack$profile==1,])
# cluster se by "resp_id"

end_mod_profile1$clse<-cluster.vcov(end_mod_profile1,end_stack[end_stack$profile==1,]$resp_id)
e1<-coeftest(end_mod_profile1,end_mod_profile1$clse)
e1

# profile 2
end_mod_profile2<-lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                     data=end_stack[end_stack$profile==2,])
# cluster se by "resp_id"

end_mod_profile2$clse<-cluster.vcov(end_mod_profile2,end_stack[end_stack$profile==2,]$resp_id)
e2<-coeftest(end_mod_profile2,end_mod_profile2$clse)
e2

stargazer(e1,e2,
          covariate.labels=  c("Spending: High","Casualties: High","Interest: Long-term", "Interest: Little",
                               "Personal: Little","Public Opinion: Split","Public Opinion: Against","Reputation: Little"),
          dep.var.labels   = "Backing Down")


#####################################
# Randomization
#####################################
# regress attributes on age, gender, education
# See whether I can reject the null of random assignment

# Age
# Intervention
start_stack$demAge<-as.numeric(start_stack$demAge)
str(start_stack$demAge)
start_age<-lm(demAge~strength+casualties+interest+reputation+public+economy, 
                       data=start_stack)
start_age$clse<-cluster.vcov(start_age,start_stack$resp_id)
sage<-coeftest(start_age,start_age$clse)
sage

# Backing Down
end_stack$demAge<-as.numeric(end_stack$demAge)
str(end_stack$demAge)
end_age<-lm(demAge~spending+casualties+interest+personal_reputation+public+US_reputation,
              data=end_stack)
end_age$clse<-cluster.vcov(end_age,end_stack$resp_id)
eage<-coeftest(end_age,end_age$clse)
eage


# Gender
# Intervention
str(start_stack$demGender)
start_stack$demGender<-as.numeric(start_stack$demGender)
str(start_stack$demGender)
start_Gender<-lm(demGender~strength+casualties+interest+reputation+public+economy, 
              data=start_stack)
start_Gender$clse<-cluster.vcov(start_Gender,start_stack$resp_id)
sGender<-coeftest(start_Gender,start_Gender$clse)
sGender

# Backing Down
end_stack$demGender<-as.numeric(end_stack$demGender)
str(end_stack$demGender)
end_Gender<-lm(demGender~spending+casualties+interest+personal_reputation+public+US_reputation,
            data=end_stack)
end_Gender$clse<-cluster.vcov(end_Gender,end_stack$resp_id)
eGender<-coeftest(end_Gender,end_Gender$clse)
eGender


# Race
# Intervention
str(start_stack$demRace)
start_stack$demRace<-as.numeric(start_stack$demRace)
str(start_stack$demRace)
start_Race<-lm(demRace~strength+casualties+interest+reputation+public+economy, 
                 data=start_stack)
start_Race$clse<-cluster.vcov(start_Race,start_stack$resp_id)
sRace<-coeftest(start_Race,start_Race$clse)
sRace

# Backing Down
end_stack$demRace<-as.numeric(end_stack$demRace)
str(end_stack$demRace)
end_Race<-lm(demRace~spending+casualties+interest+personal_reputation+public+US_reputation,
               data=end_stack)
end_Race$clse<-cluster.vcov(end_Race,end_stack$resp_id)
eRace<-coeftest(end_Race,end_Race$clse)
eRace


# Education
# Intervention
str(start_stack$demEdu)
start_stack$demEdu<-as.numeric(start_stack$demEdu)
str(start_stack$demEdu)
start_Edu<-lm(demEdu~strength+casualties+interest+reputation+public+economy, 
               data=start_stack)
start_Edu$clse<-cluster.vcov(start_Edu,start_stack$resp_id)
sEdu<-coeftest(start_Edu,start_Edu$clse)
sEdu

# Backing Down
end_stack$demEdu<-as.numeric(end_stack$demEdu)
str(end_stack$demEdu)
end_Edu<-lm(demEdu~spending+casualties+interest+personal_reputation+public+US_reputation,
             data=end_stack)
end_Edu$clse<-cluster.vcov(end_Edu,end_stack$resp_id)
eEdu<-coeftest(end_Edu,end_Edu$clse)
eEdu

# coef tables
stargazer(sage, sRace, sGender, sEdu,
          covariate.labels = c("Military: as strong as the US","Military: Weaker","Casualties: Low",
                               "Long-term interest","High Interest",
                               "High Reputation",
                               "Public Opinion: Split", "Public Opinion: Support", 
                               "Economy: Into Recession", "Economy: Recovering","Economy: In good shape"),
          dep.var.labels   = "Intervention")

stargazer(eage, eRace, eGender, eEdu,
          covariate.labels=  c("Spending: High","Casualties: High","Interest: Long-term", "Interest: Little",
                               "Personal: Little","Public Opinion: Split","Public Opinion: Against","Reputation: Little"),
          dep.var.labels   = "Backing Down")

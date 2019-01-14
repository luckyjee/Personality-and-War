##################################
## Demographic Data Manipulation
##################################

df_dem<-df_raw %>%
        select(age:pid,id,- attentive_test)

df_dem[sapply(df_dem, is.factor)] <- lapply(df_dem[sapply(df_dem, is.factor)], 
                                            as.character)

### Age
df_dem$demAge=rep(NA,dim(df_dem)[1])
df_dem$demAge[df_dem$age=="18-24"]<-"1"
df_dem$demAge[df_dem$age=="25-34"]<-"2"
df_dem$demAge[df_dem$age=="35-44"]<-"3"
df_dem$demAge[df_dem$age=="45-54"]<-"4"
df_dem$demAge[df_dem$age=="55-64"]<-"5"
df_dem$demAge[df_dem$age=="64 or older"]<-"6"
#levels(df_dem$demAge) = c("18-24","25-34","35-44","45-54","55-64","64 or older")

### Education
df_dem$demEdu=rep(NA,dim(df_dem)[1])
df_dem$demEdu[df_dem$education=="Did not finish high school"]<-"1"
df_dem$demEdu[df_dem$education=="High school diploma or equivalent, no further schooling"]<-"2"
df_dem$demEdu[df_dem$education=="Technical or vocational school after high school"]<-"3"
df_dem$demEdu[df_dem$education=="Some college, no degree"]<-"4"
df_dem$demEdu[df_dem$education=="Associate's or two-year college degree"]<-"5"
df_dem$demEdu[df_dem$education=="Four-year college degree"]<-"6"
df_dem$demEdu[df_dem$education=="Graduate or professional school after college, no degree"]<-"7"
df_dem$demEdu[df_dem$education=="Graduate or professional degree"]<-"8"
#levels(df_dem$demEdu) = c("Did not finish high school","High school diploma or equivalent, no further schooling",
#                          "Technical or vocational school after high school","Some college, no degree","Associate's or two-year college degree",
#                          "Four-year college degree","Graduate or professional school after college, no degree",
#                          "Graduate or professional degree")

### Gender
df_dem$demGender=rep(NA,dim(df_dem)[1])
df_dem$demGender[df_dem$gender=="Male"]<-"1"
df_dem$demGender[df_dem$gender=="Female"]<-"2"
#levels(df_dem$demGender) = c("Male","Female")


### Race
df_dem$demRace=rep(NA,dim(df_dem)[1])
df_dem$demRace[df_dem$race=="American Indian"]<-"1"
df_dem$demRace[df_dem$race=="Asian American"]<-"2"
df_dem$demRace[df_dem$race=="Black"]<-"3"
df_dem$demRace[df_dem$race=="White"]<-"4"
df_dem$demRace[df_dem$race=="Other"]<-"5"
#levels(df_dem$demRace) = c("American Indian","Asian American","Black","White","Other")


### Interest in Politics
df_dem$demInpoli=rep(NA,dim(df_dem)[1])
df_dem$demInpoli[df_dem$interest.in.politics=="Very interested"]<-"1"
df_dem$demInpoli[df_dem$interest.in.politics=="Somewhat interested"]<-"2"
df_dem$demInpoli[df_dem$interest.in.politics=="Slightly interested"]<-"3"
df_dem$demInpoli[df_dem$interest.in.politics=="Not at all interested"]<-"4"
#levels(df_dem$demInpoli) = c("Very interested", "Somewhat interested","Slightly interested","Not at all interest")


### Political ID
df_dem$demPID=rep(NA,dim(df_dem)[1])
df_dem$demPID[df_dem$pid=="Republican"]<-"1"
df_dem$demPID[df_dem$pid=="Democrat"]<-"2"
df_dem$demPID[df_dem$pid=="Independent"]<-"3"
df_dem$demPID[df_dem$pid=="Something else"]<-"4"
#levels(df_dem$demPID) = c("Republican","Democrat","Independent","Something Else")


df_dem[sapply(df_dem, is.character)] <- lapply(df_dem[sapply(df_dem, is.character)], 
                                            as.factor)


df_dem<-df_dem %>%
        select(demAge, demEdu, demGender, demRace, demInpoli, demPID)






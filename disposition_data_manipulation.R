##################################
## Disposition Data Manipulation
##################################

df_dis<-df_raw %>%
        select(E_1:O_44,id)

df_dis[sapply(df_dis, is.factor)] <- lapply(df_dis[sapply(df_dis, is.factor)], 
                                            as.character)

df_dis[,c(1:44)][df_dis[,c(1:44)]=="Strongly Agree"]<-"5"
df_dis[,c(1:44)][df_dis[,c(1:44)]=="Agree"]<-"4"
df_dis[,c(1:44)][df_dis[,c(1:44)]=="Neither Agree nor Disagree"]<-"3"
df_dis[,c(1:44)][df_dis[,c(1:44)]=="Disagree"]<-"2"
df_dis[,c(1:44)][df_dis[,c(1:44)]=="Strongly Disagree"]<-"1"

## Reverse Score of variables: A_2R,E_6R,C_8R,N_9R,A_12R,C_18R,E_21R,C_23R,N_24R,A_27R,E_31R,N_34R,
## O_35R,A_37R,O_41R,C_43R
df_dis[sapply(df_dis, is.character)] <- lapply(df_dis[sapply(df_dis, is.character)], 
                                               as.numeric)
names(df_dis)

df_dis[,c(6,21,31,2,12,27,37,8,18,23,43,9,24,34,35,41)]<-abs(6-df_dis[,c(6,21,31,2,12,27,37,8,18,23,43,9,24,34,35,41)])

## Calculating average score for Big Five
df_dis$extraversion<-rowMeans(subset(df_dis, select=c(1,6,11,16,21,26,31,36)))
df_dis$agreeableness<-rowMeans(subset(df_dis, select=c(2,7,12,17,22,27,32,37,42)))
df_dis$conscientiousness<-rowMeans(subset(df_dis, select=c(3,8,13,18,23,28,33,38,43)))
df_dis$neuroticism<-rowMeans(subset(df_dis,select=c(4,9,14,19,24,29,34,39)))
df_dis$openness<-rowMeans(subset(df_dis, select=c(5,10,15,20,25,30,35,40,41,44)))

## Divide sample into high- and low-scoring subgroups
        ## high: >=4; low<2 (but this is subject to interpretation)
        ## high: >3; low<3 (subject to interpretation)
        ## median value 


df_dis$extraversionHi=NA
df_dis$agreeablenessHi=NA
df_dis$conscientiousnessHi=NA
df_dis$neuroticismHi=NA
df_dis$opennessHi=NA

#df_dis$extraversionHi = ifelse(df_dis$extraversion>=4,1,0)
#df_dis$agreeablenessHi = ifelse(df_dis$agreeableness>=4,1,0)
#df_dis$conscientiousnessHi = ifelse(df_dis$conscientiousness>=4,1,0)
#df_dis$neuroticismHi = ifelse(df_dis$neuroticism>=4,1,0)
#df_dis$opennessHi = ifelse(df_dis$openness>=4,1,0)

# high: >3; low <3
df_dis$extraversionHi = ifelse(df_dis$extraversion>3,1,0)
df_dis$agreeablenessHi = ifelse(df_dis$agreeableness>3,1,0)
df_dis$conscientiousnessHi = ifelse(df_dis$conscientiousness>3,1,0)
df_dis$neuroticismHi = ifelse(df_dis$neuroticism>3,1,0)
df_dis$opennessHi = ifelse(df_dis$openness>3,1,0)

df_dis$extraversionLo=NA
df_dis$agreeablenessLo=NA
df_dis$conscientiousnessLo=NA
df_dis$neuroticismLo=NA
df_dis$opennessLo=NA

#df_dis$extraversionLo = ifelse(df_dis$extraversion<=2,1,0)
#df_dis$agreeablenessLo = ifelse(df_dis$agreeableness<=2,1,0)
#df_dis$conscientiousnessLo = ifelse(df_dis$conscientiousness<=2,1,0)
#df_dis$neuroticismLo = ifelse(df_dis$neuroticism<=2,1,0)
#df_dis$opennessLo = ifelse(df_dis$openness<=2,1,0)


df_dis$extraversionLo = ifelse(df_dis$extraversion<3,1,0)
df_dis$agreeablenessLo = ifelse(df_dis$agreeableness<3,1,0)
df_dis$conscientiousnessLo = ifelse(df_dis$conscientiousness<3,1,0)
df_dis$neuroticismLo = ifelse(df_dis$neuroticism<3,1,0)
df_dis$opennessLo = ifelse(df_dis$openness<3,1,0)

names(df_dis)

disposition<-df_dis %>%
        select(id:opennessLo)


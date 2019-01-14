
###################################
## clear workspace
## load R libraries
####################################

rm(list=ls())
setwd("/Users/jihyunshin/Dropbox/Experiment_Summer2015")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(survey)
library(cjoint)
library(boot)
library(knitr)
library(rmarkdown)
library(stargazer)
library(plm)
library(lmtest)
library(multiwayvcov)

####################################
## Read the data 
###################################
df_raw<-read.csv("raw_qualtrics.csv")
df_raw$id = seq(1,nrow(df_raw)) 


##################################
## Disposition Data Manipulation
##################################
source("disposition_data_manipulation.R")


#########################
## Append post-stratification weights to the data frame
#########################
source("post_stratify.R")

##################################
## Demographic Data Manipulation
##################################
source("demographic_data_manipulation.R")

################################################
## stack data frames for "Starting a War" Scenario
## 6 rows per respondent (2 conflicts x 3 scenarios)
###############################################
source("stack_data_frame_startawar.R")



################################################
## stack data frames for "Ending a War" Scenario
## 6 rows per respondent (2 conflicts x 3 scenarios)
###############################################
source("stack_data_frame_endawar.R")

## Note: create a data frame on demographics 

#########################
## Run Models
#########################

## 1
## Main effects - LOOKING GREAT!! 
## simple OLS
#summary(lm(selected~strength+casualties+interest+economy+public+reputation,
#           data=start_stack))


######## cluster SE
#summary(amce(selected~strength+casualties+interest+economy+public+reputation, 
#             data=start_stack, cluster=TRUE,respondent.id="resp_id"))

######## bootstrap 
#boot.fn=function(data, index)
#        coefficients(lm(selected~strength+casualties, 
#                          data=start_stack, subset=index))

#set.seed(1)
#boot(start_stack, boot.fn,10000)
#######

#summary(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
#           data=end_stack))

#summary(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
#           data=end_stack))

## Note1: Control for Demographics
## Note2: Make tables for paper





##############################
### Create Main Effects Figures
###############################

########################### Main effects - Start a War ##################################
#source("maineffect_startawar.R")
##### (START) No clustering SE #######
#start_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
#              data=start_stack))

## Change the reference categories
#start_stack <- within(start_stack, casualties<-relevel(casualties, ref="Low"))
#start_stack <- within(start_stack, economy<-relevel(economy, ref="Recovering from recession"))
#start_stack <- within(start_stack, interest<-relevel(interest, ref="Little"))
#start_stack <- within(start_stack, reputation<-relevel(reputation, ref="Little"))
#start_stack <- within(start_stack, public<-relevel(public, ref="Evenly Split"))

#start_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
#               data=start_stack))

#start_vars=variable.names(start_mod)
#start_coefs = c(summary(start_mod)$coef[,1])
#start_err = c(summary(start_mod)$coef[,2]*1.96)
#start_tvals = c(summary(start_mod)$coef[,4])
#start_tmp = data.frame(cbind(start_coefs,start_err))
######### (END) No clustering SE #########

start_stack <- within(start_stack, strength<-relevel(strength, ref="Strong"))
start_stack <- within(start_stack, casualties<-relevel(casualties, ref="High"))
start_stack <- within(start_stack, economy<-relevel(economy, ref="In recession"))
start_stack <- within(start_stack, interest<-relevel(interest, ref="Little"))
start_stack <- within(start_stack, reputation<-relevel(reputation, ref="Little"))
start_stack <- within(start_stack, public<-relevel(public, ref="Against"))

start_stack$economy <- factor(start_stack$economy, levels=c("In recession","Sliding into recession","Recovering from recession",
                                                            "In good shape"))

start_stack$interest <- factor(start_stack$interest, levels=c("Little","Not immediate, but long-term interest",
                                                            "High"))

######### (START) using cjoint package: CLUSTER SE ########## 
start_mod<-amce(selected~strength+casualties+interest+reputation+public+economy, 
                data=start_stack, cluster=TRUE,respondent.id="resp_id")

start_mod_demplus<-amce(selected~strength+casualties+interest+reputation+public+economy+
                                demAge+demEdu+demGender+demRace+demPID+demInpoli, 
                data=start_stack, cluster=TRUE,respondent.id="resp_id")

start_vars=variable.names(start_mod)
start_coefs = c(summary(start_mod)$est[,3])
start_err = c(summary(start_mod)$est[,4]*1.96)
start_tvals = c(summary(start_mod)$est[,6])
start_tmp = data.frame(cbind(start_coefs,start_err))
############# (END) using cjoint package ####################


## select particular coefficients
start_tmp2=start_tmp
start_tmp2$name = row.names(start_tmp2)
start_tmp2 = start_tmp2[,c("name","start_coefs","start_err")]
names(start_tmp2) = c("variable","coef","se")

#start_tmp3<-start_tmp2[-1,] #remove the intercept row 
start_tmp3<-start_tmp2
start_tmp3$variable = as.factor(start_tmp3$variable)
start_tmp3$coef<-as.numeric(as.vector(start_tmp3$coef))
start_tmp3$se<-as.numeric(as.vector(start_tmp3$se))
start_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                       "Low (casualties)",
                       "Long-term interest","High (interest)",
                  "High (reputation)",
                 "Evenly Split", "Support", 
                 "Sliding into recession", "Recovering from recession","In good shape")


start_variables=read.csv("start_variables.csv")
for (i in 1:dim(start_variables)[1]){
        if (sum(start_tmp3$variable==start_variables$variable[i], na.rm=T)>0){
                start_variables$coef[i]<-start_tmp3$coef[start_tmp3$variable==start_variables$variable[i]]
                start_variables$se[i]<-start_tmp3$se[start_tmp3$variable==start_variables$variable[i]]
        }
}

start_variables2 = start_variables
start_variables2$coef[is.na(start_variables2$coef)]<-""
start_variables2$se[is.na(start_variables2$se)]<-""
start_variables2$variable<-as.vector(start_variables2$variable)
start_variables2$coef<-as.numeric(as.vector(start_variables2$coef))
start_variables2$se<-as.numeric(as.vector(start_variables2$se))

order<-1:dim(start_variables2)[1]
start_variables2 <- transform(start_variables2, variable2=reorder(variable2, -order) ) 

title<-paste("Effects of Attributes on Intervention")

start_p=ggplot(data = start_variables2, aes(x = coef, y = variable2)) +
        geom_point(size = 3)+
        geom_errorbarh( aes(y = variable2, xmin = coef - se, xmax = coef + se, height=.28),size=1)+
        xlab("Change in Intervention")+
        xlim(-.2,.3)+
        theme(axis.title=element_text(size=11,face="bold"))+
        ##theme_bw()+
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title.y = element_blank()) +
        geom_vline(xintercept = 0,size=0.6,colour="blue",linetype="dotted") +
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=13, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=13))+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(legend.position = "none")
start_p

   

ggsave(start_p, file = "main_effects_clustered_intervention.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015")


########################### Main effects - End a War ##################################
#source("maineffect_endawar.R")

## change the reference categories
end_stack <- within(end_stack, personal_reputation <- relevel(personal_reputation, ref = "Humiliation if you back down"))
end_stack <- within(end_stack, casualties<-relevel(casualties, ref="Low"))
end_stack <- within(end_stack, spending<-relevel(spending, ref="Low"))
end_stack <- within(end_stack, interest<-relevel(interest, ref="High"))
end_stack <- within(end_stack, US_reputation<-relevel(US_reputation, ref="Damaging to Superpower status"))
end_stack <- within(end_stack, public<-relevel(public, ref="Support"))

end_stack$interest <- factor(end_stack$interest, levels=c("High","Not immediate, but long-term interest","Little"))

end_stack$public <- factor(end_stack$public, levels=c("Support","Evenly Split","Against"))

#end_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
#               data=end_stack))

end_mod<-amce(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
              data=end_stack, cluster=TRUE,respondent.id="resp_id")

end_vars=variable.names(end_mod)
end_coefs = c(summary(end_mod)$est[,3])
end_err = c(summary(end_mod)$est[,4]*1.96)
end_tvals = c(summary(end_mod)$est[,6])
end_tmp = data.frame(cbind(end_coefs,end_err))

## select particular coefficients
end_tmp2=end_tmp
end_tmp2$name = row.names(end_tmp2)
end_tmp2 = end_tmp2[,c("name","end_coefs","end_err")]
names(end_tmp2) = c("variable","coef","se")

end_tmp3<-end_tmp2
end_tmp3$variable = as.factor(end_tmp3$variable)
end_tmp3$coef<-as.numeric(as.vector(end_tmp3$coef))
end_tmp3$se<-as.numeric(as.vector(end_tmp3$se))
end_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                     "Little (personal)","Evenly Split","Against","Little (reputation)")




end_variables=read.csv("end_variables.csv")
for (i in 1:dim(end_variables)[1]){
        if (sum(end_tmp3$variable==end_variables$variable[i], na.rm=T)>0){
                end_variables$coef[i]<-end_tmp3$coef[end_tmp3$variable==end_variables$variable[i]]
                end_variables$se[i]<-end_tmp3$se[end_tmp3$variable==end_variables$variable[i]]
        }
}

end_variables2 = end_variables
end_variables2$coef[is.na(end_variables2$coef)]<-""
end_variables2$se[is.na(end_variables2$se)]<-""
end_variables2$variable<-as.vector(end_variables2$variable)
end_variables2$coef<-as.numeric(as.vector(end_variables2$coef))
end_variables2$se<-as.numeric(as.vector(end_variables2$se))

order<-1:dim(end_variables2)[1]
end_variables2 <- transform(end_variables2, variable2=reorder(variable2, -order) ) 

title<-paste("Effects of Attributes on Backing Down")

end_p=ggplot(data = end_variables2, aes(x = coef, y = variable2)) +
        geom_point(size = 3)+
        geom_errorbarh( aes(y = variable2, xmin = coef - se, xmax = coef + se, height=.28),size=1)+
        xlab("Change in Backing Down")+
        xlim(-.2,.2)+
        theme(axis.title=element_text(size=11,face="bold"))+
        ##theme_bw()+
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title.y = element_blank()) +
        geom_vline(xintercept = 0,size=.6,colour="blue",linetype="dotted") +
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=13, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=13))+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(legend.position = "none")
end_p

ggsave(end_p, file = "main_effects_clustered_end.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015")


########################### Main effects - Increase Military in a War ##########################
#source("maineffect_increasecommitment.R")

## change the reference categories - if this is not already done in the "main effects -end a war" 
end_stack <- within(end_stack, personal_reputation <- relevel(personal_reputation, ref = "Humiliation if you back down"))
end_stack <- within(end_stack, casualties<-relevel(casualties, ref="Low"))
end_stack <- within(end_stack, spending<-relevel(spending, ref="Low"))
end_stack <- within(end_stack, interest<-relevel(interest, ref="High"))
end_stack <- within(end_stack, US_reputation<-relevel(US_reputation, ref="Damaging to Superpower status"))
end_stack <- within(end_stack, public<-relevel(public, ref="Support"))

end_stack$interest <- factor(end_stack$interest, levels=c("High","Not immediate, but long-term interest","Little"))

end_stack$public <- factor(end_stack$public, levels=c("Support","Evenly Split","Against"))

#increase_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
#             data=end_stack))
increase_mod<-amce(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
              data=end_stack, cluster=TRUE,respondent.id="resp_id")

increase_vars=variable.names(increase_mod)
increase_coefs = c(summary(increase_mod)$est[,3])
increase_err = c(summary(increase_mod)$est[,4]*1.96)
increase_tvals = c(summary(increase_mod)$est[,6])
increase_tmp = data.frame(cbind(increase_coefs,increase_err))


## select particular coefficients
increase_tmp2=increase_tmp
increase_tmp2$name = row.names(increase_tmp2)
increase_tmp2 = increase_tmp2[,c("name","increase_coefs","increase_err")]
names(increase_tmp2) = c("variable","coef","se")

increase_tmp3<-increase_tmp2
increase_tmp3$variable = as.factor(increase_tmp3$variable)
increase_tmp3$coef<-as.numeric(as.vector(increase_tmp3$coef))
increase_tmp3$se<-as.numeric(as.vector(increase_tmp3$se))
increase_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                          "Little (personal)","Evenly Split","Against","Little (reputation)")




increase_variables=read.csv("increase_variables.csv")
for (i in 1:dim(increase_variables)[1]){
        if (sum(increase_tmp3$variable==increase_variables$variable[i], na.rm=T)>0){
                increase_variables$coef[i]<-increase_tmp3$coef[increase_tmp3$variable==increase_variables$variable[i]]
                increase_variables$se[i]<-increase_tmp3$se[increase_tmp3$variable==increase_variables$variable[i]]
        }
}

increase_variables2 = increase_variables
increase_variables2$coef[is.na(increase_variables2$coef)]<-""
increase_variables2$se[is.na(increase_variables2$se)]<-""
increase_variables2$variable<-as.vector(increase_variables2$variable)
increase_variables2$coef<-as.numeric(as.vector(increase_variables2$coef))
increase_variables2$se<-as.numeric(as.vector(increase_variables2$se))

order<-1:dim(increase_variables2)[1]
increase_variables2 <- transform(increase_variables2, variable2=reorder(variable2, -order) ) 

title<-paste("Effects of Attributes on Increasing Commitment")

increase_p=ggplot(data = increase_variables2, aes(x = coef, y = variable2)) +
        geom_point(size = 3)+
        geom_errorbarh( aes(y = variable2, xmin = coef - se, xmax = coef + se, height=.28),size=1)+
        xlab("Change in Commitment")+
        theme(axis.title=element_text(size=11,face="bold"))+
        xlim(-.2,.2)+
        ##theme_bw()+
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title.y = element_blank()) +
        geom_vline(xintercept = 0,size=.6,colour="blue",linetype="dotted") +
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=13, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=13))+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(legend.position = "none")

increase_p

ggsave(increase_p, file = "main_effects_clustered_increase.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015")


## Combine main effects graphs for "end" and "increase" 

grid.arrange(end_p, increase_p, ncol=2, widths=1.99:1,heights=1.99:1)

## or:
grid.arrange(start_p, end_p, increase_p, ncol=1)






####################################
### Dispostional Effects ##########
###################################

#################### Start A War #####################
##################### Extraversion ###################

## Extraversion -High
nrow(start_stack[start_stack$extraversionHi==1,]) #n=828; n=2376

start_exthi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
           data=start_stack[start_stack$extraversionHi==1,],weight=disWts))

start_exthi_vars=variable.names(start_exthi_mod)
start_exthi_coefs = c(summary(start_exthi_mod)$coef[,1])
start_exthi_err = c(summary(start_exthi_mod)$coef[,2]*1.96)
start_exthi_tvals = c(summary(start_exthi_mod)$coef[,4])
start_exthi_tmp = data.frame(cbind(start_exthi_coefs,start_exthi_err))

## select particular coefficients
start_exthi_tmp2=start_exthi_tmp
start_exthi_tmp2$name = row.names(start_exthi_tmp2)
start_exthi_tmp2 = start_exthi_tmp2[,c("name","start_exthi_coefs","start_exthi_err")]
names(start_exthi_tmp2) = c("variable","coef","se")

start_exthi_tmp3<-start_exthi_tmp2[-1,]
start_exthi_tmp3$variable = as.factor(start_exthi_tmp3$variable)
start_exthi_tmp3$coef<-as.numeric(as.vector(start_exthi_tmp3$coef))
start_exthi_tmp3$se<-as.numeric(as.vector(start_exthi_tmp3$se))
start_exthi_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                              "Low (casualties)",
                              "Long-term interest","High (interest)",
                              "High (reputation)",
                              "Evenly Split", "Support", 
                              "Sliding into recession", "Recovering from recession","In good shape")

start_exthi_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_exthi_variables)[1]){
        if (sum(start_exthi_tmp3$variable==start_exthi_variables$variable[i], na.rm=T)>0){
                start_exthi_variables$coef[i]<-start_exthi_tmp3$coef[start_exthi_tmp3$variable==start_exthi_variables$variable[i]]
                start_exthi_variables$se[i]<-start_exthi_tmp3$se[start_exthi_tmp3$variable==start_exthi_variables$variable[i]]
        }
}

start_exthi_variables2 = start_exthi_variables
start_exthi_variables2$coef[is.na(start_exthi_variables2$coef)]<-""
start_exthi_variables2$se[is.na(start_exthi_variables2$se)]<-""
start_exthi_variables2$variable<-as.vector(start_exthi_variables2$variable)
start_exthi_variables2$coef<-as.numeric(as.vector(start_exthi_variables2$coef))
start_exthi_variables2$se<-as.numeric(as.vector(start_exthi_variables2$se))

order<-1:dim(start_exthi_variables2)[1]
start_exthi_variables2 <- transform(start_exthi_variables2, variable2=reorder(variable2, -order) ) 


## Extraversion - Low 
nrow(start_stack[start_stack$extraversionLo==1,]) # n=966; n=3270

start_extlo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$extraversionLo==1,],weight=disWts))

start_extlo_vars=variable.names(start_extlo_mod)
start_extlo_coefs = c(summary(start_extlo_mod)$coef[,1])
start_extlo_err = c(summary(start_extlo_mod)$coef[,2]*1.96)
start_extlo_tvals = c(summary(start_extlo_mod)$coef[,4])
start_extlo_tmp = data.frame(cbind(start_extlo_coefs,start_extlo_err))

## select particular coefficients
start_extlo_tmp2=start_extlo_tmp
start_extlo_tmp2$name = row.names(start_extlo_tmp2)
start_extlo_tmp2 = start_extlo_tmp2[,c("name","start_extlo_coefs","start_extlo_err")]
names(start_extlo_tmp2) = c("variable","coef","se")

start_extlo_tmp3<-start_extlo_tmp2[-1,]
start_extlo_tmp3$variable = as.factor(start_extlo_tmp3$variable)
start_extlo_tmp3$coef<-as.numeric(as.vector(start_extlo_tmp3$coef))
start_extlo_tmp3$se<-as.numeric(as.vector(start_extlo_tmp3$se))
start_extlo_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                             "Low (casualties)",
                             "Long-term interest","High (interest)",
                             "High (reputation)",
                             "Evenly Split", "Support", 
                             "Sliding into recession", "Recovering from recession","In good shape")


start_extlo_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_extlo_variables)[1]){
        if (sum(start_extlo_tmp3$variable==start_extlo_variables$variable[i], na.rm=T)>0){
                start_extlo_variables$coef[i]<-start_extlo_tmp3$coef[start_extlo_tmp3$variable==start_extlo_variables$variable[i]]
                start_extlo_variables$se[i]<-start_extlo_tmp3$se[start_extlo_tmp3$variable==start_extlo_variables$variable[i]]
        }
}

start_extlo_variables2 = start_extlo_variables
start_extlo_variables2$coef[is.na(start_extlo_variables2$coef)]<-""
start_extlo_variables2$se[is.na(start_extlo_variables2$se)]<-""
start_extlo_variables2$variable<-as.vector(start_extlo_variables2$variable)
start_extlo_variables2$coef<-as.numeric(as.vector(start_extlo_variables2$coef))
start_extlo_variables2$se<-as.numeric(as.vector(start_extlo_variables2$se))

order<-1:dim(start_extlo_variables2)[1]
start_extlo_variables2 <- transform(start_extlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low

start_ext <- rbind(start_exthi_variables2,start_extlo_variables2)

start_ext$id<-1:dim(start_ext)[1]
        
start_ext$dis<-NA
start_ext$dis[start_ext$id %in% c(1:23)] <- "High"
start_ext$dis[start_ext$id %in% c(24:46)] <- "Low"
start_ext$dis[start_ext$id %in% c(2,6,9,13,16,20,25,29,32,36,39,43)] <- "Baseline"

#### Plotting 

title<-paste("Intervention-Extraversion")

start_ext_p= ggplot(data = start_ext, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=13, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=13))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

start_ext_p        

ggsave(start_ext_p, file = "weighted_start_ext_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")




#################### Start A War #####################
##################### Agreeableness ###################
#source("startawar_agreeableness.R")
### note: when changed to "high: >=3; low <3" -> high agreealbeness less willing to intervene 
### when casualties high (significant). 

## Agreeableness -High
nrow(start_stack[start_stack$agreeablenessHi==1,]) # n=2346; n=5046

start_agrhi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$agreeablenessHi==1,], weight=disWts))

start_agrhi_vars=variable.names(start_agrhi_mod)
start_agrhi_coefs = c(summary(start_agrhi_mod)$coef[,1])
start_agrhi_err = c(summary(start_agrhi_mod)$coef[,2]*1.96)
start_agrhi_tvals = c(summary(start_agrhi_mod)$coef[,4])
start_agrhi_tmp = data.frame(cbind(start_agrhi_coefs,start_agrhi_err))

## select particular coefficients
start_agrhi_tmp2=start_agrhi_tmp
start_agrhi_tmp2$name = row.names(start_agrhi_tmp2)
start_agrhi_tmp2 = start_agrhi_tmp2[,c("name","start_agrhi_coefs","start_agrhi_err")]
names(start_agrhi_tmp2) = c("variable","coef","se")

start_agrhi_tmp3<-start_agrhi_tmp2[-1,]
start_agrhi_tmp3$variable = as.factor(start_agrhi_tmp3$variable)
start_agrhi_tmp3$coef<-as.numeric(as.vector(start_agrhi_tmp3$coef))
start_agrhi_tmp3$se<-as.numeric(as.vector(start_agrhi_tmp3$se))
start_agrhi_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                             "Low (casualties)",
                             "Long-term interest","High (interest)",
                             "High (reputation)",
                             "Evenly Split", "Support", 
                             "Sliding into recession", "Recovering from recession","In good shape")


start_agrhi_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_agrhi_variables)[1]){
        if (sum(start_agrhi_tmp3$variable==start_agrhi_variables$variable[i], na.rm=T)>0){
                start_agrhi_variables$coef[i]<-start_agrhi_tmp3$coef[start_agrhi_tmp3$variable==start_agrhi_variables$variable[i]]
                start_agrhi_variables$se[i]<-start_agrhi_tmp3$se[start_agrhi_tmp3$variable==start_agrhi_variables$variable[i]]
        }
}

start_agrhi_variables2 = start_agrhi_variables
start_agrhi_variables2$coef[is.na(start_agrhi_variables2$coef)]<-""
start_agrhi_variables2$se[is.na(start_agrhi_variables2$se)]<-""
start_agrhi_variables2$variable<-as.vector(start_agrhi_variables2$variable)
start_agrhi_variables2$coef<-as.numeric(as.vector(start_agrhi_variables2$coef))
start_agrhi_variables2$se<-as.numeric(as.vector(start_agrhi_variables2$se))

order<-1:dim(start_agrhi_variables2)[1]
start_agrhi_variables2 <- transform(start_agrhi_variables2, variable2=reorder(variable2, -order) ) 


## Agreealbeness - Low 
nrow(start_stack[start_stack$agreeablenessLo==1,]) #only n=78; n=900

start_agrlo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$agreeablenessLo==1,],weight=disWts))

start_agrlo_vars=variable.names(start_agrlo_mod)
start_agrlo_coefs = c(summary(start_agrlo_mod)$coef[,1])
start_agrlo_err = c(summary(start_agrlo_mod)$coef[,2]*1.96)
start_agrlo_tvals = c(summary(start_agrlo_mod)$coef[,4])
start_agrlo_tmp = data.frame(cbind(start_agrlo_coefs,start_agrlo_err))

## select particular coefficients
start_agrlo_tmp2=start_agrlo_tmp
start_agrlo_tmp2$name = row.names(start_agrlo_tmp2)
start_agrlo_tmp2 = start_agrlo_tmp2[,c("name","start_agrlo_coefs","start_agrlo_err")]
names(start_agrlo_tmp2) = c("variable","coef","se")

start_agrlo_tmp3<-start_agrlo_tmp2[-1,]
start_agrlo_tmp3$variable = as.factor(start_agrlo_tmp3$variable)
start_agrlo_tmp3$coef<-as.numeric(as.vector(start_agrlo_tmp3$coef))
start_agrlo_tmp3$se<-as.numeric(as.vector(start_agrlo_tmp3$se))
start_agrlo_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                             "Low (casualties)",
                             "Long-term interest","High (interest)",
                             "High (reputation)",
                             "Evenly Split", "Support", 
                             "Sliding into recession", "Recovering from recession","In good shape")


start_agrlo_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_agrlo_variables)[1]){
        if (sum(start_agrlo_tmp3$variable==start_agrlo_variables$variable[i], na.rm=T)>0){
                start_agrlo_variables$coef[i]<-start_agrlo_tmp3$coef[start_agrlo_tmp3$variable==start_agrlo_variables$variable[i]]
                start_agrlo_variables$se[i]<-start_agrlo_tmp3$se[start_agrlo_tmp3$variable==start_agrlo_variables$variable[i]]
        }
}

start_agrlo_variables2 = start_agrlo_variables
start_agrlo_variables2$coef[is.na(start_agrlo_variables2$coef)]<-""
start_agrlo_variables2$se[is.na(start_agrlo_variables2$se)]<-""
start_agrlo_variables2$variable<-as.vector(start_agrlo_variables2$variable)
start_agrlo_variables2$coef<-as.numeric(as.vector(start_agrlo_variables2$coef))
start_agrlo_variables2$se<-as.numeric(as.vector(start_agrlo_variables2$se))

order<-1:dim(start_agrlo_variables2)[1]
start_agrlo_variables2 <- transform(start_agrlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
start_agr <- rbind(start_agrhi_variables2,start_agrlo_variables2)

start_agr$id<-1:dim(start_agr)[1]

start_agr$dis<-NA
start_agr$dis[start_agr$id %in% c(1:23)] <- "High"
start_agr$dis[start_agr$id %in% c(24:46)] <- "Low"
start_agr$dis[start_ext$id %in% c(2,6,9,13,16,20,25,29,32,36,39,43)] <- "Baseline"

#### Plotting 

title<-paste("Intervention-Agreeableness")

start_agr_p = ggplot(data = start_agr, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.4)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

start_agr_p        
# Note: low agreeableness sample size was too small, n=78
ggsave(start_agr_p, file = "weighted_start_agr_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")




######################## Start a War ######################
##################### Conscientiousness ###################
#source("startawar_conscientiousness.R")
### note: counterintuitive: why does low conscientiousness more willing to intervene?


## Conscientiousness-High
nrow(start_stack[start_stack$conscientiousnessHi==1,]) #n=3150, n=5220

start_conshi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$conscientiousnessHi==1,],weight=disWts))
## Try logit 
#start_conhi_mod<- summary(glm(selected~strength+casualties+interest+reputation+public+economy,
#            data=start_stack[start_stack$conscientiousnessLo==1,], weight=disWts, family="binomial"))
##

start_conshi_vars=variable.names(start_conshi_mod)
start_conshi_coefs = c(summary(start_conshi_mod)$coef[,1])
start_conshi_err = c(summary(start_conshi_mod)$coef[,2]*1.96)
start_conshi_tvals = c(summary(start_conshi_mod)$coef[,4])
start_conshi_tmp = data.frame(cbind(start_conshi_coefs,start_conshi_err))

## select particular coefficients
start_conshi_tmp2=start_conshi_tmp
start_conshi_tmp2$name = row.names(start_conshi_tmp2)
start_conshi_tmp2 = start_conshi_tmp2[,c("name","start_conshi_coefs","start_conshi_err")]
names(start_conshi_tmp2) = c("variable","coef","se")

start_conshi_tmp3<-start_conshi_tmp2[-1,]
start_conshi_tmp3$variable = as.factor(start_conshi_tmp3$variable)
start_conshi_tmp3$coef<-as.numeric(as.vector(start_conshi_tmp3$coef))
start_conshi_tmp3$se<-as.numeric(as.vector(start_conshi_tmp3$se))
start_conshi_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                              "Low (casualties)",
                              "Long-term interest","High (interest)",
                              "High (reputation)",
                              "Evenly Split", "Support", 
                              "Sliding into recession", "Recovering from recession","In good shape")



start_conshi_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_conshi_variables)[1]){
        if (sum(start_conshi_tmp3$variable==start_conshi_variables$variable[i], na.rm=T)>0){
                start_conshi_variables$coef[i]<-start_conshi_tmp3$coef[start_conshi_tmp3$variable==start_conshi_variables$variable[i]]
                start_conshi_variables$se[i]<-start_conshi_tmp3$se[start_conshi_tmp3$variable==start_conshi_variables$variable[i]]
        }
}

start_conshi_variables2 = start_conshi_variables
start_conshi_variables2$coef[is.na(start_conshi_variables2$coef)]<-""
start_conshi_variables2$se[is.na(start_conshi_variables2$se)]<-""
start_conshi_variables2$variable<-as.vector(start_conshi_variables2$variable)
start_conshi_variables2$coef<-as.numeric(as.vector(start_conshi_variables2$coef))
start_conshi_variables2$se<-as.numeric(as.vector(start_conshi_variables2$se))

order<-1:dim(start_conshi_variables2)[1]
start_conshi_variables2 <- transform(start_conshi_variables2, variable2=reorder(variable2, -order) ) 


## Conscientiousness - Low 
nrow(start_stack[start_stack$conscientiousnessLo==1,]) # n=42, n=576

start_conlo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$conscientiousnessLo==1,],weight=disWts))

## Try logit
#start_conlo_mod<-summary(glm(selected~strength+casualties+interest+reputation+public+economy,
#       data=start_stack[start_stack$conscientiousnessLo==1,], weight=disWts, family="binomial"))

##amce: cluster SE, but no weights
#summary(amce(selected~strength+casualties+interest+reputation+public+economy,
#             data=start_stack[start_stack$conscientiousnessLo==1,], 
#             cluster=TRUE,respondent.id="resp_id"))

#summary(amce(selected~strength+casualties+interest+reputation+public+economy,
#             data=start_stack[start_stack$conscientiousnessLo==1,],weights=disWts, # error
#             cluster=TRUE,respondent.id="resp_id"))

## amce; Cluster SE; weight
#cl   <- function(dat,fm, cluster){
#        require(sandwich, quietly = TRUE)
#        require(lmtest, quietly = TRUE)
#        M <- length(unique(cluster))
#        N <- length(cluster)
#        K <- fm$rank
#        dfc <- (M/(M-1))*((N-1)/(N-K))
#        uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
#        vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
#        coeftest(fm, vcovCL) }

#start_conlo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
#     data=start_stack[start_stack$conscientiousnessLo==1,],weight=disWts))
#cl(start_stack[start_stack$conscientiousnessLo==1,], 
#   start_conlo_mod, start_stack[start_stack$conscientiousnessLo==1,]$resp_id)

#######bootstrap: SE in bootstrap is larger than the original
#boot.fn=function(data,index)
#        coefficients(lm(selected~strength+casualties+interest+reputation+public+economy,
#                        data=start_stack[start_stack$conscientiousnessLo==1,],
#                        weight=disWts, subset=index))
#set.seed(1)
#boot(start_stack[start_stack$conscientiousnessLo==1,], boot.fn, 100000)
##
##

start_conlo_vars=variable.names(start_conlo_mod)
start_conlo_coefs = c(summary(start_conlo_mod)$coef[,1])
start_conlo_err = c(summary(start_conlo_mod)$coef[,2]*1.96)
start_conlo_tvals = c(summary(start_conlo_mod)$coef[,4])
start_conlo_tmp = data.frame(cbind(start_conlo_coefs,start_conlo_err))

## select particular coefficients
start_conlo_tmp2=start_conlo_tmp
start_conlo_tmp2$name = row.names(start_conlo_tmp2)
start_conlo_tmp2 = start_conlo_tmp2[,c("name","start_conlo_coefs","start_conlo_err")]
names(start_conlo_tmp2) = c("variable","coef","se")

start_conlo_tmp3<-start_conlo_tmp2[-1,]
start_conlo_tmp3$variable = as.factor(start_conlo_tmp3$variable)
start_conlo_tmp3$coef<-as.numeric(as.vector(start_conlo_tmp3$coef))
start_conlo_tmp3$se<-as.numeric(as.vector(start_conlo_tmp3$se))
start_conlo_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                             "Low (casualties)",
                             "Long-term interest","High (interest)",
                             "High (reputation)",
                             "Evenly Split", "Support", 
                             "Sliding into recession", "Recovering from recession","In good shape")



start_conlo_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_conlo_variables)[1]){
        if (sum(start_conlo_tmp3$variable==start_conlo_variables$variable[i], na.rm=T)>0){
                start_conlo_variables$coef[i]<-start_conlo_tmp3$coef[start_conlo_tmp3$variable==start_conlo_variables$variable[i]]
                start_conlo_variables$se[i]<-start_conlo_tmp3$se[start_conlo_tmp3$variable==start_conlo_variables$variable[i]]
        }
}

start_conlo_variables2 = start_conlo_variables
start_conlo_variables2$coef[is.na(start_conlo_variables2$coef)]<-""
start_conlo_variables2$se[is.na(start_conlo_variables2$se)]<-""
start_conlo_variables2$variable<-as.vector(start_conlo_variables2$variable)
start_conlo_variables2$coef<-as.numeric(as.vector(start_conlo_variables2$coef))
start_conlo_variables2$se<-as.numeric(as.vector(start_conlo_variables2$se))

order<-1:dim(start_conlo_variables2)[1]
start_conlo_variables2 <- transform(start_conlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
start_con <- rbind(start_conshi_variables2,start_conlo_variables2)

start_con$id<-1:dim(start_con)[1]

start_con$dis<-NA
start_con$dis[start_con$id %in% c(1:23)] <- "High"
start_con$dis[start_con$id %in% c(24:46)] <- "Low"
start_con$dis[start_con$id %in% c(2,6,9,13,16,20,25,29,32,36,39,43)] <- "Baseline"

#### Plotting 

title<-paste("Intervention-Conscientiousness")

start_con_p<- ggplot(data = start_con, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.4)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_con_p        

ggsave(start_con_p, file = "weighted_start_con_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")



################### Start a War #####################
##################### Neuroticism ###################
#source("startawar_neuroticism.R")
## note: almost exactly the same effects between high and low (when criteria is 3)

## Neuroticism-High
nrow(start_stack[start_stack$neuroticismHi==1,]) #n=504; n=1686

start_neuhi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                      data=start_stack[start_stack$conscientiousnessHi==1,],weight=disWts))

start_neuhi_vars=variable.names(start_neuhi_mod)
start_neuhi_coefs = c(summary(start_neuhi_mod)$coef[,1])
start_neuhi_err = c(summary(start_neuhi_mod)$coef[,2]*1.96)
start_neuhi_tvals = c(summary(start_neuhi_mod)$coef[,4])
start_neuhi_tmp = data.frame(cbind(start_neuhi_coefs,start_neuhi_err))

## select particular coefficients
start_neuhi_tmp2=start_neuhi_tmp
start_neuhi_tmp2$name = row.names(start_neuhi_tmp2)
start_neuhi_tmp2 = start_neuhi_tmp2[,c("name","start_neuhi_coefs","start_neuhi_err")]
names(start_neuhi_tmp2) = c("variable","coef","se")

start_neuhi_tmp3<-start_neuhi_tmp2[-1,]
start_neuhi_tmp3$variable = as.factor(start_neuhi_tmp3$variable)
start_neuhi_tmp3$coef<-as.numeric(as.vector(start_neuhi_tmp3$coef))
start_neuhi_tmp3$se<-as.numeric(as.vector(start_neuhi_tmp3$se))
start_neuhi_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                             "Low (casualties)",
                             "Long-term interest","High (interest)",
                             "High (reputation)",
                             "Evenly Split", "Support", 
                             "Sliding into recession", "Recovering from recession","In good shape")



start_neuhi_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_neuhi_variables)[1]){
        if (sum(start_neuhi_tmp3$variable==start_neuhi_variables$variable[i], na.rm=T)>0){
                start_neuhi_variables$coef[i]<-start_neuhi_tmp3$coef[start_neuhi_tmp3$variable==start_neuhi_variables$variable[i]]
                start_neuhi_variables$se[i]<-start_neuhi_tmp3$se[start_neuhi_tmp3$variable==start_neuhi_variables$variable[i]]
        }
}

start_neuhi_variables2 = start_neuhi_variables
start_neuhi_variables2$coef[is.na(start_neuhi_variables2$coef)]<-""
start_neuhi_variables2$se[is.na(start_neuhi_variables2$se)]<-""
start_neuhi_variables2$variable<-as.vector(start_neuhi_variables2$variable)
start_neuhi_variables2$coef<-as.numeric(as.vector(start_neuhi_variables2$coef))
start_neuhi_variables2$se<-as.numeric(as.vector(start_neuhi_variables2$se))

order<-1:dim(start_neuhi_variables2)[1]
start_neuhi_variables2 <- transform(start_neuhi_variables2, variable2=reorder(variable2, -order) ) 


## Neuroticism - Low 
nrow(start_stack[start_stack$neuroticismLo==1,]) # n=1440; n=3978

start_neulo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                      data=start_stack[start_stack$neuroticismLo==1,],weight=disWts))

start_neulo_vars=variable.names(start_neulo_mod)
start_neulo_coefs = c(summary(start_neulo_mod)$coef[,1])
start_neulo_err = c(summary(start_neulo_mod)$coef[,2]*1.96)
start_neulo_tvals = c(summary(start_neulo_mod)$coef[,4])
start_neulo_tmp = data.frame(cbind(start_neulo_coefs,start_neulo_err))

## select particular coefficients
start_neulo_tmp2=start_neulo_tmp
start_neulo_tmp2$name = row.names(start_neulo_tmp2)
start_neulo_tmp2 = start_neulo_tmp2[,c("name","start_neulo_coefs","start_neulo_err")]
names(start_neulo_tmp2) = c("variable","coef","se")

start_neulo_tmp3<-start_neulo_tmp2[-1,]
start_neulo_tmp3$variable = as.factor(start_neulo_tmp3$variable)
start_neulo_tmp3$coef<-as.numeric(as.vector(start_neulo_tmp3$coef))
start_neulo_tmp3$se<-as.numeric(as.vector(start_neulo_tmp3$se))
start_neulo_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                             "Low (casualties)",
                             "Long-term interest","High (interest)",
                             "High (reputation)",
                             "Evenly Split", "Support", 
                             "Sliding into recession", "Recovering from recession","In good shape")



start_neulo_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_neulo_variables)[1]){
        if (sum(start_neulo_tmp3$variable==start_neulo_variables$variable[i], na.rm=T)>0){
                start_neulo_variables$coef[i]<-start_neulo_tmp3$coef[start_neulo_tmp3$variable==start_neulo_variables$variable[i]]
                start_neulo_variables$se[i]<-start_neulo_tmp3$se[start_neulo_tmp3$variable==start_neulo_variables$variable[i]]
        }
}

start_neulo_variables2 = start_neulo_variables
start_neulo_variables2$coef[is.na(start_neulo_variables2$coef)]<-""
start_neulo_variables2$se[is.na(start_neulo_variables2$se)]<-""
start_neulo_variables2$variable<-as.vector(start_neulo_variables2$variable)
start_neulo_variables2$coef<-as.numeric(as.vector(start_neulo_variables2$coef))
start_neulo_variables2$se<-as.numeric(as.vector(start_neulo_variables2$se))

order<-1:dim(start_neulo_variables2)[1]
start_neulo_variables2 <- transform(start_neulo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
start_neu <- rbind(start_neuhi_variables2,start_neulo_variables2)

start_neu$id<-1:dim(start_neu)[1]

start_neu$dis<-NA
start_neu$dis[start_neu$id %in% c(1:23)] <- "High"
start_neu$dis[start_neu$id %in% c(24:46)] <- "Low"
start_neu$dis[start_neu$id %in% c(2,6,9,13,16,20,25,29,32,36,39,43)] <- "Baseline"

#### Plotting 


title<-paste("Intervention-Neuroticism")

start_neu_p<- ggplot(data = start_neu, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_neu_p        

ggsave(start_neu_p, file = "weighted_start_neu_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")




################### Start a War #####################
##################### Openness ###################
#source("startawar_openness.R")

## Openness -High
nrow(start_stack[start_stack$opennessHi==1,]) #n=1908; n=4974

start_openhi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                      data=start_stack[start_stack$opennessHi==1,],weight=disWts))

start_openhi_vars=variable.names(start_openhi_mod)
start_openhi_coefs = c(summary(start_openhi_mod)$coef[,1])
start_openhi_err = c(summary(start_openhi_mod)$coef[,2]*1.96)
start_openhi_tvals = c(summary(start_openhi_mod)$coef[,4])
start_openhi_tmp = data.frame(cbind(start_openhi_coefs,start_openhi_err))

## select particular coefficients
start_openhi_tmp2=start_openhi_tmp
start_openhi_tmp2$name = row.names(start_openhi_tmp2)
start_openhi_tmp2 = start_openhi_tmp2[,c("name","start_openhi_coefs","start_openhi_err")]
names(start_openhi_tmp2) = c("variable","coef","se")

start_openhi_tmp3<-start_openhi_tmp2[-1,]
start_openhi_tmp3$variable = as.factor(start_openhi_tmp3$variable)
start_openhi_tmp3$coef<-as.numeric(as.vector(start_openhi_tmp3$coef))
start_openhi_tmp3$se<-as.numeric(as.vector(start_openhi_tmp3$se))
start_openhi_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                              "Low (casualties)",
                              "Long-term interest","High (interest)",
                              "High (reputation)",
                              "Evenly Split", "Support", 
                              "Sliding into recession", "Recovering from recession","In good shape")



start_openhi_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_openhi_variables)[1]){
        if (sum(start_openhi_tmp3$variable==start_openhi_variables$variable[i], na.rm=T)>0){
                start_openhi_variables$coef[i]<-start_openhi_tmp3$coef[start_openhi_tmp3$variable==start_openhi_variables$variable[i]]
                start_openhi_variables$se[i]<-start_openhi_tmp3$se[start_openhi_tmp3$variable==start_openhi_variables$variable[i]]
        }
}

start_openhi_variables2 = start_openhi_variables
start_openhi_variables2$coef[is.na(start_openhi_variables2$coef)]<-""
start_openhi_variables2$se[is.na(start_openhi_variables2$se)]<-""
start_openhi_variables2$variable<-as.vector(start_openhi_variables2$variable)
start_openhi_variables2$coef<-as.numeric(as.vector(start_openhi_variables2$coef))
start_openhi_variables2$se<-as.numeric(as.vector(start_openhi_variables2$se))

order<-1:dim(start_openhi_variables2)[1]
start_openhi_variables2 <- transform(start_openhi_variables2, variable2=reorder(variable2, -order) ) 


## Openness - Low 
nrow(start_stack[start_stack$opennessLo==1,]) # n=78; too small; n=972

start_openlo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                      data=start_stack[start_stack$opennessLo==1,],weight=disWts))

start_openlo_vars=variable.names(start_openlo_mod)
start_openlo_coefs = c(summary(start_openlo_mod)$coef[,1])
start_openlo_err = c(summary(start_openlo_mod)$coef[,2]*1.96)
start_openlo_tvals = c(summary(start_openlo_mod)$coef[,4])
start_openlo_tmp = data.frame(cbind(start_openlo_coefs,start_openlo_err))

## select particular coefficients
start_openlo_tmp2=start_openlo_tmp
start_openlo_tmp2$name = row.names(start_openlo_tmp2)
start_openlo_tmp2 = start_openlo_tmp2[,c("name","start_openlo_coefs","start_openlo_err")]
names(start_openlo_tmp2) = c("variable","coef","se")

start_openlo_tmp3<-start_openlo_tmp2[-1,]
start_openlo_tmp3$variable = as.factor(start_openlo_tmp3$variable)
start_openlo_tmp3$coef<-as.numeric(as.vector(start_openlo_tmp3$coef))
start_openlo_tmp3$se<-as.numeric(as.vector(start_openlo_tmp3$se))
start_openlo_tmp3$variable<-c("As strong as the U.S.","Weaker than the U.S.",
                              "Low (casualties)",
                              "Long-term interest","High (interest)",
                              "High (reputation)",
                              "Evenly Split", "Support", 
                              "Sliding into recession", "Recovering from recession","In good shape")



start_openlo_variables=read.csv("start_variables_dis.csv")
for (i in 1:dim(start_openlo_variables)[1]){
        if (sum(start_openlo_tmp3$variable==start_openlo_variables$variable[i], na.rm=T)>0){
                start_openlo_variables$coef[i]<-start_openlo_tmp3$coef[start_openlo_tmp3$variable==start_openlo_variables$variable[i]]
                start_openlo_variables$se[i]<-start_openlo_tmp3$se[start_openlo_tmp3$variable==start_openlo_variables$variable[i]]
        }
}

start_openlo_variables2 = start_openlo_variables
start_openlo_variables2$coef[is.na(start_openlo_variables2$coef)]<-""
start_openlo_variables2$se[is.na(start_openlo_variables2$se)]<-""
start_openlo_variables2$variable<-as.vector(start_openlo_variables2$variable)
start_openlo_variables2$coef<-as.numeric(as.vector(start_openlo_variables2$coef))
start_openlo_variables2$se<-as.numeric(as.vector(start_openlo_variables2$se))

order<-1:dim(start_openlo_variables2)[1]
start_openlo_variables2 <- transform(start_openlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
start_open <- rbind(start_openhi_variables2,start_openlo_variables2)

start_open$id<-1:dim(start_open)[1]

start_open$dis<-NA
start_open$dis[start_open$id %in% c(1:23)] <- "High"
start_open$dis[start_open$id %in% c(24:46)] <- "Low"
start_open$dis[start_open$id %in% c(2,6,9,13,16,20,25,29,32,36,39,43)] <- "Baseline"

#### Plotting 

title<-paste("Intervention-Openness")

start_open_p<-ggplot(data = start_open, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.4)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_open_p        # n=opennessLo was only 78

ggsave(start_open_p, file = "weighted_start_open_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")





####################################
### Dispostional Effects ##########
###################################

#################### End A War #####################
##################### Extraversion ###################
#source("endawar_extraversion.R")

## Extraversion -High
nrow(end_stack[end_stack$extraversionHi==1,]) #n=2676

end_exthi_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$extraversionHi==1,],weight=disWts))

end_exthi_vars=variable.names(end_exthi_mod)
end_exthi_coefs = c(summary(end_exthi_mod)$coef[,1])
end_exthi_err = c(summary(end_exthi_mod)$coef[,2]*1.96)
end_exthi_tvals = c(summary(end_exthi_mod)$coef[,4])
end_exthi_tmp = data.frame(cbind(end_exthi_coefs,end_exthi_err))

## select particular coefficients
end_exthi_tmp2=end_exthi_tmp
end_exthi_tmp2$name = row.names(end_exthi_tmp2)
end_exthi_tmp2 = end_exthi_tmp2[,c("name","end_exthi_coefs","end_exthi_err")]
names(end_exthi_tmp2) = c("variable","coef","se")

end_exthi_tmp3<-end_exthi_tmp2[-1,]
end_exthi_tmp3$variable = as.factor(end_exthi_tmp3$variable)
end_exthi_tmp3$coef<-as.numeric(as.vector(end_exthi_tmp3$coef))
end_exthi_tmp3$se<-as.numeric(as.vector(end_exthi_tmp3$se))
end_exthi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_exthi_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_exthi_variables)[1]){
        if (sum(end_exthi_tmp3$variable==end_exthi_variables$variable[i], na.rm=T)>0){
                end_exthi_variables$coef[i]<-end_exthi_tmp3$coef[end_exthi_tmp3$variable==end_exthi_variables$variable[i]]
                end_exthi_variables$se[i]<-end_exthi_tmp3$se[end_exthi_tmp3$variable==end_exthi_variables$variable[i]]
        }
}

end_exthi_variables2 = end_exthi_variables
end_exthi_variables2$coef[is.na(end_exthi_variables2$coef)]<-""
end_exthi_variables2$se[is.na(end_exthi_variables2$se)]<-""
end_exthi_variables2$variable<-as.vector(end_exthi_variables2$variable)
end_exthi_variables2$coef<-as.numeric(as.vector(end_exthi_variables2$coef))
end_exthi_variables2$se<-as.numeric(as.vector(end_exthi_variables2$se))

order<-1:dim(end_exthi_variables2)[1]
end_exthi_variables2 <- transform(end_exthi_variables2, variable2=reorder(variable2, -order) ) 


## Extraversion - Low 
nrow(end_stack[end_stack$extraversionLo==1,]) # n=3270

end_extlo_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$extraversionLo==1,],weight=disWts))

end_extlo_vars=variable.names(end_extlo_mod)
end_extlo_coefs = c(summary(end_extlo_mod)$coef[,1])
end_extlo_err = c(summary(end_extlo_mod)$coef[,2]*1.96)
end_extlo_tvals = c(summary(end_extlo_mod)$coef[,4])
end_extlo_tmp = data.frame(cbind(end_extlo_coefs,end_extlo_err))

## select particular coefficients
end_extlo_tmp2=end_extlo_tmp
end_extlo_tmp2$name = row.names(end_extlo_tmp2)
end_extlo_tmp2 = end_extlo_tmp2[,c("name","end_extlo_coefs","end_extlo_err")]
names(end_extlo_tmp2) = c("variable","coef","se")

end_extlo_tmp3<-end_extlo_tmp2[-1,]
end_extlo_tmp3$variable = as.factor(end_extlo_tmp3$variable)
end_extlo_tmp3$coef<-as.numeric(as.vector(end_extlo_tmp3$coef))
end_extlo_tmp3$se<-as.numeric(as.vector(end_extlo_tmp3$se))
end_extlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_extlo_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_extlo_variables)[1]){
        if (sum(end_extlo_tmp3$variable==end_extlo_variables$variable[i], na.rm=T)>0){
                end_extlo_variables$coef[i]<-end_extlo_tmp3$coef[end_extlo_tmp3$variable==end_extlo_variables$variable[i]]
                end_extlo_variables$se[i]<-end_extlo_tmp3$se[end_extlo_tmp3$variable==end_extlo_variables$variable[i]]
        }
}

end_extlo_variables2 = end_extlo_variables
end_extlo_variables2$coef[is.na(end_extlo_variables2$coef)]<-""
end_extlo_variables2$se[is.na(end_extlo_variables2$se)]<-""
end_extlo_variables2$variable<-as.vector(end_extlo_variables2$variable)
end_extlo_variables2$coef<-as.numeric(as.vector(end_extlo_variables2$coef))
end_extlo_variables2$se<-as.numeric(as.vector(end_extlo_variables2$se))

order<-1:dim(end_extlo_variables2)[1]
end_extlo_variables2 <- transform(end_extlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
end_ext <- rbind(end_exthi_variables2,end_extlo_variables2)

end_ext$id<-1:dim(end_ext)[1]

end_ext$dis<-NA
end_ext$dis[end_ext$id %in% c(1:20)] <- "High"
end_ext$dis[end_ext$id %in% c(21:40)] <- "Low"
end_ext$dis[end_ext$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"


#### Plotting 

title<-paste("Backing Down-Extraversion")

end_ext_p = ggplot(data = end_ext, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_ext_p     


ggsave(end_ext_p , file = "weighted_end_ext_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")



####################################
### Dispostional Effects ##########
###################################

#################### End A War #####################
##################### Agreeableness ###################
#source("endawar_agreeableness.R")
## Agreeableness -High
nrow(end_stack[end_stack$agreeablenessHi==1,]) #n=5046

end_agrhi_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$agreeablenessHi==1,],weight=disWts))

end_agrhi_vars=variable.names(end_agrhi_mod)
end_agrhi_coefs = c(summary(end_agrhi_mod)$coef[,1])
end_agrhi_err = c(summary(end_agrhi_mod)$coef[,2]*1.96)
end_agrhi_tvals = c(summary(end_agrhi_mod)$coef[,4])
end_agrhi_tmp = data.frame(cbind(end_agrhi_coefs,end_agrhi_err))

## select particular coefficients
end_agrhi_tmp2=end_agrhi_tmp
end_agrhi_tmp2$name = row.names(end_agrhi_tmp2)
end_agrhi_tmp2 = end_agrhi_tmp2[,c("name","end_agrhi_coefs","end_agrhi_err")]
names(end_agrhi_tmp2) = c("variable","coef","se")

end_agrhi_tmp3<-end_agrhi_tmp2[-1,]
end_agrhi_tmp3$variable = as.factor(end_agrhi_tmp3$variable)
end_agrhi_tmp3$coef<-as.numeric(as.vector(end_agrhi_tmp3$coef))
end_agrhi_tmp3$se<-as.numeric(as.vector(end_agrhi_tmp3$se))
end_agrhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_agrhi_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_agrhi_variables)[1]){
        if (sum(end_agrhi_tmp3$variable==end_agrhi_variables$variable[i], na.rm=T)>0){
                end_agrhi_variables$coef[i]<-end_agrhi_tmp3$coef[end_agrhi_tmp3$variable==end_agrhi_variables$variable[i]]
                end_agrhi_variables$se[i]<-end_agrhi_tmp3$se[end_agrhi_tmp3$variable==end_agrhi_variables$variable[i]]
        }
}

end_agrhi_variables2 = end_agrhi_variables
end_agrhi_variables2$coef[is.na(end_agrhi_variables2$coef)]<-""
end_agrhi_variables2$se[is.na(end_agrhi_variables2$se)]<-""
end_agrhi_variables2$variable<-as.vector(end_agrhi_variables2$variable)
end_agrhi_variables2$coef<-as.numeric(as.vector(end_agrhi_variables2$coef))
end_agrhi_variables2$se<-as.numeric(as.vector(end_agrhi_variables2$se))

order<-1:dim(end_agrhi_variables2)[1]
end_agrhi_variables2 <- transform(end_agrhi_variables2, variable2=reorder(variable2, -order) ) 


## Agreeableness - Low 
nrow(end_stack[end_stack$agreeablenessLo==1,]) # n=900

end_agrlo_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$agreeablenessLo==1,],weight=disWts))

end_agrlo_vars=variable.names(end_agrlo_mod)
end_agrlo_coefs = c(summary(end_agrlo_mod)$coef[,1])
end_agrlo_err = c(summary(end_agrlo_mod)$coef[,2]*1.96)
end_agrlo_tvals = c(summary(end_agrlo_mod)$coef[,4])
end_agrlo_tmp = data.frame(cbind(end_agrlo_coefs,end_agrlo_err))

## select particular coefficients
end_agrlo_tmp2=end_agrlo_tmp
end_agrlo_tmp2$name = row.names(end_agrlo_tmp2)
end_agrlo_tmp2 = end_agrlo_tmp2[,c("name","end_agrlo_coefs","end_agrlo_err")]
names(end_agrlo_tmp2) = c("variable","coef","se")

end_agrlo_tmp3<-end_agrlo_tmp2[-1,]
end_agrlo_tmp3$variable = as.factor(end_agrlo_tmp3$variable)
end_agrlo_tmp3$coef<-as.numeric(as.vector(end_agrlo_tmp3$coef))
end_agrlo_tmp3$se<-as.numeric(as.vector(end_agrlo_tmp3$se))
end_agrlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_agrlo_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_agrlo_variables)[1]){
        if (sum(end_agrlo_tmp3$variable==end_agrlo_variables$variable[i], na.rm=T)>0){
                end_agrlo_variables$coef[i]<-end_agrlo_tmp3$coef[end_agrlo_tmp3$variable==end_agrlo_variables$variable[i]]
                end_agrlo_variables$se[i]<-end_agrlo_tmp3$se[end_agrlo_tmp3$variable==end_agrlo_variables$variable[i]]
        }
}

end_agrlo_variables2 = end_agrlo_variables
end_agrlo_variables2$coef[is.na(end_agrlo_variables2$coef)]<-""
end_agrlo_variables2$se[is.na(end_agrlo_variables2$se)]<-""
end_agrlo_variables2$variable<-as.vector(end_agrlo_variables2$variable)
end_agrlo_variables2$coef<-as.numeric(as.vector(end_agrlo_variables2$coef))
end_agrlo_variables2$se<-as.numeric(as.vector(end_agrlo_variables2$se))

order<-1:dim(end_agrlo_variables2)[1]
end_agrlo_variables2 <- transform(end_agrlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
end_agr <- rbind(end_agrhi_variables2,end_agrlo_variables2)

end_agr$id<-1:dim(end_agr)[1]

end_agr$dis<-NA
end_agr$dis[end_agr$id %in% c(1:20)] <- "High"
end_agr$dis[end_agr$id %in% c(21:40)] <- "Low"
end_agr$dis[end_agr$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"


#### Plotting 

title<-paste("Backing Down- Agreeableness")

end_agr_p = ggplot(data = end_agr, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_agr_p   

ggsave(end_agr_p , file = "weighted_end_agr_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")


####################################
### Dispostional Effects ##########
###################################

#################### End A War #####################
##################### Conscientiousness ###################
#source("endawar_conscientiousness.R")
## Conscientiousness -High
nrow(end_stack[end_stack$conscientiousnessHi==1,]) #n=5370

end_conhi_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$conscientiousnessHi==1,],weight=disWts))

end_conhi_vars=variable.names(end_conhi_mod)
end_conhi_coefs = c(summary(end_conhi_mod)$coef[,1])
end_conhi_err = c(summary(end_conhi_mod)$coef[,2]*1.96)
end_conhi_tvals = c(summary(end_conhi_mod)$coef[,4])
end_conhi_tmp = data.frame(cbind(end_conhi_coefs,end_conhi_err))

## select particular coefficients
end_conhi_tmp2=end_conhi_tmp
end_conhi_tmp2$name = row.names(end_conhi_tmp2)
end_conhi_tmp2 = end_conhi_tmp2[,c("name","end_conhi_coefs","end_conhi_err")]
names(end_conhi_tmp2) = c("variable","coef","se")

end_conhi_tmp3<-end_conhi_tmp2[-1,]
end_conhi_tmp3$variable = as.factor(end_conhi_tmp3$variable)
end_conhi_tmp3$coef<-as.numeric(as.vector(end_conhi_tmp3$coef))
end_conhi_tmp3$se<-as.numeric(as.vector(end_conhi_tmp3$se))
end_conhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_conhi_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_conhi_variables)[1]){
        if (sum(end_conhi_tmp3$variable==end_conhi_variables$variable[i], na.rm=T)>0){
                end_conhi_variables$coef[i]<-end_conhi_tmp3$coef[end_conhi_tmp3$variable==end_conhi_variables$variable[i]]
                end_conhi_variables$se[i]<-end_conhi_tmp3$se[end_conhi_tmp3$variable==end_conhi_variables$variable[i]]
        }
}

end_conhi_variables2 = end_conhi_variables
end_conhi_variables2$coef[is.na(end_conhi_variables2$coef)]<-""
end_conhi_variables2$se[is.na(end_conhi_variables2$se)]<-""
end_conhi_variables2$variable<-as.vector(end_conhi_variables2$variable)
end_conhi_variables2$coef<-as.numeric(as.vector(end_conhi_variables2$coef))
end_conhi_variables2$se<-as.numeric(as.vector(end_conhi_variables2$se))

order<-1:dim(end_conhi_variables2)[1]
end_conhi_variables2 <- transform(end_conhi_variables2, variable2=reorder(variable2, -order) ) 


## Conscientiousness - Low 
nrow(end_stack[end_stack$conscientiousnessLo==1,]) # n=576

end_conlo_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$conscientiousnessLo==1,],weight=disWts))

end_conlo_vars=variable.names(end_conlo_mod)
end_conlo_coefs = c(summary(end_conlo_mod)$coef[,1])
end_conlo_err = c(summary(end_conlo_mod)$coef[,2]*1.96)
end_conlo_tvals = c(summary(end_conlo_mod)$coef[,4])
end_conlo_tmp = data.frame(cbind(end_conlo_coefs,end_conlo_err))

## select particular coefficients
end_conlo_tmp2=end_conlo_tmp
end_conlo_tmp2$name = row.names(end_conlo_tmp2)
end_conlo_tmp2 = end_conlo_tmp2[,c("name","end_conlo_coefs","end_conlo_err")]
names(end_conlo_tmp2) = c("variable","coef","se")

end_conlo_tmp3<-end_conlo_tmp2[-1,]
end_conlo_tmp3$variable = as.factor(end_conlo_tmp3$variable)
end_conlo_tmp3$coef<-as.numeric(as.vector(end_conlo_tmp3$coef))
end_conlo_tmp3$se<-as.numeric(as.vector(end_conlo_tmp3$se))
end_conlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_conlo_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_conlo_variables)[1]){
        if (sum(end_conlo_tmp3$variable==end_conlo_variables$variable[i], na.rm=T)>0){
                end_conlo_variables$coef[i]<-end_conlo_tmp3$coef[end_conlo_tmp3$variable==end_conlo_variables$variable[i]]
                end_conlo_variables$se[i]<-end_conlo_tmp3$se[end_conlo_tmp3$variable==end_conlo_variables$variable[i]]
        }
}

end_conlo_variables2 = end_conlo_variables
end_conlo_variables2$coef[is.na(end_conlo_variables2$coef)]<-""
end_conlo_variables2$se[is.na(end_conlo_variables2$se)]<-""
end_conlo_variables2$variable<-as.vector(end_conlo_variables2$variable)
end_conlo_variables2$coef<-as.numeric(as.vector(end_conlo_variables2$coef))
end_conlo_variables2$se<-as.numeric(as.vector(end_conlo_variables2$se))

order<-1:dim(end_conlo_variables2)[1]
end_conlo_variables2 <- transform(end_conlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
end_con <- rbind(end_conhi_variables2,end_conlo_variables2)

end_con$id<-1:dim(end_con)[1]

end_con$dis<-NA
end_con$dis[end_con$id %in% c(1:20)] <- "High"
end_con$dis[end_con$id %in% c(21:40)] <- "Low"
end_con$dis[end_con$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Backing Down - Conscientiousness")

end_con_p = ggplot(data = end_con, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_con_p        

ggsave(end_con_p , file = "weighted_end_con_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")



####################################
### Dispostional Effects ##########
###################################

#################### End A War #####################
##################### Neuroticism ###################
#source("endawar_neuroticism.R")
## Neuroticism -High
nrow(end_stack[end_stack$neuroticismHi==1,]) #n=1968

end_neuhi_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$neuroticismHi==1,],weight=disWts))

end_neuhi_vars=variable.names(end_neuhi_mod)
end_neuhi_coefs = c(summary(end_neuhi_mod)$coef[,1])
end_neuhi_err = c(summary(end_neuhi_mod)$coef[,2]*1.96)
end_neuhi_tvals = c(summary(end_neuhi_mod)$coef[,4])
end_neuhi_tmp = data.frame(cbind(end_neuhi_coefs,end_neuhi_err))

## select particular coefficients
end_neuhi_tmp2=end_neuhi_tmp
end_neuhi_tmp2$name = row.names(end_neuhi_tmp2)
end_neuhi_tmp2 = end_neuhi_tmp2[,c("name","end_neuhi_coefs","end_neuhi_err")]
names(end_neuhi_tmp2) = c("variable","coef","se")

end_neuhi_tmp3<-end_neuhi_tmp2[-1,]
end_neuhi_tmp3$variable = as.factor(end_neuhi_tmp3$variable)
end_neuhi_tmp3$coef<-as.numeric(as.vector(end_neuhi_tmp3$coef))
end_neuhi_tmp3$se<-as.numeric(as.vector(end_neuhi_tmp3$se))
end_neuhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_neuhi_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_neuhi_variables)[1]){
        if (sum(end_neuhi_tmp3$variable==end_neuhi_variables$variable[i], na.rm=T)>0){
                end_neuhi_variables$coef[i]<-end_neuhi_tmp3$coef[end_neuhi_tmp3$variable==end_neuhi_variables$variable[i]]
                end_neuhi_variables$se[i]<-end_neuhi_tmp3$se[end_neuhi_tmp3$variable==end_neuhi_variables$variable[i]]
        }
}

end_neuhi_variables2 = end_neuhi_variables
end_neuhi_variables2$coef[is.na(end_neuhi_variables2$coef)]<-""
end_neuhi_variables2$se[is.na(end_neuhi_variables2$se)]<-""
end_neuhi_variables2$variable<-as.vector(end_neuhi_variables2$variable)
end_neuhi_variables2$coef<-as.numeric(as.vector(end_neuhi_variables2$coef))
end_neuhi_variables2$se<-as.numeric(as.vector(end_neuhi_variables2$se))

order<-1:dim(end_neuhi_variables2)[1]
end_neuhi_variables2 <- transform(end_neuhi_variables2, variable2=reorder(variable2, -order) ) 


## Neuroticism - Low 
nrow(end_stack[end_stack$neuroticismLo==1,]) # n=3978

end_neulo_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                   data=end_stack[end_stack$neuroticismLo==1,],weight=disWts))

end_neulo_vars=variable.names(end_neulo_mod)
end_neulo_coefs = c(summary(end_neulo_mod)$coef[,1])
end_neulo_err = c(summary(end_neulo_mod)$coef[,2]*1.96)
end_neulo_tvals = c(summary(end_neulo_mod)$coef[,4])
end_neulo_tmp = data.frame(cbind(end_neulo_coefs,end_neulo_err))

## select particular coefficients
end_neulo_tmp2=end_neulo_tmp
end_neulo_tmp2$name = row.names(end_neulo_tmp2)
end_neulo_tmp2 = end_neulo_tmp2[,c("name","end_neulo_coefs","end_neulo_err")]
names(end_neulo_tmp2) = c("variable","coef","se")

end_neulo_tmp3<-end_neulo_tmp2[-1,]
end_neulo_tmp3$variable = as.factor(end_neulo_tmp3$variable)
end_neulo_tmp3$coef<-as.numeric(as.vector(end_neulo_tmp3$coef))
end_neulo_tmp3$se<-as.numeric(as.vector(end_neulo_tmp3$se))
end_neulo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                           "Little (personal)","Evenly Split","Against","Little (reputation)")


end_neulo_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_neulo_variables)[1]){
        if (sum(end_neulo_tmp3$variable==end_neulo_variables$variable[i], na.rm=T)>0){
                end_neulo_variables$coef[i]<-end_neulo_tmp3$coef[end_neulo_tmp3$variable==end_neulo_variables$variable[i]]
                end_neulo_variables$se[i]<-end_neulo_tmp3$se[end_neulo_tmp3$variable==end_neulo_variables$variable[i]]
        }
}

end_neulo_variables2 = end_neulo_variables
end_neulo_variables2$coef[is.na(end_neulo_variables2$coef)]<-""
end_neulo_variables2$se[is.na(end_neulo_variables2$se)]<-""
end_neulo_variables2$variable<-as.vector(end_neulo_variables2$variable)
end_neulo_variables2$coef<-as.numeric(as.vector(end_neulo_variables2$coef))
end_neulo_variables2$se<-as.numeric(as.vector(end_neulo_variables2$se))

order<-1:dim(end_neulo_variables2)[1]
end_neulo_variables2 <- transform(end_neulo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
end_neu <- rbind(end_neuhi_variables2,end_neulo_variables2)

end_neu$id<-1:dim(end_neu)[1]

end_neu$dis<-NA
end_neu$dis[end_neu$id %in% c(1:20)] <- "High"
end_neu$dis[end_neu$id %in% c(21:40)] <- "Low"
end_neu$dis[end_neu$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"


#### Plotting 

title<-paste("Backing Down - Neuroticism")

end_neu_p = ggplot(data = end_neu, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_neu_p        
ggsave(end_neu_p , file = "weighted_end_neu_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")



####################################
### Dispostional Effects ##########
###################################

#################### End A War #####################
##################### Openness ###################
#source("endawar_openness.R")
## Openness -High
nrow(end_stack[end_stack$opennessHi==1,]) #n=4974

end_openhi_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                    data=end_stack[end_stack$opennessHi==1,],weight=disWts))

end_openhi_vars=variable.names(end_openhi_mod)
end_openhi_coefs = c(summary(end_openhi_mod)$coef[,1])
end_openhi_err = c(summary(end_openhi_mod)$coef[,2]*1.96)
end_openhi_tvals = c(summary(end_openhi_mod)$coef[,4])
end_openhi_tmp = data.frame(cbind(end_openhi_coefs,end_openhi_err))

## select particular coefficients
end_openhi_tmp2=end_openhi_tmp
end_openhi_tmp2$name = row.names(end_openhi_tmp2)
end_openhi_tmp2 = end_openhi_tmp2[,c("name","end_openhi_coefs","end_openhi_err")]
names(end_openhi_tmp2) = c("variable","coef","se")

end_openhi_tmp3<-end_openhi_tmp2[-1,]
end_openhi_tmp3$variable = as.factor(end_openhi_tmp3$variable)
end_openhi_tmp3$coef<-as.numeric(as.vector(end_openhi_tmp3$coef))
end_openhi_tmp3$se<-as.numeric(as.vector(end_openhi_tmp3$se))
end_openhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                            "Little (personal)","Evenly Split","Against","Little (reputation)")

end_openhi_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_openhi_variables)[1]){
        if (sum(end_openhi_tmp3$variable==end_openhi_variables$variable[i], na.rm=T)>0){
                end_openhi_variables$coef[i]<-end_openhi_tmp3$coef[end_openhi_tmp3$variable==end_openhi_variables$variable[i]]
                end_openhi_variables$se[i]<-end_openhi_tmp3$se[end_openhi_tmp3$variable==end_openhi_variables$variable[i]]
        }
}

end_openhi_variables2 = end_openhi_variables
end_openhi_variables2$coef[is.na(end_openhi_variables2$coef)]<-""
end_openhi_variables2$se[is.na(end_openhi_variables2$se)]<-""
end_openhi_variables2$variable<-as.vector(end_openhi_variables2$variable)
end_openhi_variables2$coef<-as.numeric(as.vector(end_openhi_variables2$coef))
end_openhi_variables2$se<-as.numeric(as.vector(end_openhi_variables2$se))

order<-1:dim(end_openhi_variables2)[1]
end_openhi_variables2 <- transform(end_openhi_variables2, variable2=reorder(variable2, -order) ) 


## Openness - Low 
nrow(end_stack[end_stack$opennessLo==1,]) # n=972

end_openlo_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                    data=end_stack[end_stack$opennessLo==1,],weight=disWts))

end_openlo_vars=variable.names(end_openlo_mod)
end_openlo_coefs = c(summary(end_openlo_mod)$coef[,1])
end_openlo_err = c(summary(end_openlo_mod)$coef[,2]*1.96)
end_openlo_tvals = c(summary(end_openlo_mod)$coef[,4])
end_openlo_tmp = data.frame(cbind(end_openlo_coefs,end_openlo_err))

## select particular coefficients
end_openlo_tmp2=end_openlo_tmp
end_openlo_tmp2$name = row.names(end_openlo_tmp2)
end_openlo_tmp2 = end_openlo_tmp2[,c("name","end_openlo_coefs","end_openlo_err")]
names(end_openlo_tmp2) = c("variable","coef","se")

end_openlo_tmp3<-end_openlo_tmp2[-1,]
end_openlo_tmp3$variable = as.factor(end_openlo_tmp3$variable)
end_openlo_tmp3$coef<-as.numeric(as.vector(end_openlo_tmp3$coef))
end_openlo_tmp3$se<-as.numeric(as.vector(end_openlo_tmp3$se))
end_openlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                            "Little (personal)","Evenly Split","Against","Little (reputation)")


end_openlo_variables=read.csv("end_variables_dis.csv")
for (i in 1:dim(end_openlo_variables)[1]){
        if (sum(end_openlo_tmp3$variable==end_openlo_variables$variable[i], na.rm=T)>0){
                end_openlo_variables$coef[i]<-end_openlo_tmp3$coef[end_openlo_tmp3$variable==end_openlo_variables$variable[i]]
                end_openlo_variables$se[i]<-end_openlo_tmp3$se[end_openlo_tmp3$variable==end_openlo_variables$variable[i]]
        }
}

end_openlo_variables2 = end_openlo_variables
end_openlo_variables2$coef[is.na(end_openlo_variables2$coef)]<-""
end_openlo_variables2$se[is.na(end_openlo_variables2$se)]<-""
end_openlo_variables2$variable<-as.vector(end_openlo_variables2$variable)
end_openlo_variables2$coef<-as.numeric(as.vector(end_openlo_variables2$coef))
end_openlo_variables2$se<-as.numeric(as.vector(end_openlo_variables2$se))

order<-1:dim(end_openlo_variables2)[1]
end_openlo_variables2 <- transform(end_openlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
end_open <- rbind(end_openhi_variables2,end_openlo_variables2)

end_open$id<-1:dim(end_open)[1]

end_open$dis<-NA
end_open$dis[end_open$id %in% c(1:20)] <- "High"
end_open$dis[end_open$id %in% c(21:40)] <- "Low"
end_open$dis[end_open$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Backing Down - Openness")

end_open_p = ggplot(data = end_open, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_open_p        

ggsave(end_open_p , file = "weighted_end_open_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")



####################################
### Dispostional Effects ##########
###################################

#################### Increase Commitment #####################
##################### Openness ###################
#source("increase_openness.R")
## Openness -High
nrow(end_stack[end_stack$opennessHi==1,]) #n=4974

increase_openhi_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                         data=end_stack[end_stack$opennessHi==1,],weight=disWts))

increase_openhi_vars=variable.names(increase_openhi_mod)
increase_openhi_coefs = c(summary(increase_openhi_mod)$coef[,1])
increase_openhi_err = c(summary(increase_openhi_mod)$coef[,2]*1.96)
increase_openhi_tvals = c(summary(increase_openhi_mod)$coef[,4])
increase_openhi_tmp = data.frame(cbind(increase_openhi_coefs,increase_openhi_err))

## select particular coefficients
increase_openhi_tmp2=increase_openhi_tmp
increase_openhi_tmp2$name = row.names(increase_openhi_tmp2)
increase_openhi_tmp2 = increase_openhi_tmp2[,c("name","increase_openhi_coefs","increase_openhi_err")]
names(increase_openhi_tmp2) = c("variable","coef","se")

increase_openhi_tmp3<-increase_openhi_tmp2[-1,]
increase_openhi_tmp3$variable = as.factor(increase_openhi_tmp3$variable)
increase_openhi_tmp3$coef<-as.numeric(as.vector(increase_openhi_tmp3$coef))
increase_openhi_tmp3$se<-as.numeric(as.vector(increase_openhi_tmp3$se))
increase_openhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                 "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_openhi_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_openhi_variables)[1]){
        if (sum(increase_openhi_tmp3$variable==increase_openhi_variables$variable[i], na.rm=T)>0){
                increase_openhi_variables$coef[i]<-increase_openhi_tmp3$coef[increase_openhi_tmp3$variable==increase_openhi_variables$variable[i]]
                increase_openhi_variables$se[i]<-increase_openhi_tmp3$se[increase_openhi_tmp3$variable==increase_openhi_variables$variable[i]]
        }
}

increase_openhi_variables2 = increase_openhi_variables
increase_openhi_variables2$coef[is.na(increase_openhi_variables2$coef)]<-""
increase_openhi_variables2$se[is.na(increase_openhi_variables2$se)]<-""
increase_openhi_variables2$variable<-as.vector(increase_openhi_variables2$variable)
increase_openhi_variables2$coef<-as.numeric(as.vector(increase_openhi_variables2$coef))
increase_openhi_variables2$se<-as.numeric(as.vector(increase_openhi_variables2$se))

order<-1:dim(increase_openhi_variables2)[1]
increase_openhi_variables2 <- transform(increase_openhi_variables2, variable2=reorder(variable2, -order) ) 


## Openness - Low 
nrow(end_stack[end_stack$opennessLo==1,]) # n=972

increase_openlo_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                         data=end_stack[end_stack$opennessLo==1,],weight=disWts))

increase_openlo_vars=variable.names(increase_openlo_mod)
increase_openlo_coefs = c(summary(increase_openlo_mod)$coef[,1])
increase_openlo_err = c(summary(increase_openlo_mod)$coef[,2]*1.96)
increase_openlo_tvals = c(summary(increase_openlo_mod)$coef[,4])
increase_openlo_tmp = data.frame(cbind(increase_openlo_coefs,increase_openlo_err))

## select particular coefficients
increase_openlo_tmp2=increase_openlo_tmp
increase_openlo_tmp2$name = row.names(increase_openlo_tmp2)
increase_openlo_tmp2 = increase_openlo_tmp2[,c("name","increase_openlo_coefs","increase_openlo_err")]
names(increase_openlo_tmp2) = c("variable","coef","se")

increase_openlo_tmp3<-increase_openlo_tmp2[-1,]
increase_openlo_tmp3$variable = as.factor(increase_openlo_tmp3$variable)
increase_openlo_tmp3$coef<-as.numeric(as.vector(increase_openlo_tmp3$coef))
increase_openlo_tmp3$se<-as.numeric(as.vector(increase_openlo_tmp3$se))
increase_openlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                 "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_openlo_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_openlo_variables)[1]){
        if (sum(increase_openlo_tmp3$variable==increase_openlo_variables$variable[i], na.rm=T)>0){
                increase_openlo_variables$coef[i]<-increase_openlo_tmp3$coef[increase_openlo_tmp3$variable==increase_openlo_variables$variable[i]]
                increase_openlo_variables$se[i]<-increase_openlo_tmp3$se[increase_openlo_tmp3$variable==increase_openlo_variables$variable[i]]
        }
}

increase_openlo_variables2 = increase_openlo_variables
increase_openlo_variables2$coef[is.na(increase_openlo_variables2$coef)]<-""
increase_openlo_variables2$se[is.na(increase_openlo_variables2$se)]<-""
increase_openlo_variables2$variable<-as.vector(increase_openlo_variables2$variable)
increase_openlo_variables2$coef<-as.numeric(as.vector(increase_openlo_variables2$coef))
increase_openlo_variables2$se<-as.numeric(as.vector(increase_openlo_variables2$se))

order<-1:dim(increase_openlo_variables2)[1]
increase_openlo_variables2 <- transform(increase_openlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
increase_open <- rbind(increase_openhi_variables2,increase_openlo_variables2)

increase_open$id<-1:dim(increase_open)[1]

increase_open$dis<-NA
increase_open$dis[increase_open$id %in% c(1:20)] <- "High"
increase_open$dis[increase_open$id %in% c(21:40)] <- "Low"
increase_open$dis[increase_open$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"


#### Plotting 

title<-paste("Increase Commitment - Openness")

increase_open_p = ggplot(data = increase_open, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.3,.2)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
increase_open_p        

ggsave(increase_open_p , file = "weighted_increase_open_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")


####################################
### Dispostional Effects ##########
###################################
#source("increase_extraversion.R")
#################### Increase Commitment #####################
##################### extraversion ###################

## extraversion -High
nrow(end_stack[end_stack$extraversionHi==1,]) #n=2676

increase_exthi_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$extraversionHi==1,],weight=disWts))

increase_exthi_vars=variable.names(increase_exthi_mod)
increase_exthi_coefs = c(summary(increase_exthi_mod)$coef[,1])
increase_exthi_err = c(summary(increase_exthi_mod)$coef[,2]*1.96)
increase_exthi_tvals = c(summary(increase_exthi_mod)$coef[,4])
increase_exthi_tmp = data.frame(cbind(increase_exthi_coefs,increase_exthi_err))

## select particular coefficients
increase_exthi_tmp2=increase_exthi_tmp
increase_exthi_tmp2$name = row.names(increase_exthi_tmp2)
increase_exthi_tmp2 = increase_exthi_tmp2[,c("name","increase_exthi_coefs","increase_exthi_err")]
names(increase_exthi_tmp2) = c("variable","coef","se")

increase_exthi_tmp3<-increase_exthi_tmp2[-1,]
increase_exthi_tmp3$variable = as.factor(increase_exthi_tmp3$variable)
increase_exthi_tmp3$coef<-as.numeric(as.vector(increase_exthi_tmp3$coef))
increase_exthi_tmp3$se<-as.numeric(as.vector(increase_exthi_tmp3$se))
increase_exthi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")

increase_exthi_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_exthi_variables)[1]){
        if (sum(increase_exthi_tmp3$variable==increase_exthi_variables$variable[i], na.rm=T)>0){
                increase_exthi_variables$coef[i]<-increase_exthi_tmp3$coef[increase_exthi_tmp3$variable==increase_exthi_variables$variable[i]]
                increase_exthi_variables$se[i]<-increase_exthi_tmp3$se[increase_exthi_tmp3$variable==increase_exthi_variables$variable[i]]
        }
}

increase_exthi_variables2 = increase_exthi_variables
increase_exthi_variables2$coef[is.na(increase_exthi_variables2$coef)]<-""
increase_exthi_variables2$se[is.na(increase_exthi_variables2$se)]<-""
increase_exthi_variables2$variable<-as.vector(increase_exthi_variables2$variable)
increase_exthi_variables2$coef<-as.numeric(as.vector(increase_exthi_variables2$coef))
increase_exthi_variables2$se<-as.numeric(as.vector(increase_exthi_variables2$se))

order<-1:dim(increase_exthi_variables2)[1]
increase_exthi_variables2 <- transform(increase_exthi_variables2, variable2=reorder(variable2, -order) ) 


## extraversion - Low 
nrow(end_stack[end_stack$extraversionLo==1,]) # n=3270

increase_extlo_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$extraversionLo==1,],weight=disWts))

increase_extlo_vars=variable.names(increase_extlo_mod)
increase_extlo_coefs = c(summary(increase_extlo_mod)$coef[,1])
increase_extlo_err = c(summary(increase_extlo_mod)$coef[,2]*1.96)
increase_extlo_tvals = c(summary(increase_extlo_mod)$coef[,4])
increase_extlo_tmp = data.frame(cbind(increase_extlo_coefs,increase_extlo_err))

## select particular coefficients
increase_extlo_tmp2=increase_extlo_tmp
increase_extlo_tmp2$name = row.names(increase_extlo_tmp2)
increase_extlo_tmp2 = increase_extlo_tmp2[,c("name","increase_extlo_coefs","increase_extlo_err")]
names(increase_extlo_tmp2) = c("variable","coef","se")

increase_extlo_tmp3<-increase_extlo_tmp2[-1,]
increase_extlo_tmp3$variable = as.factor(increase_extlo_tmp3$variable)
increase_extlo_tmp3$coef<-as.numeric(as.vector(increase_extlo_tmp3$coef))
increase_extlo_tmp3$se<-as.numeric(as.vector(increase_extlo_tmp3$se))
increase_extlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_extlo_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_extlo_variables)[1]){
        if (sum(increase_extlo_tmp3$variable==increase_extlo_variables$variable[i], na.rm=T)>0){
                increase_extlo_variables$coef[i]<-increase_extlo_tmp3$coef[increase_extlo_tmp3$variable==increase_extlo_variables$variable[i]]
                increase_extlo_variables$se[i]<-increase_extlo_tmp3$se[increase_extlo_tmp3$variable==increase_extlo_variables$variable[i]]
        }
}

increase_extlo_variables2 = increase_extlo_variables
increase_extlo_variables2$coef[is.na(increase_extlo_variables2$coef)]<-""
increase_extlo_variables2$se[is.na(increase_extlo_variables2$se)]<-""
increase_extlo_variables2$variable<-as.vector(increase_extlo_variables2$variable)
increase_extlo_variables2$coef<-as.numeric(as.vector(increase_extlo_variables2$coef))
increase_extlo_variables2$se<-as.numeric(as.vector(increase_extlo_variables2$se))

order<-1:dim(increase_extlo_variables2)[1]
increase_extlo_variables2 <- transform(increase_extlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
increase_ext <- rbind(increase_exthi_variables2,increase_extlo_variables2)

increase_ext$id<-1:dim(increase_ext)[1]

increase_ext$dis<-NA
increase_ext$dis[increase_ext$id %in% c(1:20)] <- "High"
increase_ext$dis[increase_ext$id %in% c(21:40)] <- "Low"
increase_ext$dis[increase_ext$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"


#### Plotting 

title<-paste("Increase Commitment - Extraversion")

increase_ext_p = ggplot(data = increase_ext, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.4,.2)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
increase_ext_p        

ggsave(increase_ext_p , file = "weighted_increase_ext_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")


####################################
### Dispostional Effects ##########
###################################

#################### Increase Commitment #####################
##################### agreeableness ###################
#source("increase_agreeableness.R")

## agreeableness -High
nrow(end_stack[end_stack$agreeablenessHi==1,]) #n=5046

increase_agrhi_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$agreeablenessHi==1,],weight=disWts))

increase_agrhi_vars=variable.names(increase_agrhi_mod)
increase_agrhi_coefs = c(summary(increase_agrhi_mod)$coef[,1])
increase_agrhi_err = c(summary(increase_agrhi_mod)$coef[,2]*1.96)
increase_agrhi_tvals = c(summary(increase_agrhi_mod)$coef[,4])
increase_agrhi_tmp = data.frame(cbind(increase_agrhi_coefs,increase_agrhi_err))

## select particular coefficients
increase_agrhi_tmp2=increase_agrhi_tmp
increase_agrhi_tmp2$name = row.names(increase_agrhi_tmp2)
increase_agrhi_tmp2 = increase_agrhi_tmp2[,c("name","increase_agrhi_coefs","increase_agrhi_err")]
names(increase_agrhi_tmp2) = c("variable","coef","se")

increase_agrhi_tmp3<-increase_agrhi_tmp2[-1,]
increase_agrhi_tmp3$variable = as.factor(increase_agrhi_tmp3$variable)
increase_agrhi_tmp3$coef<-as.numeric(as.vector(increase_agrhi_tmp3$coef))
increase_agrhi_tmp3$se<-as.numeric(as.vector(increase_agrhi_tmp3$se))
increase_agrhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_agrhi_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_agrhi_variables)[1]){
        if (sum(increase_agrhi_tmp3$variable==increase_agrhi_variables$variable[i], na.rm=T)>0){
                increase_agrhi_variables$coef[i]<-increase_agrhi_tmp3$coef[increase_agrhi_tmp3$variable==increase_agrhi_variables$variable[i]]
                increase_agrhi_variables$se[i]<-increase_agrhi_tmp3$se[increase_agrhi_tmp3$variable==increase_agrhi_variables$variable[i]]
        }
}

increase_agrhi_variables2 = increase_agrhi_variables
increase_agrhi_variables2$coef[is.na(increase_agrhi_variables2$coef)]<-""
increase_agrhi_variables2$se[is.na(increase_agrhi_variables2$se)]<-""
increase_agrhi_variables2$variable<-as.vector(increase_agrhi_variables2$variable)
increase_agrhi_variables2$coef<-as.numeric(as.vector(increase_agrhi_variables2$coef))
increase_agrhi_variables2$se<-as.numeric(as.vector(increase_agrhi_variables2$se))

order<-1:dim(increase_agrhi_variables2)[1]
increase_agrhi_variables2 <- transform(increase_agrhi_variables2, variable2=reorder(variable2, -order) ) 


## agreeableness - Low 
nrow(end_stack[end_stack$agreeablenessLo==1,]) # n=900

increase_agrlo_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$agreeablenessLo==1,],weight=disWts))

increase_agrlo_vars=variable.names(increase_agrlo_mod)
increase_agrlo_coefs = c(summary(increase_agrlo_mod)$coef[,1])
increase_agrlo_err = c(summary(increase_agrlo_mod)$coef[,2]*1.96)
increase_agrlo_tvals = c(summary(increase_agrlo_mod)$coef[,4])
increase_agrlo_tmp = data.frame(cbind(increase_agrlo_coefs,increase_agrlo_err))

## select particular coefficients
increase_agrlo_tmp2=increase_agrlo_tmp
increase_agrlo_tmp2$name = row.names(increase_agrlo_tmp2)
increase_agrlo_tmp2 = increase_agrlo_tmp2[,c("name","increase_agrlo_coefs","increase_agrlo_err")]
names(increase_agrlo_tmp2) = c("variable","coef","se")

increase_agrlo_tmp3<-increase_agrlo_tmp2[-1,]
increase_agrlo_tmp3$variable = as.factor(increase_agrlo_tmp3$variable)
increase_agrlo_tmp3$coef<-as.numeric(as.vector(increase_agrlo_tmp3$coef))
increase_agrlo_tmp3$se<-as.numeric(as.vector(increase_agrlo_tmp3$se))
increase_agrlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_agrlo_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_agrlo_variables)[1]){
        if (sum(increase_agrlo_tmp3$variable==increase_agrlo_variables$variable[i], na.rm=T)>0){
                increase_agrlo_variables$coef[i]<-increase_agrlo_tmp3$coef[increase_agrlo_tmp3$variable==increase_agrlo_variables$variable[i]]
                increase_agrlo_variables$se[i]<-increase_agrlo_tmp3$se[increase_agrlo_tmp3$variable==increase_agrlo_variables$variable[i]]
        }
}

increase_agrlo_variables2 = increase_agrlo_variables
increase_agrlo_variables2$coef[is.na(increase_agrlo_variables2$coef)]<-""
increase_agrlo_variables2$se[is.na(increase_agrlo_variables2$se)]<-""
increase_agrlo_variables2$variable<-as.vector(increase_agrlo_variables2$variable)
increase_agrlo_variables2$coef<-as.numeric(as.vector(increase_agrlo_variables2$coef))
increase_agrlo_variables2$se<-as.numeric(as.vector(increase_agrlo_variables2$se))

order<-1:dim(increase_agrlo_variables2)[1]
increase_agrlo_variables2 <- transform(increase_agrlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
increase_agr <- rbind(increase_agrhi_variables2,increase_agrlo_variables2)

increase_agr$id<-1:dim(increase_agr)[1]

increase_agr$dis<-NA
increase_agr$dis[increase_agr$id %in% c(1:20)] <- "High"
increase_agr$dis[increase_agr$id %in% c(21:40)] <- "Low"
increase_agr$dis[increase_agr$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Increase Commitment - agreeableness")

increase_agr_p = ggplot(data = increase_agr, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.3,.2)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
increase_agr_p        

ggsave(increase_agr_p , file = "weighted_increase_agr_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")


####################################
### Dispostional Effects ##########
###################################

#################### Increase Commitment #####################
##################### conscientiousness ###################
#source("increase_conscientiousness.R")

## conscientiousness -High
nrow(end_stack[end_stack$conscientiousnessHi==1,]) #n=5370

increase_conhi_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$conscientiousnessHi==1,],weight=disWts))

increase_conhi_vars=variable.names(increase_conhi_mod)
increase_conhi_coefs = c(summary(increase_conhi_mod)$coef[,1])
increase_conhi_err = c(summary(increase_conhi_mod)$coef[,2]*1.96)
increase_conhi_tvals = c(summary(increase_conhi_mod)$coef[,4])
increase_conhi_tmp = data.frame(cbind(increase_conhi_coefs,increase_conhi_err))

## select particular coefficients
increase_conhi_tmp2=increase_conhi_tmp
increase_conhi_tmp2$name = row.names(increase_conhi_tmp2)
increase_conhi_tmp2 = increase_conhi_tmp2[,c("name","increase_conhi_coefs","increase_conhi_err")]
names(increase_conhi_tmp2) = c("variable","coef","se")

increase_conhi_tmp3<-increase_conhi_tmp2[-1,]
increase_conhi_tmp3$variable = as.factor(increase_conhi_tmp3$variable)
increase_conhi_tmp3$coef<-as.numeric(as.vector(increase_conhi_tmp3$coef))
increase_conhi_tmp3$se<-as.numeric(as.vector(increase_conhi_tmp3$se))
increase_conhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_conhi_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_conhi_variables)[1]){
        if (sum(increase_conhi_tmp3$variable==increase_conhi_variables$variable[i], na.rm=T)>0){
                increase_conhi_variables$coef[i]<-increase_conhi_tmp3$coef[increase_conhi_tmp3$variable==increase_conhi_variables$variable[i]]
                increase_conhi_variables$se[i]<-increase_conhi_tmp3$se[increase_conhi_tmp3$variable==increase_conhi_variables$variable[i]]
        }
}

increase_conhi_variables2 = increase_conhi_variables
increase_conhi_variables2$coef[is.na(increase_conhi_variables2$coef)]<-""
increase_conhi_variables2$se[is.na(increase_conhi_variables2$se)]<-""
increase_conhi_variables2$variable<-as.vector(increase_conhi_variables2$variable)
increase_conhi_variables2$coef<-as.numeric(as.vector(increase_conhi_variables2$coef))
increase_conhi_variables2$se<-as.numeric(as.vector(increase_conhi_variables2$se))

order<-1:dim(increase_conhi_variables2)[1]
increase_conhi_variables2 <- transform(increase_conhi_variables2, variable2=reorder(variable2, -order) ) 


## conscientiousness - Low 
nrow(end_stack[end_stack$conscientiousnessLo==1,]) # n=576

increase_conlo_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$conscientiousnessLo==1,],weight=disWts))

increase_conlo_vars=variable.names(increase_conlo_mod)
increase_conlo_coefs = c(summary(increase_conlo_mod)$coef[,1])
increase_conlo_err = c(summary(increase_conlo_mod)$coef[,2]*1.96)
increase_conlo_tvals = c(summary(increase_conlo_mod)$coef[,4])
increase_conlo_tmp = data.frame(cbind(increase_conlo_coefs,increase_conlo_err))

## select particular coefficients
increase_conlo_tmp2=increase_conlo_tmp
increase_conlo_tmp2$name = row.names(increase_conlo_tmp2)
increase_conlo_tmp2 = increase_conlo_tmp2[,c("name","increase_conlo_coefs","increase_conlo_err")]
names(increase_conlo_tmp2) = c("variable","coef","se")

increase_conlo_tmp3<-increase_conlo_tmp2[-1,]
increase_conlo_tmp3$variable = as.factor(increase_conlo_tmp3$variable)
increase_conlo_tmp3$coef<-as.numeric(as.vector(increase_conlo_tmp3$coef))
increase_conlo_tmp3$se<-as.numeric(as.vector(increase_conlo_tmp3$se))
increase_conlo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_conlo_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_conlo_variables)[1]){
        if (sum(increase_conlo_tmp3$variable==increase_conlo_variables$variable[i], na.rm=T)>0){
                increase_conlo_variables$coef[i]<-increase_conlo_tmp3$coef[increase_conlo_tmp3$variable==increase_conlo_variables$variable[i]]
                increase_conlo_variables$se[i]<-increase_conlo_tmp3$se[increase_conlo_tmp3$variable==increase_conlo_variables$variable[i]]
        }
}

increase_conlo_variables2 = increase_conlo_variables
increase_conlo_variables2$coef[is.na(increase_conlo_variables2$coef)]<-""
increase_conlo_variables2$se[is.na(increase_conlo_variables2$se)]<-""
increase_conlo_variables2$variable<-as.vector(increase_conlo_variables2$variable)
increase_conlo_variables2$coef<-as.numeric(as.vector(increase_conlo_variables2$coef))
increase_conlo_variables2$se<-as.numeric(as.vector(increase_conlo_variables2$se))

order<-1:dim(increase_conlo_variables2)[1]
increase_conlo_variables2 <- transform(increase_conlo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
increase_con <- rbind(increase_conhi_variables2,increase_conlo_variables2)

increase_con$id<-1:dim(increase_con)[1]

increase_con$dis<-NA
increase_con$dis[increase_con$id %in% c(1:20)] <- "High"
increase_con$dis[increase_con$id %in% c(21:40)] <- "Low"
increase_con$dis[increase_agr$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Increase Commitment - Conscientiousness")

increase_con_p = ggplot(data = increase_con, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.3,.2)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
increase_con_p        

ggsave(increase_con_p , file ="weighted_increase_con_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")

###################################
### Dispostional Effects ##########
###################################

#################### Increase Commitment #####################
##################### neuroticism ###################
#source("increase_neuroticism.R")

## neuroticism -High
nrow(end_stack[end_stack$neuroticismHi==1,]) #n=1968

nrow(end_stack[end_stack$extraversionHi==1&end_stack$conscientiousnessHi==1,])

increase_neuhi_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$neuroticismHi==1,],weight=disWts))

increase_neuhi_vars=variable.names(increase_neuhi_mod)
increase_neuhi_coefs = c(summary(increase_neuhi_mod)$coef[,1])
increase_neuhi_err = c(summary(increase_neuhi_mod)$coef[,2]*1.96)
increase_neuhi_tvals = c(summary(increase_neuhi_mod)$coef[,4])
increase_neuhi_tmp = data.frame(cbind(increase_neuhi_coefs,increase_neuhi_err))

## select particular coefficients
increase_neuhi_tmp2=increase_neuhi_tmp
increase_neuhi_tmp2$name = row.names(increase_neuhi_tmp2)
increase_neuhi_tmp2 = increase_neuhi_tmp2[,c("name","increase_neuhi_coefs","increase_neuhi_err")]
names(increase_neuhi_tmp2) = c("variable","coef","se")

increase_neuhi_tmp3<-increase_neuhi_tmp2[-1,]
increase_neuhi_tmp3$variable = as.factor(increase_neuhi_tmp3$variable)
increase_neuhi_tmp3$coef<-as.numeric(as.vector(increase_neuhi_tmp3$coef))
increase_neuhi_tmp3$se<-as.numeric(as.vector(increase_neuhi_tmp3$se))
increase_neuhi_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_neuhi_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_neuhi_variables)[1]){
        if (sum(increase_neuhi_tmp3$variable==increase_neuhi_variables$variable[i], na.rm=T)>0){
                increase_neuhi_variables$coef[i]<-increase_neuhi_tmp3$coef[increase_neuhi_tmp3$variable==increase_neuhi_variables$variable[i]]
                increase_neuhi_variables$se[i]<-increase_neuhi_tmp3$se[increase_neuhi_tmp3$variable==increase_neuhi_variables$variable[i]]
        }
}

increase_neuhi_variables2 = increase_neuhi_variables
increase_neuhi_variables2$coef[is.na(increase_neuhi_variables2$coef)]<-""
increase_neuhi_variables2$se[is.na(increase_neuhi_variables2$se)]<-""
increase_neuhi_variables2$variable<-as.vector(increase_neuhi_variables2$variable)
increase_neuhi_variables2$coef<-as.numeric(as.vector(increase_neuhi_variables2$coef))
increase_neuhi_variables2$se<-as.numeric(as.vector(increase_neuhi_variables2$se))

order<-1:dim(increase_neuhi_variables2)[1]
increase_neuhi_variables2 <- transform(increase_neuhi_variables2, variable2=reorder(variable2, -order) ) 


## neuroticism - Low 
nrow(end_stack[end_stack$neuroticismLo==1,]) # n=3978

increase_neulo_mod<-(lm(increase_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
                        data=end_stack[end_stack$neuroticismLo==1,],weight=disWts))

increase_neulo_vars=variable.names(increase_neulo_mod)
increase_neulo_coefs = c(summary(increase_neulo_mod)$coef[,1])
increase_neulo_err = c(summary(increase_neulo_mod)$coef[,2]*1.96)
increase_neulo_tvals = c(summary(increase_neulo_mod)$coef[,4])
increase_neulo_tmp = data.frame(cbind(increase_neulo_coefs,increase_neulo_err))

## select particular coefficients
increase_neulo_tmp2=increase_neulo_tmp
increase_neulo_tmp2$name = row.names(increase_neulo_tmp2)
increase_neulo_tmp2 = increase_neulo_tmp2[,c("name","increase_neulo_coefs","increase_neulo_err")]
names(increase_neulo_tmp2) = c("variable","coef","se")

increase_neulo_tmp3<-increase_neulo_tmp2[-1,]
increase_neulo_tmp3$variable = as.factor(increase_neulo_tmp3$variable)
increase_neulo_tmp3$coef<-as.numeric(as.vector(increase_neulo_tmp3$coef))
increase_neulo_tmp3$se<-as.numeric(as.vector(increase_neulo_tmp3$se))
increase_neulo_tmp3$variable<-c("High (spending)","High (casualties)","Long-term interest", "Little (interest)",
                                "Little (personal)","Evenly Split","Against","Little (reputation)")


increase_neulo_variables=read.csv("increase_variables_dis.csv")
for (i in 1:dim(increase_neulo_variables)[1]){
        if (sum(increase_neulo_tmp3$variable==increase_neulo_variables$variable[i], na.rm=T)>0){
                increase_neulo_variables$coef[i]<-increase_neulo_tmp3$coef[increase_neulo_tmp3$variable==increase_neulo_variables$variable[i]]
                increase_neulo_variables$se[i]<-increase_neulo_tmp3$se[increase_neulo_tmp3$variable==increase_neulo_variables$variable[i]]
        }
}

increase_neulo_variables2 = increase_neulo_variables
increase_neulo_variables2$coef[is.na(increase_neulo_variables2$coef)]<-""
increase_neulo_variables2$se[is.na(increase_neulo_variables2$se)]<-""
increase_neulo_variables2$variable<-as.vector(increase_neulo_variables2$variable)
increase_neulo_variables2$coef<-as.numeric(as.vector(increase_neulo_variables2$coef))
increase_neulo_variables2$se<-as.numeric(as.vector(increase_neulo_variables2$se))

order<-1:dim(increase_neulo_variables2)[1]
increase_neulo_variables2 <- transform(increase_neulo_variables2, variable2=reorder(variable2, -order) ) 

## rbind all three - Average, High, Low
increase_neu <- rbind(increase_neuhi_variables2,increase_neulo_variables2)

increase_neu$id<-1:dim(increase_neu)[1]

increase_neu$dis<-NA
increase_neu$dis[increase_neu$id %in% c(1:20)] <- "High"
increase_neu$dis[increase_neu$id %in% c(21:40)] <- "Low"
increase_neu$dis[increase_neu$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Increase Commitment - Neuroticism")

increase_neu_p = ggplot(data = increase_neu, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.3,.2)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        ###theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
increase_neu_p        

ggsave(increase_neu_p , file = "weighted_increase_neu_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")

############################
## Plotting plots together
###########################
grid.arrange(start_ext_p, start_agr_p, start_con_p, start_neu_p, start_open_p, ncol=3)

grid.arrange(end_ext_p,end_agr_p,end_con_p,end_neu_p,end_open_p,ncol=3)

grid.arrange(increase_ext_p, increase_agr_p, increase_con_p, increase_neu_p, increase_open_p, ncol=3)

grid.arrange(start_con_p,end_con_p)
grid.arrange(start_open_p,end_open_p)
grid.arrange(start_agr_p,end_agr_p)
grid.arrange(start_neu_p,end_neu_p)
grid.arrange(start_ext_p,end_ext_p)

start_p
end_p
#########################################################
######         Cross Big Five Comparison       ##########
#########################################################

##################################################
##### Compare: Intervention- High Ext vs. High Con
##################################################
start_exthi_conshi <- rbind(start_exthi_variables2,start_conshi_variables2)

start_exthi_conshi$id<-1:dim(start_exthi_conshi)[1]

start_exthi_conshi$dis<-NA
start_exthi_conshi$dis[start_exthi_conshi$id %in% c(1:23)] <- "High\nExtraversion"
start_exthi_conshi$dis[start_exthi_conshi$id %in% c(24:46)] <- "High\nConscientiousness"
start_exthi_conshi$dis[start_exthi_conshi$id %in% c(2,6,9,13,16,20,25,29,32,36,39,43)] <- "Baseline"

#### Plotting 

title<-paste("Intervention - High Extraversion vs. High Conscientiousness")

start_exthi_conshi_p= ggplot(data = start_exthi_conshi, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

start_exthi_conshi_p   

ggsave(start_ext_p, file = "weighted_start_exthi_conshi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")


##################################################
##### Compare: End- High Ext vs. High Con
##################################################
end_exthi_conshi <- rbind(end_exthi_variables2, end_conhi_variables2)

end_exthi_conshi$id<-1:dim(end_exthi_conshi)[1]

end_exthi_conshi$dis<-NA
end_exthi_conshi$dis[end_exthi_conshi$id %in% c(1:20)] <- "High\nExtraversion"
end_exthi_conshi$dis[end_exthi_conshi$id %in% c(21:40)] <- "High\nConscientiousness"
end_exthi_conshi$dis[end_exthi_conshi$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Backing Down - High Extraversion vs. High Conscientiousness")

end_exthi_conshi_p= ggplot(data = end_exthi_conshi, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

end_exthi_conshi_p   

ggsave(end_exthi_conshi_p  , file = "weighted_end_exthi_conshi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")



##########################################
##### Compare: End- High Ext vs. High Agr   ####### NOT VERY SUBSTANTIVELY INTERESTING RESULTS
##########################################
end_exthi_agrhi <- rbind(end_exthi_variables2, end_agrhi_variables2)

end_exthi_agrhi$id<-1:dim(end_exthi_agrhi)[1]

end_exthi_agrhi$dis<-NA
end_exthi_agrhi$dis[end_exthi_agrhi$id %in% c(1:20)] <- "High\nExtraversion"
end_exthi_agrhi$dis[end_exthi_agrhi$id %in% c(21:40)] <- "High\nAgreeableness"
end_exthi_agrhi$dis[end_exthi_agrhi$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Backing Down - High Extraversion vs. High Agreeableness")

end_exthi_agrhi_p= ggplot(data = end_exthi_agrhi, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

end_exthi_agrhi_p   

ggsave(end_exthi_agrhi_p  , file = "weighted_end_exthi_agrhi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")

##################################################
##### Compare: End- High Ext vs. High Con
##################################################
end_exthi_conshi <- rbind(end_exthi_variables2, end_conhi_variables2)

end_exthi_conshi$id<-1:dim(end_exthi_conshi)[1]

end_exthi_conshi$dis<-NA
end_exthi_conshi$dis[end_exthi_conshi$id %in% c(1:20)] <- "High\nExtraversion"
end_exthi_conshi$dis[end_exthi_conshi$id %in% c(21:40)] <- "High\nConscientiousness"
end_exthi_conshi$dis[end_exthi_conshi$id %in% c(2,5,8,12,15,19,22,25,28,32,35,39)] <- "Baseline"

#### Plotting 

title<-paste("Backing Down - High Extraversion vs. High Conscientiousness")

end_exthi_conshi_p= ggplot(data = end_exthi_conshi, aes(x = coef, y = variable2, linetype=dis,  colour=dis)) +
        geom_point(aes(shape=dis), size=4)+
        geom_errorbarh( aes(xmin = coef - se, xmax = coef + se, height=.5), size=1)+
        scale_colour_manual(values=c("#009E73", "#CC6666", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="blue",linetype="dotted") +
        #theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=11, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

end_exthi_conshi_p   

ggsave(end_exthi_conshi_p  , file = "weighted_end_exthi_conshi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/Figure_criteria3_weighted")


################################################################
################ Tables Using stargazer #########################

devtools::install_github('rstudio/rmarkdown')

########################################
######## Intervene: Clustered SE ########
start_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
               data=start_stack))
#start_mod_cl<-cluster.vcov(start_mod, start_stack$resp_id)
#coeftest(start_mod,start_mod_cl)
library(sandwich)
cov1        <- vcovHC(start_mod, type = "HC1")
robust.se   <- sqrt(diag(cov1))

stargazer(start_mod, style="apsr", title="Table 3", se = list(robust.se, NULL),
          covariate.labels=c("Strength: As Strong As the U.S.", "Strength: Weaker than the U.S." ,"Casualties: Low", "Interest: Long-term Interest",
                             "Interest: High","US Reputation at Stake: High",
                             "Public Opinion: Evenly Split","Public Opinion: Support",
                             "Economy: Sliding into Recession","Economy: Recovering from Recession", 
                             "Economy: In Good Shape"),notes="Note: clustered standard errors in parentheses",
          dep.var.labels.include=FALSE, single.row=TRUE,
          column.labels=c("Intervene"))

#stargazer(start_mod, end_mod, se=list(coeftest(start_mod, start_mod_cl),se=coef(end_mod,end_mod_cl)), style="apsr" )
#stargazer(start_mod,se=list(coeftest(start_mod,start_mod_cl)),type="html")

########################################
######## Back Down: Clustered SE ########

end_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
             data=end_stack))
#end_mod_cl<-cluster.vcov(end_mod,end_stack$resp_id)
#coeftest(end_mod,end_mod_cl)


cov2        <- vcovHC(end_mod, type = "HC1")
robust.se2   <- sqrt(diag(cov2))

stargazer(end_mod, style="apsr",  se = list(robust.se2, NULL),
          covariate.labels=c("Spending: High", "Casualties: High", "Interest: Long-term Interest",
                             "Interest: Little","Personal Reputation: Little",
                             "Public Opinion: Evenly Split","Public Opinion: Against",
                             "U.S. Reputation: Little"),
          dep.var.labels.include=FALSE, single.row=TRUE, notes="Note: clustered standard errors in parentheses",
          column.labels=c("Back Down"))

############################################################
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


##########################################################
######### Backing Down: conhi, exthi, neulo ##############

stargazer(end_conhi_mod,end_exthi_mod,end_neulo_mod,style="apsr",
          covariate.labels=c("Spending: High","Casualties: High", "Interest: Long-term","Interest: Little",
                             "Personal Reputation at Stake: Little",
                             "Public Opinion: Evenly Split","Public Opinion: Against","US Reputation at Stake: Little"),
          title            = "Comparison of Personality Traits in Backing Down",
          column.labels = c("High Conscientiousness","High Extraversion","Low Neuroticism"),
          dep.var.labels.include  = FALSE, single.row=TRUE)








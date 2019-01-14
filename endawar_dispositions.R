####################################
### Dispostional Effects ##########
###################################


## change the reference categories
end_stack <- within(end_stack, personal_reputation <- relevel(personal_reputation, ref = "Humiliation if you back down"))
end_stack <- within(end_stack, casualties<-relevel(casualties, ref="Low"))
end_stack <- within(end_stack, spending<-relevel(spending, ref="Low"))
end_stack <- within(end_stack, interest<-relevel(interest, ref="High"))
end_stack <- within(end_stack, US_reputation<-relevel(US_reputation, ref="Damaging to Superpower status"))
end_stack <- within(end_stack, public<-relevel(public, ref="Support"))

end_stack$interest <- factor(end_stack$interest, levels=c("High","Not immediate, but long-term interest","Little"))

end_stack$public <- factor(end_stack$public, levels=c("Support","Evenly Split","Against"))

#################### End A War #####################
##################### Extraversion ###################
#source("endawar_extraversion.R")

## Extraversion -High
nrow(end_stack[end_stack$extraversionHi==1,]) #n=2376

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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_ext_p     


ggsave(end_ext_p , file = "weighted_end_ext_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")



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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_agr_p   

ggsave(end_agr_p , file = "weighted_end_agr_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")


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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_con_p        

ggsave(end_con_p , file = "weighted_end_con_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")



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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_neu_p        
ggsave(end_neu_p , file = "weighted_end_neu_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")



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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
end_open_p        

ggsave(end_open_p , file = "weighted_end_open_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")


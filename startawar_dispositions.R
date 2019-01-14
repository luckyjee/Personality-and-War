#################### Start A War #####################
## baseline
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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(axis.title=element_text(size=15,face="bold"))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

start_ext_p        

ggsave(start_ext_p, file = "weighted_start_ext_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")




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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.4)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(axis.title=element_text(size=15,face="bold"))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
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
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")




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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.4)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(axis.title=element_text(size=15,face="bold"))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_con_p        

ggsave(start_con_p, file = "weighted_start_con_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")



################### Start a War #####################
##################### Neuroticism ###################
#source("startawar_neuroticism.R")
## note: almost exactly the same effects between high and low (when criteria is 3)

## Neuroticism-High
nrow(start_stack[start_stack$neuroticismHi==1,]) #n=504; n=1686

#start_neuhi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
#                     data=start_stack[start_stack$conscientiousnessHi==1,],weight=disWts))

start_neuhi_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$conscientiousnessHi==1,]))

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

#start_neulo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
#                     data=start_stack[start_stack$neuroticismLo==1,],weight=disWts))

start_neulo_mod<-(lm(selected~strength+casualties+interest+reputation+public+economy,
                     data=start_stack[start_stack$neuroticismLo==1,]))

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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.3)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(axis.title=element_text(size=15,face="bold"))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_neu_p        

ggsave(start_neu_p, file = "weighted_start_neu_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")




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
        scale_colour_manual(values=c("black", "black", "#0072B2"))+
        xlab("")+
        xlim(-.2,.4)+
        ylab("")+
        geom_vline(xintercept = 0,size=.5,colour="black",linetype="dotted") +
        theme_bw()+
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(axis.title=element_text(size=15,face="bold"))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
start_open_p        # n=opennessLo was only 78

ggsave(start_open_p, file = "weighted_start_open_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")




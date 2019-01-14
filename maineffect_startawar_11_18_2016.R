#########################
## Run Models
#########################
########################### Main effects - Start a War ##################################
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
#start_mod<-amce(selected~strength+casualties+interest+reputation+public+economy, 
#                data=start_stack, cluster=TRUE,respondent.id="resp_id")
############# (END) using cjoint package ####################

start_mod<-lm(selected~strength+casualties+interest+reputation+public+economy, 
              data=start_stack)
# cluster se by "resp_id"
library(car)
library(lmtest)
#install.packages("multiwayvcov")
library(multiwayvcov)

start_mod$clse<-cluster.vcov(start_mod,start_stack$resp_id)
s<-coeftest(start_mod,start_mod$clse)
s

start_vars=variable.names(start_mod)
start_coefs = c(s[,1])
start_err = c(s[,2]*1.96)
start_tvals = c(s[,4])
start_tmp = data.frame(cbind(start_coefs,start_err))



## select particular coefficients
start_tmp2=start_tmp
start_tmp2$name = row.names(start_tmp2)
start_tmp2 = start_tmp2[,c("name","start_coefs","start_err")]
names(start_tmp2) = c("variable","coef","se")

start_tmp3<-start_tmp2[-1,] #remove the intercept row 
#start_tmp3<-start_tmp2
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
        theme(axis.title=element_text(size=15,face="bold"))+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title.y = element_blank()) +
        geom_vline(xintercept = 0,size=0.6,colour="black",linetype="dotted") +
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
       # ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(legend.position = "none")
start_p



ggsave(start_p, file = "main_clustered_intervention.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015")

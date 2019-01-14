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
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=11, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=11))+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(legend.position = "none")

increase_p

ggsave(increase_p, file = "main_effects_clustered_increase.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015")

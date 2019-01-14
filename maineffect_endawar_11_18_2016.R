
########################### Main effects - End a War ##################################


## change the reference categories
end_stack <- within(end_stack, personal_reputation <- relevel(personal_reputation, ref = "Humiliation if you back down"))
end_stack <- within(end_stack, casualties<-relevel(casualties, ref="Low"))
end_stack <- within(end_stack, spending<-relevel(spending, ref="Low"))
end_stack <- within(end_stack, interest<-relevel(interest, ref="High"))
end_stack <- within(end_stack, US_reputation<-relevel(US_reputation, ref="Damaging to Superpower status"))
end_stack <- within(end_stack, public<-relevel(public, ref="Support"))

end_stack$interest <- factor(end_stack$interest, levels=c("High","Not immediate, but long-term interest","Little"))

end_stack$public <- factor(end_stack$public, levels=c("Support","Evenly Split","Against"))

end_mod<-(lm(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
               data=end_stack))

#end_mod<-amce(end_selected~spending+casualties+interest+personal_reputation+public+US_reputation,
#              data=end_stack, cluster=TRUE,respondent.id="resp_id")

library(car)
library(lmtest)
library(multiwayvcov)

end_mod$clse<-cluster.vcov(end_mod,end_stack$resp_id)
e<-coeftest(end_mod, end_mod$clse)
e

end_vars=variable.names(end_mod)
end_coefs = c(e[,1])
end_err = c(e[,2]*1.96)
end_tvals = c(e[,4])
end_tmp = data.frame(cbind(end_coefs,end_err))


## select particular coefficients
end_tmp2=end_tmp
end_tmp2$name = row.names(end_tmp2)
end_tmp2 = end_tmp2[,c("name","end_coefs","end_err")]
names(end_tmp2) = c("variable","coef","se")

end_tmp3<-end_tmp2[-1,] # remove the intercept
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
        theme(axis.title=element_text(size=15,face="bold"))+
        theme_bw()+
        #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.title.y = element_blank()) +
        geom_vline(xintercept = 0,size=.6,colour="black",linetype="dotted") +
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        #ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))+
        theme(legend.position = "none")
end_p

ggsave(end_p, file = "main_clustered_end.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015")


## create regression table using stargazer package
#install.packages("stargazer")
library(stargazer)

stargazer(e, 
          covariate.labels = c("Spending: High", "Casualties: High", "Interest: Not Immediate, But Long-Term",
                               "Interest: Little", "Personal Reputation: Little", "Public Opinion: Evenly Split",
                               "Public Opinion: Against,", "US Reputation at Stake: Little"),
          dep.var.labels   = "Backing Down",
          notes="I do not include any demographic variables since none of them are significant.")

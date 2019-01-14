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
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

start_exthi_conshi_p   

ggsave(start_ext_p, file = "weighted_start_exthi_conshi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")


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
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

end_exthi_conshi_p   

ggsave(end_exthi_conshi_p  , file = "weighted_end_exthi_conshi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")



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
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

end_exthi_agrhi_p   

ggsave(end_exthi_agrhi_p  , file = "weighted_end_exthi_agrhi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")

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
        theme(axis.text.y = element_text(angle = 0, hjust = 0, color="black", size=15, face="bold"),
              axis.text.x=element_text(face="bold", color="black", size=15))+
        theme(legend.title=element_blank())+
        theme(legend.text=element_text(size=15, face="bold"))+
        theme(legend.position = "bottom") +
        theme(legend.key = element_blank())+
        ggtitle(title)+
        theme(plot.title = element_text(lineheight=.8, face="bold"))
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

end_exthi_conshi_p   

ggsave(end_exthi_conshi_p  , file = "weighted_end_exthi_conshi_p.png", 
       width = 12, 
       height = 10,
       path = "/Users/jihyunshin/Dropbox/Experiment_Summer2015/heroes_chickens_november_2015")


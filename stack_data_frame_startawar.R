################################################
## stack data frames for "Starting a War" Scenario
## 6 rows per respondent (2 conflicts x 3 scenarios)
###############################################

startcols1 = df_raw[,c("Intervene_choice1","Scale_con1","Strength_con1","Casualties_con1","Interest_con1",
                       "Economy_con1","Public_con1","Reputation_con1")]
startcols2 = df_raw[,c("Intervene_choice1","Scale_con2","Strength_con2","Casualties_con2","Interest_con2",
                       "Economy_con2","Public_con2","Reputation_con2")]
startcols3 = df_raw[,c("Intervene_choice2","Scale_con3","Strength_con3","Casualties_con3","Interest_con3",
                       "Economy_con3","Public_con3","Reputation_con3")]
startcols4 = df_raw[,c("Intervene_choice2","Scale_con4","Strength_con4","Casualties_con4","Interest_con4",
                       "Economy_con4","Public_con4","Reputation_con4")]
startcols5 = df_raw[,c("Intervene_choice3","Scale_con5","Strength_con5","Casualties_con5","Interest_con5",
                       "Economy_con5","Public_con5","Reputation_con5")]
startcols6 = df_raw[,c("Intervene_choice3","Scale_con6","Strength_con6","Casualties_con6","Interest_con6",
                       "Economy_con6","Public_con6","Reputation_con6")]

## Conflict Selection Variable
startcols1$selected = ifelse(startcols1$Intervene_choice1=="Conflict 1",1,0)
startcols2$selected = ifelse(startcols2$Intervene_choice1=="Conflict 2",1,0)
startcols3$selected = ifelse(startcols3$Intervene_choice2=="Conflict 3",1,0)
startcols4$selected = ifelse(startcols4$Intervene_choice2=="Conflict 4",1,0)
startcols5$selected = ifelse(startcols5$Intervene_choice3=="Conflict 5",1,0)
startcols6$selected = ifelse(startcols6$Intervene_choice3=="Conflict 6",1,0)

## Standardize Names
names(startcols1) = c("choice","scale","strength","casualties","interest","economy","public","reputation","selected")
names(startcols2) = c("choice","scale","strength","casualties","interest","economy","public","reputation","selected")
names(startcols3) = c("choice","scale","strength","casualties","interest","economy","public","reputation","selected")
names(startcols4) = c("choice","scale","strength","casualties","interest","economy","public","reputation","selected")
names(startcols5) = c("choice","scale","strength","casualties","interest","economy","public","reputation","selected")
names(startcols6) = c("choice","scale","strength","casualties","interest","economy","public","reputation","selected")


start_stack = cbind(rbind(startcols1,startcols2,startcols3,startcols4,startcols5,startcols6),
                    rbind(disposition, disposition,disposition,disposition,disposition,disposition),
                    rbind(df_dem,df_dem,df_dem,df_dem,df_dem,df_dem))

## Respondent ID
start_stack$resp_id = rep(seq(1,nrow(df_raw),1),6) # from 1 to 991, then repeat it 6 times

## Scenario ID - this is the number of the choice set
start_stack$scenario_id = rep(1:3,each=nrow(start_stack)/3) 

## Respondent - Scenario ID
start_stack$resp_scenario_id = paste(start_stack$resp_id,start_stack$scenario_id,sep="")
start_stack$resp_scenario_id = as.numeric(as.character(start_stack$resp_scenario_id))


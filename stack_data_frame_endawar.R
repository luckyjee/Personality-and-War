################################################
## stack data frames for "Ending a War" Scenario
## 6 rows per respondent (2 conflicts x 3 scenarios)
###############################################

endcols1 = df_raw[,c("End_choice1","Scale_con7","Increase_choice_con7_8","Spending_con7","Casualties_con7",
                     "Interest_con7", "Personal_con7","Public_con7","Reputation_con7")]
endcols2 = df_raw[,c("End_choice1","Scale_con8","Increase_choice_con7_8","Spending_con8","Casualties_con8",
                     "Interest_con8", "Personal_con8","Public_con8","Reputation_con8")]
endcols3 = df_raw[,c("End_choice2","Scale_con9","Increase_choice_9_10","Spending_con9","Casualties_con9",
                     "Interest_con9", "Personal_con9","Public_con9","Reputation_con9")]
endcols4 = df_raw[,c("End_choice2","Scale_con10","Increase_choice_9_10","Spending_con10","Casualties_con10",
                     "Interest_con10", "Personal_con10","Public_con10","Reputation_con10")]
endcols5 = df_raw[,c("End_choice3","Scale_con11","Increase_choice_11_12","Spending_con11","Casualties_con11",
                     "Interest_con11", "Personal_con11","Public_con11","Reputation_con11")]
endcols6 = df_raw[,c("End_choice3","Scale_con12","Increase_choice_11_12","Spending_con12","Casualties_con12",
                     "Interest_con12", "Personal_con12","Public_con12","Reputation_con12")]

## Conflict Selection Variable
endcols1$selected = ifelse(endcols1$End_choice1=="Conflict 7",1,0)
endcols2$selected = ifelse(endcols2$End_choice1=="Conflict 8",1,0)
endcols3$selected = ifelse(endcols3$End_choice2=="Conflict 9",1,0)
endcols4$selected = ifelse(endcols4$End_choice2=="Conflict 10",1,0)
endcols5$selected = ifelse(endcols5$End_choice3=="Conflict 11",1,0)
endcols6$selected = ifelse(endcols6$End_choice3=="Conflict 12",1,0)

endcols1$task_profile<-"7"
endcols2$task_profile<-"8"
endcols3$task_profile<-"9"
endcols4$task_profile<-"10"
endcols5$task_profile<-"11"
endcols6$task_profile<-"12"

## Increase Military Commitment
endcols1$increase = ifelse(endcols1$Increase_choice_con7_8 %in% c("Conflict 7","Both"),1,0)
endcols2$increase = ifelse(endcols2$Increase_choice_con7_8 %in% c("Conflict 8","Both"),1,0)
endcols3$increase = ifelse(endcols3$Increase_choice_9_10 %in% c("Conflict 9","Both"),1,0)
endcols4$increase = ifelse(endcols4$Increase_choice_9_10 %in% c("Conflict 10","Both"),1,0)
endcols5$increase = ifelse(endcols5$Increase_choice_11_12 %in% c("Conflict 11","Both"),1,0)
endcols6$increase = ifelse(endcols6$Increase_choice_11_12 %in% c("Conflict 12","Both"),1,0)

## Standardize Names
names(endcols1) = c("end_choice","scale","increase_choice","spending","casualties","interest","personal_reputation","public","US_reputation","end_selected", "task_profile_end","increase_selected")
names(endcols2) = c("end_choice","scale","increase_choice","spending","casualties","interest","personal_reputation","public","US_reputation","end_selected","increase_selected")
names(endcols3) = c("end_choice","scale","increase_choice","spending","casualties","interest","personal_reputation","public","US_reputation","end_selected","increase_selected")
names(endcols4) = c("end_choice","scale","increase_choice","spending","casualties","interest","personal_reputation","public","US_reputation","end_selected","increase_selected")
names(endcols5) = c("end_choice","scale","increase_choice","spending","casualties","interest","personal_reputation","public","US_reputation","end_selected","increase_selected")
names(endcols6) = c("end_choice","scale","increase_choice","spending","casualties","interest","personal_reputation","public","US_reputation","end_selected","increase_selected")


end_stack = cbind(rbind(endcols1,endcols2,endcols3,endcols4,endcols5,endcols6),
                  rbind(disposition, disposition,disposition,disposition,disposition,disposition),
                  rbind(df_dem,df_dem,df_dem,df_dem,df_dem,df_dem))

## Respondent ID
end_stack$resp_id = rep(seq(1,nrow(df_raw),1),6) # from 1 to 991, then repeat it 6 times

## Scenario ID - this is the number of the choice set
end_stack$scenario_id = rep(1:3,each=nrow(end_stack)/3) 

## Respondent - Scenario ID
end_stack$resp_scenario_id = paste(end_stack$resp_id,end_stack$scenario_id,sep="")
end_stack$resp_scenario_id = as.numeric(as.character(end_stack$resp_scenario_id))


## Note: create a data frame on demographics 


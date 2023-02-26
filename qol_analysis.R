require(tidyverse)
require(haven)
require(labelled)
mcf_data <- read_sav("data/mcf_data_master.sav")
mcf_data1 <- mcf_data%>%
  select(inco_total,gender,education,pwd,stratum,refugee_brkdwn,
         computer_ownership,phone_ownership,mart_status,age,hh_gender,
         own_farming,mastcard_progr,matches("^language_[0-9]+$"),educ_quality,
         educ_knowledge,ownasset_1,equiment_1,
         equiment_7,have_electricity,inc_genjob,wek_howork,main_sector,sust_wage,
         sust_self_employment,indi_need,fami_need,sense_purp,shocks,how_parti,
         remi_receive,com_pers,gotten_friend,attend_church,two_views,otherviews,
         will_happen,plan_ahead,life_control,determine,worked_hard,my_actions,
         quality_life_8_services,-language_3,trainings_0,trainings_1,trainings_6)%>%
  mutate_at(
    vars(-c("inco_total","age","quality_life_8_services","wek_howork","gotten_friend","attend_church")),
            as.factor)%>%
  mutate_at(vars(c("inco_total","age","wek_howork","gotten_friend","attend_church")),as.integer)

model1 <- glm(quality_life_8_services~.,data = mcf_data1,family = "gaussian")
summary(model1)

ggplot(data=NULL,aes(predict(model1),residuals(model1)))+
  geom_point()+
  geom_smooth()

ggplot(data=NULL,aes(seq(1,length(hatvalues(model1))),hatvalues(model1)))+
  geom_point()

which.max(hatvalues(model1))

#####################################
mcf_data_char <- characterize(mcf_data)%>%
  dplyr::select(inco_total,gender,education,pwd,stratum,refugee_brkdwn,
         computer_ownership,phone_ownership,mart_status,age,hh_gender,
         own_farming,mastcard_progr,matches("^language_[0-9]+$"),educ_quality,
         educ_knowledge,ownasset_1,equiment_1,
         equiment_7,have_electricity,inc_genjob,wek_howork,main_sector,sust_wage,
         sust_self_employment,indi_need,fami_need,sense_purp,shocks,how_parti,
         remi_receive,com_pers,gotten_friend,attend_church,two_views,otherviews,
         will_happen,plan_ahead,life_control,determine,worked_hard,my_actions,
         quality_life_8_services,trainings_0,trainings_1,trainings_6)%>%
  mutate_at(
    vars(-c("inco_total","age","quality_life_8_services","wek_howork","gotten_friend","attend_church")),
    as.factor)%>%
  mutate_at(vars(c("inco_total","age","wek_howork","gotten_friend","attend_church")),as.integer)


modelchar <- lm(quality_life_8_services ~ ., data = mcf_data_char)
summary(modelchar)
vif(modelchar)
summary(
  lm(
    quality_life_8_services ~ . - language_0 - sust_wage - indi_need - 
      sust_self_employment,
    data = mcf_data_char
  )
)
vif(
  lm(
    quality_life_8_services ~ . - language_0 - sust_wage - indi_need - 
      sust_self_employment,
    data = mcf_data_char
  )
)

modelchar1 <-
  lm(
    quality_life_8_services ~ . - language_0 - sust_wage - indi_need - 
      sust_self_employment - education,
    data = mcf_data_char
  )
summary(modelchar1)
vif(modelchar1)
















mcf_data2 <- mcf_data%>%
  select(gender,education_brkdwn,remi_receive,fami_need,
         quality_life_8_services)%>%
  mutate_at(
    vars(-c("quality_life_8_services")),
    as.factor)

model2 <- glm(quality_life_8_services~.,data = mcf_data2,family = "gaussian")
summary(model2)
# write_sav(mcf_data1,"data/mcf_qol.sav")

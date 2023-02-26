# install.packages("tidyverse")
# install.packages("haven")
# install.packages("labelled")
# install.packages("car")
require(car)
require(tidyverse)
require(haven)
require(labelled)
require(rio)
mcf_data <- read_sav("data/mcf_data_master.sav")
mcf_data1 <- mcf_data %>%
  select(
    inco_total,
    gender,
    education,
    pwd,
    stratum,
    refugee_brkdwn,
    computer_ownership,
    phone_ownership,
    mart_status,
    age,
    hh_gender,
    own_farming,
    mastcard_progr,
    matches("^language_[0-9]+$"),
    educ_quality,
    educ_knowledge,
    ownasset_1,
    equiment_1,
    equiment_7,
    have_electricity,
    inc_genjob,
    wek_howork,
    main_sector,
    sust_wage,
    sust_self_employment,
    indi_need,
    fami_need,
    sense_purp,
    shocks,
    how_parti,
    remi_receive,
    com_pers,
    gotten_friend,
    attend_church,
    two_views,
    otherviews,
    will_happen,
    plan_ahead,
    life_control,
    determine,
    worked_hard,
    my_actions,
    quality_life_8_services,
    -language_3,
    trainings_0,
    trainings_1,
    trainings_6
  ) %>%
  mutate_at(vars(
    -c(
      "inco_total",
      "age",
      "quality_life_8_services",
      "wek_howork",
      "gotten_friend",
      "attend_church"
    )
  ),
  as.factor) %>%
  mutate_at(vars(
    c(
      "inco_total",
      "age",
      "wek_howork",
      "gotten_friend",
      "attend_church"
    )
  ), as.integer)

vif_values <- as_tibble(vif(lm(quality_life_8_services~.,data=mcf_data1)),rownames="predy")
ggplot(data = vif_values,aes(GVIF,predy))+
  geom_bar(stat = "identity")

barplot(vif_values$GVIF, main = "VIF Values", horiz = TRUE, col = "steelblue")

mcf_data2 <- mcf_data1%>%
  select(indi_need,quality_life_8_services)
model1 <- lm(quality_life_8_services~.,data=mcf_data2) 
summary(model1)
ggplot(data = as_tibble(model1$residuals),aes(value))+
  geom_density()

ggplot(data = tibble(residu=model1$residuals,fitty=model1$fitted.values),
       aes(fitty,residu))+
  geom_point()+
  # geom_smooth()+
  geom_hline(aes(yintercept=mean(model1$residuals)))



mcf_data2 <- mcf_data1%>%
  select(age,quality_life_8_services)
model1 <- lm(quality_life_8_services~.,data=mcf_data2) 
summary(model1)
ggplot(data = as_tibble(model1$residuals),aes(value))+
  geom_density()


ggplot(data = mcf_data1,
       aes(log(inco_total),log(quality_life_8_services)))+
  geom_point()+
  geom_smooth()



data_fitted <- tibble(residu=model1$residuals,fitty=model1$fitted.values)
ggplot(data = data_fitted,
       aes(seq(1,nrow(data_fitted)),residu))+
  geom_point()+
  geom_smooth()+
  geom_hline(aes(yintercept=mean(model1$residuals)))
model1$effects

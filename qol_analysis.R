# remove.packages("cli")
# remove.packages("usethis")
# remove.packages("tidyverse")
# remove.packages("htmltools")
# install.packages("usethis")
# install.packages("cli")
# install.packages("tidyverse")
# install.packages("htmltools")
# update.packages("Rtools")
# update.packages("devtools")
require(tidyverse)
require(haven)
require(labelled)
require(rio)
require(knitr)
#library(devtools)
#install_github("dgrtwo/broom")
require(broom)
require(officer)
require(magrittr)
require(flextable)
require(rvg)
#install.packages("mschart")
#require(mschart)
#devtools::install_github('davidgohel/ReporteRs')


Light_grey <- c("#F2F2F2") #Light grey for the background
Blue <- c("#097ABC") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#1b2f55") #Dark blue
Green <- c("#8ccc98") #Green
Dark_grey <- c("#7F7F7F") #Dark grey
Dark_green <- c("#49711E") #Dark green

mcf_data <- read_sav("data/mcf_data_master.sav")
mcf_data1 <- characterize(mcf_data) %>%
  dplyr::select(
    inco_total,
    gender,
    education_brkdwn,
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
    language_1,
    language_2,
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
    trainings_0,
    trainings_1,
    trainings_6
  ) %>%
  dplyr::mutate_at(vars(
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
  dplyr::mutate_at(vars(
    c(
      "inco_total",
      "age",
      "wek_howork",
      "gotten_friend",
      "attend_church"
    )
  ), as.integer)

mcf_data2 <- mcf_data1 %>%
  mutate(quality_life_8_services = log(quality_life_8_services))
mcf_data2 <-
  do.call(data.frame, lapply(mcf_data2, function(x)
    replace(x, is.infinite(x), NA)))
model1 <-
  glm(quality_life_8_services ~ .,
      data = mcf_data1,
      family = "gaussian")
summary(model1)
# convert the model output into table that can be export to word
model1_table <- tidy(model1)
ftab <- flextable(model1_table)
ftab <- colformat_double(
  x = ftab,
  big.mark = ",",
  digits = 2,
  na_str = "N/A"
)

ftab <- autofit(ftab)
std_border <- fp_border(color = "gray")
ftab <- border_remove(x = ftab)
ftab <- hline(ftab, part = "all", border = std_border)
std_borderv <- fp_border(color = "gray")
ftab <- vline(ftab, border = std_borderv)
ftab <- border(ftab, border = fp_border(color = "grey"))
ftab <-
  border(ftab, border = fp_border(color = "grey"), part = "header")
ftab <- fontsize(ftab, size = 7, part = "header")
ftab <- fontsize(ftab, size = 6.5, part = "body")

doc <- read_docx(path = "data/temp.docx")
doc <- body_add_flextable(doc, ftab)
print(doc, target = "data/temp.docx")

# using sink to preserve results in txt format
sink("data/lm.txt")
print(summary(model1))
sink()


#data visualization of selected variables
mcf_data %>%
  ggplot(aes(quality_life_8_services)) +
  geom_histogram(aes(y = after_stat(count) * 100 / sum(after_stat(count))), fill =
                   Light_blue) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

mcf_data %>%
  select(age, quality_life_8_services, weights) %>%
  group_by(age) %>%
  summarise(mean_qol_age = mean(quality_life_8_services)) %>%
  ggplot(aes(age, mean_qol_age)) +
  geom_point() +
  geom_smooth(se = FALSE, color = Blue) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )
mean(mcf_data$age)
sd(mcf_data$age)
mcf_data %>%
  group_by(gender) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = total * 100 / sum(total))

mcf_data %>%
  group_by(gender) %>%
  summarize(mean_qol = mean(quality_life_8_services))

mcf_data %>%
  group_by(geo_entity) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = total * 100 / sum(total))

mcf_data %>%
  group_by(geo_entity) %>%
  summarize(mean_qol = mean(quality_life_8_services))

mcf_data %>%
  group_by(gender) %>%
  summarize(mean_qol = mean(quality_life_8_services))

mcf_data %>%
  group_by(education_brkdwn) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total)))

mcf_data %>%
  group_by(education_brkdwn) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(education_brkdwn, mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

mcf_data %>%
  ggplot(aes(log10(inco_total), quality_life_8_services)) +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    #axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

mcf_data %>%
  group_by(income_classes) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(
    income_classes,
    level = c(
      "",
      "0",
      "1-20,000",
      "20,000-40,000",
      "40,000-60,000",
      "60,000-80,000",
      "80,000-100,000",
      "100,000-500,000",
      "500,000-1,000,000",
      "Above 1,000,000"
    )
  ), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

mcf_data %>%
  group_by(income_classes) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

mcf_data %>%
  group_by(phone_ownership) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

mcf_data %>%
  mutate(phoney = if_else(phone_ownership == 1 |
                            phone_ownership == 2, 1, 0)) %>%
  group_by(phoney) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(phoney), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Smartphone ownership
mcf_data %>%
  group_by(equiment_5) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

mcf_data %>%
  group_by(equiment_5) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(equiment_5), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#computer ownership
mcf_data %>%
  group_by(equiment_6) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

mcf_data %>%
  group_by(equiment_6) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(equiment_6), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#farming activities
mcf_data %>%
  group_by(sell_goods) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

mcf_data %>%
  group_by(sell_goods) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(sell_goods), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Education Knowledge (education related work opportunities)
mcf_data %>%
  group_by(educ_knowledge) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

characterize(mcf_data) %>%
  group_by(educ_knowledge) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(educ_knowledge), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#House ownership
mcf_data %>%
  group_by(ownasset_1) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

characterize(mcf_data) %>%
  group_by(ownasset_1) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(ownasset_1), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#House ownership
mcf_data %>%
  group_by(equiment_1) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

characterize(mcf_data) %>%
  group_by(equiment_1) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(equiment_1), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Car ownership
mcf_data %>%
  group_by(equiment_7) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

characterize(mcf_data) %>%
  group_by(equiment_7) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(equiment_7), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Electricity access
mcf_data %>%
  group_by(have_electricity) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

characterize(mcf_data) %>%
  group_by(have_electricity) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(have_electricity), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Income generating job
mcf_data %>%
  group_by(inc_genjob) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2))

characterize(mcf_data) %>%
  group_by(inc_genjob) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(inc_genjob), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Hours worked
view(
  mcf_data %>%
    group_by(wek_howork) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(wek_howork) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(wek_howork), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

characterize(mcf_data) %>%
  group_by(wek_howork) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(as.integer(wek_howork))) +
  geom_histogram(fill = Blue, binwidth = 40)

#Main activity
view(
  characterize(mcf_data) %>%
    group_by(main_sector) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(main_sector) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(main_sector), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#Employment status
view(
  characterize(mcf_data) %>%
    group_by(stratum) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(stratum) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(stratum), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#individual needs
view(
  characterize(mcf_data) %>%
    group_by(indi_need) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(indi_need) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(stratum), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#family needs
view(
  characterize(mcf_data) %>%
    group_by(fami_need) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(fami_need) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(stratum), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#mastercard programs participation
view(
  characterize(mcf_data) %>%
    group_by(mastcard_progr) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(mastcard_progr) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ggplot(aes(factor(stratum), mean_qol)) +
  geom_bar(stat = "identity", fill = Blue) +
  geom_text(aes(label = paste0(round(mean_qol, 1))),
            vjust = -.25,
            size = 2.25) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove y axis value labels
    axis.text.y = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #remove y axis ticks
    #rotating axis labels
    axis.text.x = element_text(
      angle = 50,
      vjust = 1,
      hjust = 1
    )
  )

#language skills
view(
  characterize(mcf_data) %>%
    group_by(language_1) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)
view(
  characterize(mcf_data) %>%
    group_by(language_2) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)
view(
  characterize(mcf_data) %>%
    group_by(language_3) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)
characterize(mcf_data) %>%
  group_by(language_1) %>%
  summarize(mean_qol = mean(quality_life_8_services))
characterize(mcf_data) %>%
  group_by(language_2) %>%
  summarize(mean_qol = mean(quality_life_8_services))
characterize(mcf_data) %>%
  group_by(language_3) %>%
  summarize(mean_qol = mean(quality_life_8_services))

#quality of education
view(
  characterize(mcf_data) %>%
    group_by(educ_quality) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(educ_quality) %>%
  summarize(mean_qol = mean(quality_life_8_services))

#trainings attended
view(
  characterize(mcf_data) %>%
    mutate(trainings_status = 
             ifelse(trainings == "0", "No trainings", "trained")) %>%
    group_by(trainings_status) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  mutate(trainings_status = 
           ifelse(trainings == "0", "No trainings", "trained")) %>%
  group_by(trainings_status) %>%
  summarize(mean_qol = mean(quality_life_8_services))

#sense of purpose
view(
  characterize(mcf_data) %>%
    group_by(sense_purp) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(sense_purp) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# shocks
view(
  characterize(mcf_data) %>%
    group_by(shocks) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(shocks) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# level of participation
view(
  characterize(mcf_data) %>%
    group_by(how_parti) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(how_parti) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# remittance reception
view(
  characterize(mcf_data) %>%
    group_by(remi_receive) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(remi_receive) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# communication status
view(
  characterize(mcf_data) %>%
    group_by(com_pers) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(com_pers) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# friendship status
join_main <- mcf_data %>%
  group_by(gotten_friend) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
  select(gotten_friend, prop_total)


join_sec <- mcf_data %>%
  group_by(gotten_friend) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ungroup() %>%
  left_join(join_main, by = "gotten_friend")
ftab <- flextable(join_sec)
ftab <- colformat_double(
  x = ftab,
  big.mark = ",",
  digits = 2,
  na_str = "N/A"
)

ftab <- autofit(ftab)
std_border <- fp_border(color = "gray")
ftab <- border_remove(x = ftab)
ftab <- hline(ftab, part = "all", border = std_border)
std_borderv <- fp_border(color = "gray")
ftab <- vline(ftab, border = std_borderv)
ftab <- border(ftab, border = fp_border(color = "grey"))
ftab <-
  border(ftab, border = fp_border(color = "grey"), part = "header")
ftab <- fontsize(ftab, size = 7, part = "header")
ftab <- fontsize(ftab, size = 6.5, part = "body")

doc <- read_docx(path = "data/temp.docx")
doc <- body_add_flextable(doc, ftab)
print(doc, target = "data/temp.docx")

# church attendance
join_main <- mcf_data %>%
  group_by(attend_church) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
  select(attend_church, prop_total)


join_sec <- mcf_data %>%
  group_by(attend_church) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ungroup() %>%
  left_join(join_main, by = "attend_church")
ftab <- flextable(join_sec)
ftab <- colformat_double(
  x = ftab,
  big.mark = ",",
  digits = 2,
  na_str = "N/A"
)

ftab <- autofit(ftab)
std_border <- fp_border(color = "gray")
ftab <- border_remove(x = ftab)
ftab <- hline(ftab, part = "all", border = std_border)
std_borderv <- fp_border(color = "gray")
ftab <- vline(ftab, border = std_borderv)
ftab <- border(ftab, border = fp_border(color = "grey"))
ftab <-
  border(ftab, border = fp_border(color = "grey"), part = "header")
ftab <- fontsize(ftab, size = 7, part = "header")
ftab <- fontsize(ftab, size = 6.5, part = "body")

doc <- read_docx(path = "data/temp.docx")
doc <- body_add_flextable(doc, ftab)
print(doc, target = "data/temp.docx")

# views
view(
  characterize(mcf_data) %>%
    group_by(two_views) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(two_views) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# other views
view(
  characterize(mcf_data) %>%
    group_by(otherviews) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(otherviews) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# fate beliefs
view(
  characterize(mcf_data) %>%
    group_by(will_happen) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(will_happen) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# plan ahead
view(
  characterize(mcf_data) %>%
    group_by(plan_ahead) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(plan_ahead) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# Life control
view(
  characterize(mcf_data) %>%
    group_by(life_control) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(life_control) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# hard work 
view(
  characterize(mcf_data) %>%
    group_by(worked_hard) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
    print(n = 888)
)

characterize(mcf_data) %>%
  group_by(worked_hard) %>%
  summarize(mean_qol = mean(quality_life_8_services))

# church attendance
join_main <- mcf_data %>%
  group_by(wek_howork) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop_total = round(total * 100 / sum(total), 2)) %>%
  select(wek_howork, prop_total)


join_sec <- mcf_data %>%
  group_by(wek_howork) %>%
  summarize(mean_qol = mean(quality_life_8_services)) %>%
  ungroup() %>%
  left_join(join_main, by = "wek_howork")

join_sec %>%
  ggplot(aes(mean_qol)) +
  geom_histogram(aes(y = after_stat(count) * 100 / sum(after_stat(count))), fill =
                   Light_blue) +
  xlim(c(0,50))+
  ylim(c(0,30))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

join_sec %>%
  ggplot(mapping = aes(x = wek_howork,y = mean_qol)) +
  geom_bar(fill =Light_blue,stat = "identity") +
  xlim(c(0,180))+
  ylim(c(0,50))+
  xlab("Hours worked per week")+
  ylab("Average QLI")+
  labs(title = "Distribution of average QLI per weekly hours worked")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )


# use of point biserial correlation for dichotomous variable and cont. variable
mcf_data %>%
  ggplot(aes(x = quality_life_8_services,y = geo_entity))+
  geom_point() + coord_flip()
cor.test(mcf_data$quality_life_8_services,mcf_data$geo_entity)

mcf_data %>%
  ggplot(aes(y = quality_life_8_services,x = as.factor(geo_entity)))+
  geom_boxplot()

characterize(mcf_data) %>%
  ggplot(aes(y = quality_life_8_services,x = as.factor(worked_hard)))+
  geom_boxplot() + 
  coord_flip()

characterize(mcf_data) %>%
  ggplot(aes(y = quality_life_8_services,x = as.factor(determine)))+
  geom_boxplot() + 
  coord_flip()

# Model creation and analysis####
mcf_data1 <- mcf_data %>%
  dplyr::select(
    inco_total,
    gender,
    education_brkdwn,
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
    language_1,
    language_2,
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
    trainings_0,
    trainings_1,
    trainings_6
  )

mcf_data2 <- mcf_data1 %>%
  mutate(widower = if_else(mart_status == 6,1,0,missing=NA),
         stratum = case_when(stratum == "Non-employed"~1,
                             stratum == "Student"~2,
                             stratum == "Self-employed"~3,
                             stratum == "Wage-employed"~4),
         Nofarming = if_else(own_farming==0,1,0,missing=NA),
         Nocomputer = if_else(computer_ownership ==4,1,0,missing=NA),
         inco_total = log(inco_total),
         gender = if_else(gender==2,0,1,missing = NA),
         hh_gender = if_else(hh_gender == 2, 0,1,missing=NA),
         refugee_brkdwn = if_else(refugee_brkdwn == "Refugee",1,0,missing = NA),
         education_brkdwn = case_when(education_brkdwn == "None"~0,
                                      education_brkdwn == "Primary"~1,
                                      education_brkdwn == "Secondary"~2,
                                      education_brkdwn == "TVET"~3,
                                      education_brkdwn == "University"~4),
         pwd = if_else(pwd==1,1,0,missing=NA),
         educ_quality = case_when(educ_quality == 1~2,
                                  educ_quality == 2~1,
                                  educ_quality == 3~-1,
                                  educ_quality == 4~-2,
                                  TRUE~NA_real_),
         educ_knowledge = case_when(educ_knowledge == 1~3,
                                    educ_knowledge == 2~2,
                                    educ_knowledge == 3~1,
                                    educ_knowledge == 4~0,
                                    TRUE~NA_real_),
         how_parti = case_when(how_parti == 1~0,
                               how_parti == 2~1,
                               how_parti == 3~2,
                               how_parti == 4~3,
                               TRUE~NA_real_),
         phone_ownership = case_when(phone_ownership == 1~2,
                                     phone_ownership == 2~1,
                                     phone_ownership == 3~0,
                                     TRUE~NA_real_),
         mastcard_progr = if_else(
           mastcard_progr == 2, 0, 1, missing = NA
         ),
         have_electricity = if_else(
           have_electricity == 2, 0,1,missing = NA
         ),
         agriculture = if_else(main_sector == 1,1,0,missing=NA),
         industry = if_else(main_sector == 2,1,0,missing=NA),
         services = if_else(main_sector == 3, 1, 0, missing = NA),
         remi_receive = if_else(remi_receive == 2,0,1,missing = NA),
         com_pers = if_else(com_pers==2,0,1,missing = NA),
         two_views = if_else(two_views == 2,0,1,missing = NA),
         otherviews = if_else(otherviews == 2, 0, 1,missing = NA))%>%
  select(-main_sector,-own_farming,-computer_ownership,-mart_status)%>%
  characterize()%>%
  mutate_at(c("my_actions","worked_hard","determine","life_control","plan_ahead",
              "will_happen","indi_need","fami_need","sense_purp"),as.character)%>%
  mutate(across(c(my_actions,worked_hard,determine,life_control,plan_ahead,
                  will_happen),~case_when(.=="a. Disagree"~-2,
                                           .=="b. Slightly disagree"~-1,
                                           .=="c. Slightly agree"~1,
                                           .=="d. Agree"~2,
                                           .=="e. Strongly agree"~3,
                                           TRUE~NA_real_)))%>%
  mutate(across(c(indi_need,fami_need,sense_purp),~case_when(
                    .=="a. Strongly disagree"~-2,
                    .=="b. Disagree"~-1,
                    .=="c. Neutral"~0,
                    .=="d. Agree"~1,
                    .=="e. Strongly Agree"~2,
                    TRUE~NA_real_
                  )))
mcf_data2 <-
  do.call(data.frame, lapply(mcf_data2, function(x)
    replace(x, is.infinite(x), NA)))
model1 <-
  glm(quality_life_8_services ~ .,
      data = mcf_data2,
      family = "gaussian")
summary(model1)

model2 <-
  glm(quality_life_8_services ~ .-language_2-sust_wage-sust_self_employment
      -attend_church-plan_ahead-trainings_1-agriculture-industry,
      data = mcf_data2,
      family = "gaussian")
summary(model2)

model3 <-
  glm(quality_life_8_services ~ .-language_2-sust_wage-sust_self_employment
      -attend_church-plan_ahead-trainings_1-agriculture-industry-services
      -stratum-educ_quality,
      data = mcf_data2,
      family = "gaussian")
summary(model3)

model4 <-
  glm(quality_life_8_services ~ .-language_2-sust_wage-sust_self_employment
      -attend_church-plan_ahead-trainings_1-agriculture-industry-services
      -stratum-educ_quality-refugee_brkdwn-phone_ownership-language_1-ownasset_1
      -inc_genjob-two_views-otherviews-will_happen-determine-worked_hard
      -trainings_0,
      data = mcf_data2,
      family = "gaussian")
summary(model4)

model5 <-
  glm(quality_life_8_services ~ .-language_2-sust_wage-sust_self_employment
      -attend_church-plan_ahead-trainings_1-agriculture-industry-services
      -stratum-educ_quality-refugee_brkdwn-phone_ownership-language_1-ownasset_1
      -inc_genjob-two_views-otherviews-will_happen-determine-worked_hard
      -trainings_0,
      data = mcf_data2,
      family = "gaussian")
summary(model5)
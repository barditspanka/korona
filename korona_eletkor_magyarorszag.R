rm(list = ls())
setwd("/Users/barditsanna/Desktop/korona/")

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)

#https://koronavirus.gov.hu/elhunytak oldalrol, letöltve 2020.06.04
deaths_HUN <- read_excel("magyaro_halalok.xlsx")

# a koreloszláshoz a 2020-as népeeség előreszámítást használom https://www.ksh.hu/interaktiv/korfak/orszag.html
pop_by_age_hungary <- read_excel("korfa_mo_2020.xlsx")

# koronavirusban elhunytak száma életkoronként
num_deaths <- deaths_HUN %>% 
#csak nők/férfiak kiválasztása  
#filter(nem=="Férfi")  %>% 
  group_by(kor) %>% 
  summarize(NUM_DEATH=n())

deaths_by_age_hungary <- full_join(pop_by_age_hungary, num_deaths, by = c("AGE" = "kor"))
deaths_by_age_hungary <- deaths_by_age_hungary %>%
  mutate(NUM_DEATH = ifelse(is.na(NUM_DEATH), 0, NUM_DEATH) ) %>%
  mutate(DEATH_RATIO=NUM_DEATH/TOTAL_POP)%>%
  mutate(LOG_DEATH_RATIO=log(DEATH_RATIO, 10))%>%
  mutate(LOG_DEATH_RATIO = ifelse(is.infinite(LOG_DEATH_RATIO), NA, LOG_DEATH_RATIO) ) %>%
  mutate(BCG = as.factor(ifelse(birthyear>1953, 1, 0)))  %>%
  mutate(BCG3 = cut(birthyear, breaks=c(-Inf, 1949, 1953, Inf), labels(c("ninncs széleskörű BCG oltás","újszülöttek 30-40%-a beoltva","minden újszülött beoltva"))))
#http://real-j.mtak.hu/11220/4/650.1962.06.03.pdf alapján BCG3 változó labeljei

#ábra az 1 emberre jutó halálok számáról az adott korosztálbyan
filter(deaths_by_age_hungary, AGE>50 & AGE<90) %>%
  ggplot(aes(x = AGE, y = DEATH_RATIO, color=BCG)) + 
    geom_point() + stat_smooth(method = loess, level = 0) +
    xlab("Életkor") +
    ylab("Elhunytak aránya az adott életkorú csoportban") +
  scale_color_discrete(name="",
                      breaks=c("0", "1"),
                      labels=c("nincs széleskörű BCG oltás", "széleskörű BCG oltás")) +
  scale_y_continuous(labels=scales::percent)

#logaritmikus skálán ábrázolva, 50-53 időszak különvéve
filter(deaths_by_age_hungary, AGE>50 & AGE<90) %>%
  mutate(DEATH_RATIO = ifelse(DEATH_RATIO==0, NA, DEATH_RATIO) ) %>%
  ggplot(aes(x = AGE, y = DEATH_RATIO, color=BCG3)) + 
  geom_point() +
  xlab("Életkor") +
  ylab("Elhunytak aránya az adott életkorú csoportban \n (log skála)") +
  scale_color_discrete(name="",
                       breaks=c("3", "2", "1"),
                       labels=c("minden újszülött beoltva","újszülöttek 30-40%-a beoltva","nincs széleskörű újszülött oltás"))+
  scale_y_log10(labels=scales::percent)

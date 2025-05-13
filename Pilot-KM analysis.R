library("ggplot2")
library("tidyverse")
library("survival")
library("survminer")
library("car")
library(dplyr)

install.packages(coxme)
library("coxme")

setwd("~/Downloads/longevity")
longevityall=read.csv("Longevity sorted data.csv")

longevityall$Density <- as.character(longevityall$Density)
longevityall <- longevityall %>%
  mutate(Density = replace(Density, Density == "10:10:00", "20")) %>% 
  mutate(Density = replace(Density, Density == "20:20:00", "40"))

longevityall$Sex <- as.factor(longevityall$Sex)
longevityall$Virgin <- as.factor(longevityall$Virgin)
longevityall$Yeast <- as.factor(longevityall$Yeast)
longevityall$Density <- as.factor(longevityall$Density)


km_fit_all <- survfit(Surv(Day,Event) ~ Sex + Virgin + Yeast + as.factor(Density), data=longevityall) 
summary(km_fit_all)
ggsurvplot(km_fit_all, data=longevityall)
longevityall<-droplevels(longevityall)
cox_model_all <- coxme(Surv(Day,Event) ~ Sex*Virgin*Yeast+Density+(1|Vial.ID), data=droplevels(longevityall))

Anova(cox_model_all)
summary(cox_model_all)


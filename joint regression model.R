rm(list = ls())
setwd("C:/Users/P269380/Documents/GOR covid-19/")

library(tidyverse)
library(lcmm)
library(broom)
library(parallel)
library(patchwork)
library(skimr)
source("./gor-covid-19/final scripts/scripts/00_variable_groups.R")

ncores <- max(1, parallel::detectCores(logical = TRUE) - 2)

lcmm_compleet <- read.csv("./gor-covid-19/final scripts/data/lcmm.csv")
m3 <- readRDS("./gor-covid-19/final scripts/models/lcmm/lcmm3.rds")

# ------------------------------------------------------------
# two stage regression
# ------------------------------------------------------------



ext_main1 <- externVar(
  m3,
  classmb = ~ Geslacht + Leeftijd_numeriek +
    Opleidingsniveau + WerkSchool +
    Woonsituatie + CMG_count_bl,
  subject = "ID",
  data    = lcmm_compleet,
  method  = "twoStageJoint", 
  nproc =  ncores
)

summary(ext_main1)


saveRDS(ext_main1, "./gor-covid-19/final scripts/models/lcmm/regression/main/ext_main1.rds")

#sensitivity analysis


ext_sens1 <- externVar(
  m3,
  classmb = ~ Geslacht + Leeftijd_numeriek +
    Opleidingsniveau + WerkSchool +
    Woonsituatie + CMG_Last_count_bl,
  subject = "ID",
  data    = lcmm_compleet,
  method  = "twoStageJoint", 
  nproc = ncores
)

summary(ext_sens1)




saveRDS(ext_sens1, "./gor-covid-19/final scripts/models/lcmm/regression/sensitivity/ext_sens1.rds")


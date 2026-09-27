# ============================================================
# Latent class mixed models (LCMM) on wellbeing factor scores
# ============================================================


rm(list = ls())
setwd("C:/Users/P269380/Documents/GOR covid-19/")

library(tidyverse)
library(lavaan)
library(lcmm)
library(nnet)
library(broom)
library(parallel)
source("./gor-covid-19/final scripts/00_variable_groups.R")
ncores <- max(1, parallel::detectCores(logical = TRUE) - 2)

set.seed(1234)

# Load the data
df <- read_csv("./gor-covid-19/final scripts/data/03_data_long.csv")

df_pred <- read_csv("./gor-covid-19/final scripts/data/04_wb_fct.csv") %>%
  select(-wave)

df <- df %>%
  left_join(df_pred, by = c("Koppelnummer", "Ronde"))


# ============================================================
# 1. Data preparation
# ============================================================

# lcmm requires a numeric subject identifier, and time centred so
# the intercept refers to the first wave
lcmm_dat <- df %>%
  mutate(ID   = as.numeric(as.factor(Koppelnummer)),
         time = Ronde - min(Ronde, na.rm = TRUE)) %>%
  as.data.frame()

set.seed(5)
sample_ids <- sample(unique(lcmm_dat$ID), 30)

ggplot(lcmm_dat %>% filter(ID %in% sample_ids),
       aes(x = time, y = wb_fct, group = ID, colour = factor(ID))) +
  geom_line(alpha = 0.4, linewidth = 0.6) +
  scale_colour_viridis_d(guide = "none") +
  labs(x = "Waves since first measurement",
       y = "Wellbeing factor score") +
  my_theme


# ============================================================
# 2. Estimate models with 1 to 5 classes
#    The one class model provides starting values; models with
#    two or more classes use gridsearch with 100 random
#    initialisations to avoid local maxima.
# ============================================================

m1 <- hlme(wb_fct ~ time +  I(time^2) ,
           random  = ~ 1,
           subject = "ID",
           ng      = 1,
           data    = lcmm_dat,
           maxiter = 1000)

saveRDS(m1, "./gor-covid-19/final scripts/models/lcmm1_randomint.rds")

m2 <- gridsearch(
  rep = 50, maxiter = 30, minit = m1, cl = ncores,
  hlme(wb_fct ~ time +  I(time^2) ,
       mixture = ~ time +  I(time^2),
       random  = ~ 1,
       subject = "ID",
       ng      = 2,
       data    = lcmm_dat)
)

saveRDS(m2, "./gor-covid-19/final scripts/models/lcmm2_randomint.rds")

m3 <- gridsearch(
  rep = 50, maxiter = 30, minit = m1,
  cl      = ncores,
  hlme(wb_fct ~ time +  I(time^2) ,
       mixture = ~ time +  I(time^2),
       random  = ~ 1,
       subject = "ID",
       ng      = 3,
       data    = lcmm_dat)
)

saveRDS(m3, "./gor-covid-19/final scripts/models/lcmm3_randomint.rds")

m4 <- gridsearch(
  rep = 50, maxiter = 30, minit = m1,
  cl      = ncores,
  hlme(wb_fct ~ time +  I(time^2) ,
       mixture = ~ time +  I(time^2) ,
       random  = ~ 1,
       subject = "ID",
       ng      = 4,
       data    = lcmm_dat)
)


saveRDS(m4, "./gor-covid-19/final scripts/models/lcmm4_randomint.rds")

m5 <- gridsearch(
  rep = 50, maxiter = 30, minit = m1,
  cl      = ncores,
  hlme(wb_fct ~ time +  I(time^2) ,
       mixture = ~ time +  I(time^2) ,
       random  = ~ 1,
       subject = "ID",
       ng      = 5,
       data    = lcmm_dat)
)


saveRDS(m5, "./gor-covid-19/final scripts/models/lcmm5_randomint.rds")
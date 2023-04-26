# Author: Mike Ackerman & Kevin See
# Purpose: Run CJS model on Lemhi River -> hydrosystem juvenile capture histories
#
# Created: April 21, 2023
# Last Modified: April 26, 2023

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(marked)

# read in capture histories, nodes of interest, etc.
load("S:/main/data/fish/lem_surv/lem_survival.Rdata")

# adapting some code from https://jamesepaterson.github.io/jamespatersonblog/2020-04-26_introduction_to_CJS.html

# basic CJS (constant survival and detection)... this is nonsensical in our case, just seeing if it runs
cjs.m1 = crm(obs_df$ch[[1]])

# examine model and coefficient estimates
cjs.m1
plogis(cjs.m1$results$beta$Phi)

# process capture history data frame for MARK analysis
?process.data
lemhi_proc = process.data(ch_all)

# create design dataframes for model
?make.design.data
lemhi_ddl = make.design.data(lemhi_proc)

Phi.dot      = list(formula = ~ 1)
Phi.cap      = list(formula = ~ capture_method)
Phi.time     = list(formula = ~ time)
Phi.yr       = list(formula = ~ brood_year)
Phi.cap.time = list(formula = ~ capture_method * time)

p.cap  = list(formula = ~ capture_method)
p.time = list(formula = ~ time)
p.yr   = list(formula = ~ brood_year)

?create.model.list
cml = create.model.list(c("Phi","p")) # makes all possible combinations of those parameter formulas
results = crm.wrapper(cml,
                      data = lemhi_proc,
                      ddl = lemhi_ddl,
                      external = FALSE,
                      accumulate = FALSE,
                      hessian = TRUE)

# view results
results

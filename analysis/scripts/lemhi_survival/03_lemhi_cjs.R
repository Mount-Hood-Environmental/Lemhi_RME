# Author: Mike Ackerman
# Purpose: Run CJS model on Lemhi River -> hydrosystem juvenile capture histories
#
# Created: April 21, 2023
# Last Modified:

# clear environment
rm(list = ls())

# load packages
library(tidyverse)
library(here)
library(marked)

# read in capture histories, nodes of interest, etc.
load("S:/main/data/fish/lem_surv/lem_survival.Rdata")

# basic CJS (constant survival and detection)
cjs.m1 = crm(obs_df$ch[[1]])

# examine model and coefficient estimates
cjs.m1
plogis(cjs.m1$results$beta$Phi)

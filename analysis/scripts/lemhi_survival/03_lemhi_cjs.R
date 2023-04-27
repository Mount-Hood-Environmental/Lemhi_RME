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

# some suggested code for collapsing some sites
chmat = splitCH(ch_all$ch) %>%
  as_tibble()
names(chmat) = nodes_of_interest

chmat = chmat %>%
  mutate(U_TRIBS = LLSPRC + BIGSPC + CANY2C + `18MILC` + BIG8MC + LEEC + HAYDNC,
         U_TRAPS = LEMTRP + HYDTRP) %>%
  # our new capture history nodes
  select(U_TRIBS, U_TRAPS, LLRTP, LLR, GRJ, BLW_GRJ) %>%
  as.matrix() %>%
  collapseCH()

ch_new = ch_all %>%
  mutate(ch = chmat) %>%
  # filter out some errant capture histories
  filter(ch != "000000") %>%
  filter(!grepl(2, ch))

# adapting some code from https://jamesepaterson.github.io/jamespatersonblog/2020-04-26_introduction_to_CJS.html

# basic CJS (constant survival and detection)... this is nonsensical in our case, just seeing if it runs
#cjs.m1 = crm(ch_new)

# examine model and coefficient estimates
#cjs.m1
#plogis(cjs.m1$results$beta$Phi)

# process capture history data frame for MARK analysis
?process.data
lemhi_proc = process.data(ch_new)

# create design dataframes for model
?make.design.data
lemhi_ddl = make.design.data(lemhi_proc)

Phi.dot      = list(formula = ~ 1)
Phi.cap      = list(formula = ~ capture_method)
Phi.time     = list(formula = ~ time)
Phi.by       = list(formula = ~ brood_year)
Phi.cap.time = list(formula = ~ capture_method * time)

p.cap  = list(formula = ~ capture_method)
p.time = list(formula = ~ time)
p.yr   = list(formula = ~ brood_year)

?create.model.list
cml = create.model.list(c("Phi", "p")) # makes all possible combinations of those parameter formulas
results = crm.wrapper(cml,
                      data = lemhi_proc,
                      ddl = lemhi_ddl,
                      external = FALSE,
                      accumulate = FALSE,
                      hessian = TRUE)

# view results
results

# save results
save(results,
     top_mod_results,
     file = "S:/main/data/fish/lem_surv/lem_surv_results.Rdata")

# load, if needed
# load("S:/main/data/fish/lem_surv/lem_surv_results.Rdata")

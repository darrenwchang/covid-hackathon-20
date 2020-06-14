# finding the effects of QE
# Michael Wang
# COVID-19 Policy Hackathon

library(magrittr)
library(tidyverse)

setwd("~/Downloads")
mydata = read.csv("qe2_effects.csv") %>% 
    as_tibble() %>% 
    filter(abs(time) <= 10) %>% 
    mutate(announce = (time > 0))

lm(X1.MO ~ time + announce, mydata) %>% 
    summary() # repeat for every variable
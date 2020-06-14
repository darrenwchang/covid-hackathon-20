<<<<<<< HEAD
<<<<<<< HEAD
library(magrittr)
library(tidyverse)

setwd("~/Downloads")
mydata = read.csv("qe2_effects.csv") %>% 
    as_tibble() %>% 
    filter(abs(time) <= 10) %>% 
    mutate(announce = (time > 0))

lm(X1.MO ~ time + announce, mydata) %>% 
=======
library(magrittr)
library(tidyverse)

setwd("~/Downloads")
mydata = read.csv("qe2_effects.csv") %>% 
    as_tibble() %>% 
    filter(abs(time) <= 10) %>% 
    mutate(announce = (time > 0))

lm(X1.MO ~ time + announce, mydata) %>% 
>>>>>>> 730c04c00e03913d2f7eeee6a6d7b1800210e82d
=======
library(magrittr)
library(tidyverse)

setwd("~/Downloads")
mydata = read.csv("qe2_effects.csv") %>% 
    as_tibble() %>% 
    filter(abs(time) <= 10) %>% 
    mutate(announce = (time > 0))

lm(X1.MO ~ time + announce, mydata) %>% 
>>>>>>> 730c04c00e03913d2f7eeee6a6d7b1800210e82d
    summary() # repeat for every variable
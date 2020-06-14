setwd("~/Downloads")
mydata = read.csv("bonds_sample.csv") %>% 
    as_tibble() %>% 
    select(-beta) %>% 
    rename(beta = beta_2)
​
# merge_dum = data.frame("maturity" = c("DGS10","DGS20","DGS3","DGS30","DGS5","DGS7"), 
    "qe_effect" = c(-0.16982,-0.03818,-0.15636,0,-0.13255,-0.222))

merge_dum = data.frame("maturity" = c("DGS10","DGS20","DGS3","DGS30","DGS5","DGS7"), 
    "qe_effect" = c(-0.0636,-0.0103,-0.3191,0,-0.1194,-0.12))

# mydata = mydata %>% 
    merge(merge_dum) %>% 
    as_tibble() %>% 
    mutate(effect_1 = qe_effect*beta*total_maturity_offering_amt_f / 100)

mydata = mydata %>% 
    merge(merge_dum) %>% 
    as_tibble() %>% 
    mutate(effect_1 = r_m*qe_effect*beta*total_maturity_offering_amt_f / 100)

mydata %>% 
    group_by(city) %>% 
    summarise(effect_1 = sum(effect_1, na.rm=T), 
    tot_sample = sum(total_maturity_offering_amt_f, na.rm=T))
​
mydata = mydata %>% 
    mutate(refin_eligible = (as.numeric(difftime(maturity_date_d,
        settlement_date_d), unit="weeks") > 262.5))

mydata %>% 
    group_by(city) %>% 
    summarise(yoyo = sum(refin_eligible*total_maturity_offering_amt_f, na.rm=T))
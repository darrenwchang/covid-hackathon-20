# COVID-19 Hackathon
# darren chang

## ---- SETUP
library(tidyverse)
library(vroom)
library(tidyquant)

setwd("C:\\Users\\darre\\Documents\\_econ\\covid-hackathon-20")
bonds_all <- vroom("C:\\Users\\darre\\Documents\\_econ\\bondinfo.csv") # read csv
bonds_all <- 
        bonds_all %>% 
        rename(issue_id_i = v1, 
                maturity_id_i = v2, 
                cusip_c = v3,
                coupon_f = v4,
                maturity_date_d = v5,
                settlement_date_d = v6,
                maturity_amount_f = v7,
                series_code_c = v8,
                active_maturity_flag_l = v9,
                coupon_code_c = v10,
                debt_type_c = v11,
                offering_price_f = v12,
                offering_yield_f = v13,
                total_maturity_offering_amt_f = v14,
                tot_mat_amt_outstanding_f = v15,
                tot_mat_amt_outstanding_date_d = v16,
                additional_credit_flag_i = v17,
                addl_credit_schedule_num_i = v18,
                series_c = v19,
                default_flag_i = v20,
                dfrd_int_cnvrsn_date_d = v21,
                put_flag_i = v22,
                optional_call_flag_i = v23,
                call_schedule_number_i = v24,
                redemption_flag_i = v25,
                prtl_redemption_flag_i = v26,
                reoffered_i = v27,
                reoffered_yield_f = v28,
                reoffered_date_d = v29,
                material_event_flag_l = v30,
                capital_purpose_c = v31,
                tax_code_c = v32,
                state_tax_l = v33,
                bank_qualified_i = v34,
                orig_cusip_status_i = v35,
                orig_cusip_type_i = v36,
                prior_cusip_c = v37,
                cusip_change_reason_c = v38,
                cusip_change_date_d = v39,
                project_name_c = v40,
                use_of_proceeds_c = v41,
                security_code_i = v42,
                sink_fund_type_i = v43,
                super_sinker_flag_i = v44,
                registration_type_i = v45,
                average_life_date_d = v46,
                dated_date_d = v47,
                delivery_date_d = v48,
                interest_calc_code_i = v49,
                first_coupon_date_d = v50,
                interest_frequency_i = v51,
                interest_accrual_date_d = v52,
                depository_type_i = v53,
                denomination_amount_f = v54,
                bond_insurance_code_c = v55,
                mtg_insurance_code_c = v56) %>% 
        mutate(maturity_date_d = as.Date(paste(maturity_date_d), 
                format = '%Y%m%d')) %>% 
        mutate(settlement_date_d = as.Date(paste(settlement_date_d), 
                format = '%Y%m%d')) %>% 
        mutate(to_mat_amt_outstanding_date_d = as.Date(paste(tot_mat_amt_outstanding_date_d ), 
                format = '%Y%m%d')) %>% 
        select(project_name_c, security_code_i, issue_id_i, maturity_id_i, cusip_c, coupon_f, 
                maturity_date_d, settlement_date_d, maturity_amount_f,
                coupon_code_c, debt_type_c, offering_price_f, offering_yield_f,
                total_maturity_offering_amt_f, tot_mat_amt_outstanding_f,
                tax_code_c, state_tax_l)

# vroom_write(bonds_all, 'bonds_all_date.csv', 
#                 delim = ',',
#                 col_names = T)

bonds_all <- vroom('bonds_all_date.csv')

#use tidy quant to obtain yield data
yield_tickers <- c('DGS1MO', 'DGS3MO', 'DGS6MO', 'DGS1', 'DGS2', 'DGS3', 'DGS5', 'DGS7', 'DGS10', 'DGS20', 'DGS30')
yield <- tq_get(yield_tickers, get = 'economic.data', from = "1970-01-01")

# find bond maturity date as a year
start_time <- Sys.time()
bonds_all <- 
        bonds_all %>%
                mutate(bond_maturity = as.double(maturity_date_d - settlement_date_d)/365.25) %>%
                select(bond_maturity, everything()) %>% 
                mutate(maturity = case_when(bond_maturity > 25 ~ 'DGS30',
                                        bond_maturity > 15 & bond_maturity < 25 ~ 'DGS20',
                                        bond_maturity > 8.5 & bond_maturity < 15 ~ 'DGS10',
                                        bond_maturity > 6 & bond_maturity < 8.5 ~ 'DGS7',
                                        bond_maturity > 4 & bond_maturity < 6 ~ 'DGS5',
                                        bond_maturity > 2.5 & bond_maturity < 4 ~ 'DGS3',
                                        bond_maturity > 1.5 & bond_maturity < 2.5 ~ 'DGS2',
                                        bond_maturity > 0.75 & bond_maturity < 1.5 ~ 'DGS1',
                                        bond_maturity > 0.375 & bond_maturity < 0.75 ~ 'DGS6MO',
                                        bond_maturity > 0.167 & bond_maturity < 0.375 ~ 'DGS3MO',
                                        bond_maturity < 0.167 ~ 'DGS1MO')) %>% 
                select(maturity, everything()) %>%
                select(-price) %>% 
                inner_join(yield, c('maturity' = 'symbol', 'settlement_date_d' = 'date')) %>% 
                mutate(maturity_year = format(settlement_date_d, '%Y'))
end_time <- Sys.time()
end_time - start_time

# save that data!
# vroom_write(bonds_all, 'bonds_all_date.csv', 
#                 delim = ',',
#                 col_names = T)

# calculate market rate of return
bonds_rm <- 
        bonds_all %>% 
                select(maturity, maturity_year, total_maturity_offering_amt_f, offering_yield_f, offering_price_f, settlement_date_d) %>%
                group_by(maturity, maturity_year) %>% 
                mutate(weight = total_maturity_offering_amt_f*offering_yield_f*offering_price_f/100) %>% 
                summarize(r_m = sum(weight, na.rm = T)/sum(total_maturity_offering_amt_f, na.rm = T))

# calculate beta
start_time <- Sys.time()
bonds_all <- 
        bonds_all %>%
        select(-r_m) %>% 
        inner_join(bonds_rm, by = c('maturity', 'maturity_year')) %>% 
        mutate(beta = (offering_yield_f - price)/(r_m - price)) %>% 
        select(beta, price, offering_yield_f, r_m, everything()) %>% 
        drop_na(beta)
end_time <- Sys.time()
end_time - start_time

bonds_sample <- 
        bonds_all %>% 
                filter(grepl('New York|Allentown|Los Angeles|Detroit|Houston', 
                        project_name_c)) %>%
                filter(maturity_date_d > '2020-06-14')

bonds_sample <- 
        bonds_sample %>% 
                mutate(city = case_when(grepl('New York', project_name_c) ~ 'NYC',
                                        grepl('Allentown', project_name_c) ~ 'ALL', 
                                        grepl('Los Angeles', project_name_c) ~ 'LAX',
                                        grepl('Detroit', project_name_c) ~ 'DET',
                                        grepl('Houston', project_name_c) ~ 'HOU')) %>% 
                group_by(city) %>% 
                mutate(beta_2 = cov(offering_yield_f, r_m)) %>% 
                select(beta_2, city, everything())

bonds_sample %>% 
        group_by(city) %>% 
        summarize(amt = sum(total_maturity_offering_amt_f,
                        na.rm = T),
                        n = n())

vroom_write(bonds_sample, 'bonds_sample.csv', 
                delim = ',',
                col_names = T)

bonds_sample <- vroom('bonds_sample.csv')
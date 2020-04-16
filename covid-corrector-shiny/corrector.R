

library(readr)
library(dplyr)
library(tidyr)
library(stringr)



Corrector <- R6::R6Class("Corrector", public = list(
  
  countries = NULL,
  
  
  initialize = function () {
    private$read_data()
  },
  
  
  new_correction = function (active_country = "China", reference_country = "South Korea", death_cutoff = 3) {
    
    private$death_cutoff <- death_cutoff
    
    private$active_country <- active_country
    private$reference_country <- reference_country
    
    c1_count <- private$cases %>% filter(`Country/Region` == private$active_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist
    c1_deaths <- private$deaths %>% filter(`Country/Region` == private$active_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist
    private$dr_act <- c1_deaths / c1_count
    
    private$d_cases_act <- c1_deaths > death_cutoff
    
    
    c0_count <- private$cases %>% filter(`Country/Region` == private$reference_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist
    c0_deaths <- private$deaths %>% filter(`Country/Region` == private$reference_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist
    private$dr_ref <- c0_deaths / c0_count
    private$d_cases_ref <- c0_deaths > death_cutoff
    
    
    private$demo_act <- private$demo %>% mutate(Age = Age %>% as.integer) %>% filter(`Country or Area` == private$active_country) %>% filter(Year == max(Year), !is.na(Age))
    private$demo_ref <- private$demo %>% mutate(Age = Age %>% as.integer) %>% filter(`Country or Area` == private$reference_country) %>% filter(Year == max(Year), !is.na(Age))
    
    
    # print("private$demo_act")
    # print(private$demo_act)
    # print("private$demo_ref")
    # print(private$demo_ref)
    
    private$vulnerable_act <- private$demo_act %>% 
      mutate(vul = case_when(
        Age < 30 ~ 0,
        Age >= 30 & Age < 40 ~ Value * 0.0011,
        Age >= 40 & Age < 50 ~ Value * 0.0009,
        Age >= 50 & Age < 60 ~ Value * 0.0037,
        Age >= 60 & Age < 70 ~ Value * 0.0151,
        Age >= 70 & Age < 80 ~ Value * 0.0535,
        Age >= 80 ~ Value * 0.1084
      ))
    
    
    private$vulnerable_ref <- private$demo_ref %>% 
      mutate(vul = case_when(
        Age < 30 ~ 0,
        Age >= 30 & Age < 40 ~ Value * 0.0011,
        Age >= 40 & Age < 50 ~ Value * 0.0009,
        Age >= 50 & Age < 60 ~ Value * 0.0037,
        Age >= 60 & Age < 70 ~ Value * 0.0151,
        Age >= 70 & Age < 80 ~ Value * 0.0535,
        Age >= 80 ~ Value * 0.1084
      ))
    
    private$v_act <- sum(private$vulnerable_act$vul) / sum(private$demo_act %>% pull(Value))
    private$v_ref <- sum(private$vulnerable_ref$vul) / sum(private$demo_ref %>% pull(Value))
    
    # print("private$v_act")
    # print(private$v_act)
    # print("private$v_ref")
    # print(private$v_ref)
    
    exp_diff <- private$v_act / private$v_ref
    
    # print("exp_diff")
    # print(exp_diff)
    
    private$scaling_factor <- private$dr_act[private$d_cases_act] / (exp_diff * sum(private$dr_ref[private$d_cases_ref]) / length(private$d_cases_ref))
    
    # print("private$scaling_factor")
    # print(private$scaling_factor)
    
    private$potential_cases <- c1_count[private$d_cases_act] * private$scaling_factor
    # print("private$potential_cases")
    # print(private$potential_cases)
    
  },
  
  
  compare_deathrates = function (active_country = private$active_country, reference_country = private$reference_country) {
    
    ## Here using orphaned functione parameters for function triggering anytime Shiny settings are updated
    
    inner_join(
      tibble(
        dr_act = private$dr_act * 100,
        date = colnames(private$cases)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date
      ) %>% filter(private$d_cases_act),
      tibble(
        dr_ref = private$dr_ref * 100,
        date = colnames(private$cases)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date
      ) %>% filter(private$d_cases_ref),
      by = "date"
    )
    
  },
  
  
  compare_demo = function (active_country = private$active_country, reference_country = private$reference_country) {
    
    ## Here using orphaned functione parameters for function triggering anytime Shiny settings are updated
    
    full_join(
      private$demo_act %>% select(Age, act_value = Value),
      private$demo_ref %>% select(Age, ref_value = Value),
      by = "Age"
    ) %>% replace_na(list(act_value = 0, ref_value = 0))
    
  },
  
  
  compare_vuln_demo = function (active_country = private$active_country, reference_country = private$reference_country) {
    
    ## Here using orphaned functione parameters for function triggering anytime Shiny settings are updated
    
    full_join(
      private$vulnerable_act %>% select(Age, act_vul = vul),
      private$vulnerable_ref %>% select(Age, ref_vul = vul),
      by = "Age"
    ) %>% replace_na(list(act_value = 0, ref_value = 0))
    
  },
  
  
  get_corrected_cases_plot_data = function () {
    
    c_count <- tibble(
      cases = private$cases %>% filter(`Country/Region` == private$active_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist,
      date = colnames(private$cases)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date,
      day = 1:(ncol(private$cases)-4)
    ) %>% filter(private$d_cases_act) %>% mutate(potential_cases = private$potential_cases)
    
  },
  
  
  get_country_plot_data = function (country, case_cutoff = 0) {
    
    c_count <- tibble(
      cases = private$cases %>% filter(`Country/Region` == country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist,
      date = colnames(private$cases)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date,
      day = 1:(ncol(private$cases)-4)
    )
    
    c_count_cutoff <- c_count$cases > case_cutoff
    
    c_deaths <- tibble(
      deaths = private$deaths %>% filter(`Country/Region` == country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist,
      date = colnames(private$deaths)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date,
      day = 1:(ncol(private$deaths)-4)
    )
    c_recovered <- tibble(
      recovered = private$recovered %>% filter(`Country/Region` == country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist,
      date = colnames(private$recovered)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date,
      day = 1:(ncol(private$recovered)-4)
    )
    
    return(
      left_join(c_count[c_count_cutoff, ], c_deaths[c_count_cutoff, ], by = "date") %>% left_join(c_recovered[c_count_cutoff, ], by = "date")
    )
    
  },
  
  
  stat_fix = function (
    death_fix_loc = 5, death_fix_sh = 5, death_fix_sc = 1, death_fix_type = "normal", death_sampling = 1000,
    cases_fix_loc = 5, cases_fix_sh = 5, cases_fix_sc = 1, cases_fix_type = "normal", cases_sampling = 1000
  ) {
    
    if (death_fix_type == "normal") {
      death_fix_prior <- rnorm(death_sampling, death_fix_loc, death_fix_sc)
    } else if (death_fix_type == "gamma") {
      death_fix_prior <- rgamma(death_sampling, death_fix_sh, death_fix_sc)
    }
    
    
    private$death_orig <- tibble(
      value = private$deaths %>% filter(`Country/Region` == private$active_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist,
      day_date = colnames(private$deaths)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date,# as.POSIXct,
      day = 1:(ncol(private$deaths)-4)
    ) %>% filter(value >= private$death_cutoff)
    
    private$corrected_death <- private$death_orig %>% select(day_date, day) %>% bind_cols(outer(private$death_orig$value, death_fix_prior) %>% as_tibble) %>% gather(key = "junk", value = "value", -day_date, -day) %>% select(-junk)
    
    
    
    if (cases_fix_type == "normal") {
      cases_fix_prior <- rnorm(cases_sampling, cases_fix_loc, cases_fix_sc)
    } else if (cases_fix_type == "gamma") {
      cases_fix_prior <- rgamma(cases_sampling, cases_fix_sh, cases_fix_sc)
    }
    
    
    private$cases_orig <- tibble(
      value = private$cases %>% filter(`Country/Region` == private$active_country) %>% group_by(`Country/Region`) %>% select(-c(1:4)) %>% summarise_all(sum) %>% ungroup %>% select(-1) %>% unlist,
      day_date = colnames(private$cases)[-c(1:4)] %>% strptime("%m/%d/%y") %>% as.Date,# as.POSIXct,
      day = 1:(ncol(private$cases)-4)
    ) %>% filter(private$d_cases_act) %>% mutate(potential_cases = private$potential_cases)
    
    
    private$corrected_cases <- private$cases_orig %>% select(day_date, day) %>% bind_cols(outer(private$cases_orig$potential_cases, cases_fix_prior) %>% as_tibble) %>% gather(key = "junk", value = "value", -day_date, -day) %>% select(-junk)
    
    
  },
  
  
  get_cases_stat_fix_plot_data = function () {
    private$corrected_cases %>% group_by(day_date) %>% summarize(value_mean = mean(value), value_sd = sd(value), value_median = median(value), value_025 = quantile(value, 0.025), value_975 = quantile(value, 0.975)) %>% ungroup %>% inner_join(private$cases_orig, by = "day_date")
  },
  
  get_death_stat_fix_plot_data = function () {
    private$corrected_death %>% group_by(day_date) %>% summarize(value_mean = mean(value), value_sd = sd(value), value_median = median(value), value_025 = quantile(value, 0.025), value_975 = quantile(value, 0.975)) %>% ungroup %>% inner_join(private$death_orig, by = "day_date")
  }
  
  
  
  
), private = list(
  
  cases = NULL,
  deaths = NULL,
  recovered = NULL,
  demo = NULL,
  
  death_cutoff = NULL,
  
  active_country = NULL,
  reference_country = NULL,
  dr_act = NULL,
  d_cases_act = NULL,
  dr_ref = NULL,
  d_cases_ref = NULL,
  demo_act = NULL,
  demo_ref = NULL,
  
  vulnerable_act = NULL,
  vulnerable_ref = NULL,
  v_act = NULL,
  v_ref = NULL,
  scaling_factor = NULL,
  potential_cases = NULL,
  
  death_orig = NULL,
  corrected_death = NULL,
  cases_orig = NULL,
  corrected_cases = NULL,
  
  
  read_data = function () {
    
    confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    recovered_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    
    private$cases <- read_csv(confirmed_url) %>% mutate(`Country/Region` = `Country/Region` %>% str_replace("Iran (Islamic Republic of)", "Iran") %>% str_replace("Taiwan*", "Taiwan") %>% str_replace("Cruise Ship", "Diamond Princess") %>% str_replace("Korea, South", "South Korea") %>% str_replace("US", "USA"))
    private$deaths <- read_csv(deaths_url) %>% mutate(`Country/Region` = `Country/Region` %>% str_replace("Iran (Islamic Republic of)", "Iran") %>% str_replace("Taiwan*", "Taiwan") %>% str_replace("Cruise Ship", "Diamond Princess") %>% str_replace("Korea, South", "South Korea") %>% str_replace("US", "USA"))
    private$recovered <- read_csv(recovered_url) %>% mutate(`Country/Region` = `Country/Region` %>% str_replace("Iran (Islamic Republic of)", "Iran") %>% str_replace("Taiwan*", "Taiwan") %>% str_replace("Cruise Ship", "Diamond Princess") %>% str_replace("Korea, South", "South Korea") %>% str_replace("US", "USA"))
    
    self$countries <- private$cases %>% pull(`Country/Region`) %>% unique %>% sort
    
    
    private$demo <- read_csv("./world_demographics.csv", col_types = cols(`Value Footnotes` = col_character())) %>% mutate(`Country or Area` = `Country or Area` %>% str_replace("Viet Nam", "Vietnam") %>% str_replace("United States of America", "USA") %>% str_replace("United Kingdom of Great Britain and Northern Ireland", "United Kingdom") %>% str_replace("Republic of Korea", "South Korea") %>% str_replace("Venezuela (Bolivarian Republic of)", "Venezuela") %>% str_replace("Iran (Islamic Republic of)", "Iran"))
    
  }
))

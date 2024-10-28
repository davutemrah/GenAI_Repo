# Simulate the data 
library(data.table)
library(tidyverse)

select <- dplyr::select
set.seed(123)

# Initiate the data -----------------------------------------------------------
# Create a function that takes in the number of units, the number of periods,
# a treatment effect that will accumulate overtime (tau), and
# cohort periods (in which periods a cohort is treated). 

calc_dep_var <- function(constant, unit_fe, period_fe, treatment_effect, error){
  dep_var = constant + unit_fe + period_fe + treatment_effect + error
}

init_data <- function(num_unit, num_period, treatment_effect, constant, treatment_period){
  end_period = num_period-1 # periods start from 0
  # Fixed Effects ------------------------------------------------
  # unit fixed effects
  unit <- tibble(
    unit = 1:num_unit, 
    unit_fe = rnorm(num_unit, 0, .01),
    # Assign units to  treatment vs  nontreatment
    treatment_group = sample(c(0,1), num_unit, replace = TRUE, prob = c(.88,.12)))
  
  # period fixed effects 
  period <- tibble(
    period = 0:end_period,
    period_fe = rnorm(num_period, 0, .01))
  
  # Trend Break -------------------------------------------------------------
  
  # make main dataset
  # full interaction of unit X period 
  tot_num_obs = num_unit*num_period 
  
  expand_grid(unit = 1:num_unit, period = 0:end_period) %>% 
    left_join(., unit) %>% 
    left_join(., period) %>% 
    # make error term and get treatment indicators and treatment effects
    mutate(error = rnorm(tot_num_obs, 0, .1),
           treat = ifelse(treatment_group ==1 & period >= treatment_period, 1, 0),
           treatment_effect = ifelse(treat == 1, treatment_effect, 0)) %>% 
    # calculate the dependent variable
    mutate(dep_var = calc_dep_var(constant, unit_fe, period_fe, treatment_effect,error))
}

sim_data <- function(...){
  constant = 80
  data <- as.data.table(init_data(num_unit = 30, 
                                  num_period = 30, 
                                  treatment_effect = -.5, 
                                  treatment_period = 20,
                                  constant = constant))
  data[, c('unit_fe', 'error', 'period_fe', 'treatment_effect', 'treatment_group'):=NULL]
  setnames(data, 'dep_var', 'revenue')
  setkeyv(data, c('unit', 'period'))
  setnames(data, 'unit', 'country')
  
  return(data)
}
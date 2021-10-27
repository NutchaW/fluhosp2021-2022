load_fluHosp_truth <- function(
   #                   truth_source = "epidata",
                       target_variable = NULL,
                       as_of = NULL,
                       truth_date = NULL,
  #                     temporal_resolution = "state",
                       locations = NULL){
  # libraries
  library(tidyverse)
  library(lubridate)
  library(purrr)
  # for now default source to epidata
  source('./R/delphi_epidata.R')
  #  specify params
  if(locations=NULL){
    load("./fluhosp_locations.Rdata")
    locs <- c(fluhosp_locations$abbreviation)
  }
  else{
    locs <- locations
  }
  if(truth_date=NULL){
    tdates <- default_dates # have to edit
  }else{
    tdates <- truth_date
  }
  # get data
  list_results <- Epidata$covid_hosp(locs,tdates)[['epidata']] %>%
    lapply(., function(x) data.frame(t(unlist(x)))) %>%
    purrr::map_dfr(., ~ .x %>%
              as.list %>%
              as_tibble)
  # second round of filtering
  if(target_variable=NULL){
    tar <= "previous_day_admission_influenza_confirmed"
  }else{
    tar <- target_variable
  }
  if(as_of=NULL){
    issue_date <-  default_issue_dates # have to edit
  }else{
    issue_date <- as_of
  }
  # have to give an warning if the column does not exist for particular dates & locs or user should just check?
  truth_data <- list_results %>%
    dplyr::filter(issue %in% issue_date) %>%
    dplyr::select(c("state","issue","date",tar))
  return(truth_data)
}

# # have to think about when the date can be set since no influenza columns last year
# res <- Epidata$covid_hosp("MA",c(20200510,20210510))
# check <- lapply(res[[1]], function(x) data.frame(t(unlist(x))))
# setdiff(colnames(check[[1]]),colnames(check[[2]]))
# 
# check2 <- Epidata$covid_hosp("MA",c(20200510,20210510))[['epidata']] %>%
#   lapply(., function(x) data.frame(t(unlist(x)))) %>%
#   map_dfr(., ~ .x %>%
#             as.list %>%
#             as_tibble)



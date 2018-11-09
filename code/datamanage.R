# - - - - - - - - - - - - - - - - - - - - - #
# TITLE: datamanage.R
# Created by:       C.K.
# Created on:       11/07/2018
# Modified by:      C.K.
# Modified on:      11/07/2018
# - - - - - - - - - - - - - - - - - - - - - #


# - - - - - - - - - - - - - - - - - - - - - #
#---- Load Libraries
# - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
library(rio)
dfm <- import_list("../data/master.xls")

library(dplyr)
library("knitr")

# - - - - - - - - - - - - - - - - - - - - - #
#---- rename things
# - - - - - - - - - - - - - - - - - - - - - #
names(dfm) <- c("Demo","Order","Antibody","Pregnancy","TransRxn","Trans")

demname <- c("MRN","FRN","pid","gender","race","age","event","weight","blood_type","pregnancy_status","admit_dt","discharge_dt","death_date","encounter_key")

names(dfm$Demo) <- demname

# Demographic table part 1
#dfm$Demo %>%
    #select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    #mutate(death = ifelse(is.na(death_date), 0, 1)) %>%
    #filter(!is.na(age) & !is.na(discharge_dt)) 

# Demographic table part 2 RBC transfusion and Number of T&S tests

## orders that are completed
names(dfm$Order) <- c("MRN","ORD","ORD_DT","CANCEL_DT","CANCEL_R","CANCEL_P","complete_dt")
dfm$Order %>%
    filter(!is.na(complete_dt)) %>%
    group_by(MRN) %>%
    summarise(n = n())
##
dfm$Antibody %>%
    filter(!is.na(LONG_TEXT_3))%>%
    head

##
dfm$TransRxn %>%
    distinct(MEDICAL_RECORD_NBR, .keep_all = TRUE) 

##
demname <- c("ENC","PER","pid","prod_nb","prod_nm","prod_cl","trans_vol","prod_cat","prod_event","prod_event_dt")
names(dfm$Trans) <- demname
dfm$Trans %>%
    filter(prod_cat == "RBC")  %>%
    group_by(pid) %>%
    summarise(RBCtrans = n()) 

## Antibody
demname <- c("MRN","DESC",paste0("LT_", as.character(1:8)))
names(dfm$Antibody) <- demname
dfm$Antibody %>%
    select(MRN, DESC) %>%
    # Filter if we only want Anti- in DESC column
    #filter(grepl("Anti-", DESC)) %>% 
    mutate(antibody = 1)


# - - - - - - - - - - - - - - - - - - - - - #
# Table 1 data
# - - - - - - - - - - - - - - - - - - - - - #

library("tableone")
library("knitr")
library("labelled")
library("kableExtra")
library("dplyr")


#tab1 <- dfm$Demo %>%
tab1 <- dfm$Demo %>%
    select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    mutate(death = ifelse(is.na(death_date), 0, 1)) %>%
    filter(!is.na(age) & !is.na(discharge_dt)) %>%
    left_join(
              dfm$Trans %>%
                  filter(prod_cat == "RBC")  %>%
                  group_by(pid) %>%
                  summarise(RBCtrans = n()) 
    ) %>%
    mutate(RBCtrans = ifelse(is.na(RBCtrans), 0, RBCtrans)) %>%
    left_join(
              dfm$Order %>%
                  filter(!is.na(complete_dt)) %>%
                  group_by(MRN) %>%
                  summarise(ntest = n())
    ) %>%
    left_join(
              dfm$Antibody %>%
                  select(MRN, DESC) %>%
                  # Filter if we only want Anti- in DESC column
                  #filter(grepl("Anti-", DESC)) %>% 
                  mutate(antibody = 1)
    ) %>%
    filter(gender == "F" | gender == "M") %>%
    mutate(los = as.numeric(difftime(discharge_dt, admit_dt, units="days")),
           ntest = ifelse(is.na(ntest), 0, ntest),
           race = ifelse(race == "Black or African American", "African American", "Other"),
           antibody = ifelse(is.na(antibody), 0, antibody))  %>%
    mutate(antibody = ifelse(antibody == 1, "Anti+","Anti-")) %>%
    set_variable_labels(gender = "Gender", age= "Age (years)",
                        race = "Ethnicity", death = "Mortality",
                        los = "Median length of admission", RBCtrans = "Median RBC transfusion", 
                        ntest = "Median number of type and screen tests performed") %>%
    CreateTableOne(vars = c("age","gender","race","death","los","RBCtrans","ntest"),
                        factorVars = c("gender","death"), strata = c("antibody"), data=.) %>%
    print(., quote = FALSE, noSpace = TRUE, printToggle = FALSE, varLabels = TRUE, nonnormal = c("los","RBCtrans","ntest")) %>%
    kable(., booktabs = TRUE, format="html")  %>%
    gsub(pattern="Mortality = 1", "Mortality", x=.)

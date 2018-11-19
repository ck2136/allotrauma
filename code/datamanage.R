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
# master data
dfm <- import_list("../data/master.xls")
# antibody development data
abd <- import("../data/LS_annotated_corr_DEIDENTIFIED.csv")
# T&S data
tns <- import("../data/TBICU_order_DEIDENTIFIED.csv")
# Transfusion data
transf <- import("../data/TBICU_BLODD_DEIDENTIFIED.csv")


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
names(tns) <- c("MRN","ORD","ORD_DT","CANCEL_DT","CANCEL_R","CANCEL_P","complete_dt","STATUS")
tns %>%
    filter(!is.na(complete_dt)) %>%
    group_by(MRN) %>%
    summarise(n = n())

##
transf %>%
    filter(CONTENT == "RBC")  %>%
    group_by(MRN) %>%
    summarise(RBCtrans = n()) 

## Antibody

#--- patients that have developed antibody during admission
abdmrn <- abd %>% filter(Dev == 1) %>% dplyr::select(MRN, Dev)

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


dfm$Demo %>%
    select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    mutate(death = ifelse(is.na(death_date), 0, 1),
           MRN = as.integer(MRN)) %>%
    filter(!is.na(age) & !is.na(discharge_dt)) %>%
    left_join(
              transf %>%
                  filter(CONTENT == "SPEC PLASMA-MOD")  %>% str
    ) %>%  filter(!is.na(CONTENT))


tab1 <- dfm$Demo %>%
    select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    mutate(death = ifelse(is.na(death_date), 0, 1)) %>%
    filter(!is.na(age) & !is.na(discharge_dt)) %>%
    left_join(
              transf %>%
                  filter(CONTENT == "RBC")  %>%
                  group_by(MRN) %>%
                  summarise(RBCtrans = n()) 
    ) %>%
    left_join(
              transf %>%
                  filter(CONTENT == "SPEC PLASMA-MOD")  %>%
                  group_by(MRN) %>%
                  summarise(Plastrans = n()) 
    ) %>%
    left_join(
              transf %>%
                  filter(CONTENT == "CRYO-TH")  %>%
                  group_by(MRN) %>%
                  summarise(cryotr = n()) 
    ) %>%
    left_join(
              transf %>%
                  filter(CONTENT == "CRYO-TH")  %>%
                  group_by(MRN) %>%
                  summarise(cryotr = n()) 
    ) %>%
    mutate(RBCtrans = ifelse(is.na(RBCtrans), 0, RBCtrans),
           MRN = as.integer(MRN)) %>% 
    left_join(
              tns %>%
                  filter(!is.na(complete_dt)) %>%
                  group_by(MRN) %>%
                  summarise(ntest = n())
    ) %>%
    left_join(
              abdmrn %>%
                  mutate(antibody = ifelse(Dev == 1, 1, 0))
    ) %>%
    filter(gender == "F" | gender == "M") %>%
    mutate(los = as.numeric(difftime(discharge_dt, admit_dt, units="days")),
           ntest = ifelse(is.na(ntest), 0, ntest),
           race = ifelse(race == "Black or African American", "African American", "Other"),
           antibody = ifelse(is.na(antibody), 0, antibody))  %>%
    mutate(antibody = ifelse(antibody == 1, "Anti+","Anti-")) 


tab1 %>%
    set_variable_labels(gender = "Gender", age= "Age (years)",
                        race = "Ethnicity", death = "Mortality",
                        los = "Median length of admission", RBCtrans = "Median RBC transfusion", 
                        ntest = "Median number of type and screen tests performed") %>%
    CreateTableOne(vars = c("age","gender","race","death","los","RBCtrans","ntest"),
                        factorVars = c("gender","death"), strata = c("antibody"), data=.) %>%
    print(., quote = FALSE, noSpace = TRUE, printToggle = FALSE, varLabels = TRUE, nonnormal = c("los","RBCtrans","ntest")) %>%
    kable(., booktabs = TRUE, format="html")  %>%
    gsub(pattern="Mortality = 1", "Mortality", x=.)


# - - - - - - - - - - - - - - - - - - - - - #
# Table 2 
# - - - - - - - - - - - - - - - - - - - - - #

##- A

##- B. Antibody and frequency

dfm$Antibody %>%
    select(MRN, DESC) %>%
    # Antibodies with long name all merged to shorter names 
    mutate(DESC = gsub(pattern="ANTI-([A-Z])", "\\1", toupper(DESC))) %>%
    mutate(DESC = substr(toupper(DESC),1,1)) %>%
    # Sumamrise by Antibody Group
    group_by(DESC) %>% 
    summarise(Frequency = n()) %>% 
    # if we just want those starting with Anti-
    rename(Antibody=DESC) %>% 
    mutate(Antibody = gsub(pattern="([A-Z])","Anti-\\1",Antibody)) %>%
    kable(., booktabs = TRUE, format="latex", align=c("l","c")) %>%
    gsub(pattern="\\[H\\]","\\[!htbp\\]", x=.)


# - - - - - - - - - - - - - - - - - - - - - #
# Table 3 
# - - - - - - - - - - - - - - - - - - - - - #

##- % patients who have had x amount of RBC transfusions that developed antibody during transfusion encounter

expand.grid(`Number of RBC transfusion` = c('0', '1-5','6-10','11-20','>20'), antibody= c('Anti+','Anti-')) %>%
    left_join(
              tab1 %>%
                  mutate('Number of RBC transfusion' = ifelse(RBCtrans == 0, '0', 
                                                              ifelse(RBCtrans < 6 & RBCtrans > 0, '1-5', 
                                                                     ifelse(RBCtrans <11 & RBCtrans >5, '6-10', 
                                                                            ifelse(RBCtrans >10 & RBCtrans < 21, '11-20', '>20'))))) %>%
              dplyr::select(`Number of RBC transfusion`, antibody)  %>%
              group_by(`Number of RBC transfusion`, antibody, add=TRUE) %>%
              #group_by(`Number of RBC transfusion`) %>% head
              summarise(value = n())  
    ) %>% 
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    spread(antibody, value) %>% 
    mutate(`% Developed Antibody During Encounter` = `Anti+`/(`Anti+`+`Anti-`) * 100) %>%
    .[match(c("0","1-5","6-10","11-20",">20"), .$`Number of RBC transfusion`), ] %>%
    dplyr::select(`Number of RBC transfusion`, `% Developed Antibody During Encounter`) %>%
    kable(., booktabs = TRUE, format="latex", align=c("l","c"), row.names=FALSE)   %>%
    kable_styling(position = "center") %>%
    gsub(pattern="\\[H\\]","\\[!htbp\\]", x=.)








# - - - - - - - - - - - - - - - - - - - - - #
# Supplemental Figures
# - - - - - - - - - - - - - - - - - - - - - #

##- Supplemental Antibody frequency by gender and race figure

library("ggplot2")
library("tidyr")


###- by Gender
dfm$Demo %>%
    select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    mutate(death = ifelse(is.na(death_date), 0, 1)) %>%
    filter(!is.na(age) & !is.na(discharge_dt)) %>%
    left_join(
              dfm$Antibody %>%
                  select(MRN, DESC) %>%
                  # Antibodies with long name all merged to shorter names 
                  mutate(DESC = gsub(pattern="ANTI-([A-Z])", "\\1", toupper(DESC))) %>%
                  mutate(DESC = substr(toupper(DESC),1,1)) %>%
                  rename(Antibody=DESC) %>% 
                  mutate(Antibody = gsub(pattern="([A-Z])","Anti-\\1",Antibody)) %>%
                  mutate(Antibodyb = 1)

    ) %>%
    filter((gender == "F" | gender == "M") & Antibodyb == 1 ) %>% 
    mutate(
           race = ifelse(race == "Black or African American", "African American", "Other"),
           gender = ifelse(gender == "F", "Female", "Male")) %>%
    mutate(antibody = ifelse(Antibodyb == 1, "Anti+","Anti-"))  %>%
    dplyr::select(MRN, age, gender, race, Antibody) %>% 
    group_by(Antibody, gender) %>%
    summarise(n = n()) %>% spread(gender, n) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    gather(., gender, value, Female:Male) %>% 
    #ggplot(., aes(Antibody, value)) +
    ggplot(., aes(gender, value)) +
    geom_bar(aes(fill=gender), position = "dodge", stat="identity") +
    #geom_bar(position = "dodge", stat="identity") +
    facet_wrap(~Antibody) +
    #geom_bar(aes(fill=gender), position = "dodge", stat="identity") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


###- by Race
dfm$Demo %>%
    select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    mutate(death = ifelse(is.na(death_date), 0, 1)) %>%
    filter(!is.na(age) & !is.na(discharge_dt)) %>%
    left_join(
              dfm$Antibody %>%
                  select(MRN, DESC) %>%
                  # Antibodies with long name all merged to shorter names 
                  mutate(DESC = gsub(pattern="ANTI-([A-Z])", "\\1", toupper(DESC))) %>%
                  mutate(DESC = substr(toupper(DESC),1,1)) %>%
                  rename(Antibody=DESC) %>% 
                  mutate(Antibody = gsub(pattern="([A-Z])","Anti-\\1",Antibody)) %>%
                  mutate(Antibodyb = 1)

    ) %>%
    filter((gender == "F" | gender == "M") & Antibodyb == 1 ) %>% 
    mutate(
           race = ifelse(race == "Black or African American", "African American", "Other"),
           gender = ifelse(gender == "F", "Female", "Male")) %>%
    mutate(antibody = ifelse(Antibodyb == 1, "Anti+","Anti-"))  %>%
    dplyr::select(MRN, age, gender, race, Antibody) %>% 
    group_by(Antibody, race) %>%
    summarise(n = n()) %>% spread(race, n) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>% 
    gather(., race, value, 2:3) %>% 
    #ggplot(., aes(Antibody, value)) +
    #geom_bar(aes(fill=race), position = "dodge", stat="identity") +
    ggplot(., aes(race, value)) +
    geom_bar(aes(fill=race), position = "dodge", stat="identity") +
    facet_wrap(~Antibody) +
    #geom_bar(aes(fill=race), position = "dodge", stat="identity") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



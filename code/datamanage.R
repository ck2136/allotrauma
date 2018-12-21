# - - - - - - - - - - - - - - - - - - - - - #
# TITLE: datamanage.R
# Created by:       C.K.
# Created on:       11/07/2018
# Modified by:      C.K.
# Modified on: 		2018 Dec 21
# - - - - - - - - - - - - - - - - - - - - - #


# - - - - - - - - - - - - - - - - - - - - - #
#---- Load Libraries
# - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
library(rio)
library(dplyr)
library("knitr")
# master data
dfm <- import_list("../data/raw/master.xls")
# antibody development data
abd <- import("../data/raw/LS_annotated_corr_DEIDENTIFIED.csv")
# T&S data
tns <- import("../data/raw/TBICU_order_DEIDENTIFIED.csv")
# Transfusion data
transf <- import("../data/raw/TBICU_BLODD.xlsx")
# Demographic data
demodf <- import("../data/raw/DEMO_RQ_DEIDENTIFIED.csv")



# - - - - - - - - - - - - - - - - - - - - - #
#---- rename things
# - - - - - - - - - - - - - - - - - - - - - #
names(dfm) <- c("Demo","Order","Antibody","Pregnancy","TransRxn","Trans")
demname <- c("MRN","FRN","pid","gender","race","age","event","weight","blood_type","pregnancy_status","admit_dt","discharge_dt","death_date","encounter_key")
names(dfm$Demo) <- demname
demname <- c("MRN","FRN","gender","race","age","event","weight","blood_type","pregnancy_status","admit_dt","discharge_dt","death_date","encounter_key")
names(demodf) <- demname

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
abdmrn <- abd %>% filter(Dev == 1) %>% dplyr::select(MRN, Dev) %>%
    rename(antibody = Dev)


# - - - - - - - - - - - - - - - - - - - - - #
# Table 1 data
# - - - - - - - - - - - - - - - - - - - - - #

library("tableone")
library("knitr")
library("labelled")
library("kableExtra")
library("dplyr")


# Demographic data
democ <- demodf %>%
    # select relevant variables
    select(MRN, age, gender,race, death_date, admit_dt, discharge_dt) %>%
    # if not dead set as 0
    mutate(
           MRN = as.integer(MRN),
           death_date = as.POSIXct(death_date, format = "%m/%d/%Y %H:%M",tz="UTC"),
           admit_dt = as.POSIXct(admit_dt, format = "%m/%d/%Y %H:%M",tz="UTC"),
           discharge_dt = as.POSIXct(discharge_dt, format="%m/%d/%Y %H:%M",tz="UTC")) %>%
    mutate(
           death = ifelse(is.na(death_date), 0, 1)
    ) %>%
    mutate(lastdate = if_else(is.na(death_date), discharge_dt, if_else(death_date - discharge_dt >= 0,death_date, discharge_dt))) %>%
    # only those with age and discharge date
    filter(!is.na(age) & !is.na(discharge_dt) & gender != "U")




tab1 <- democ %>%
    left_join(
              democ %>%
                  # join the plasma data
                  left_join(
                            transf %>%
                                filter(CONTENT == "RBC") 
                            ) %>%  filter(!is.na(CONTENT)) %>%
                  # make sure Transfusion done between admissions for each MRN
                  dplyr::select(CONTENT, MRN, admit_dt, discharge_dt, lastdate, TIMESTAMP) %>%
                  filter(
                         (TIMESTAMP <= (lastdate + 12*60*60)) & (admit_dt <= TIMESTAMP | (lastdate - 12 *60*60) <= TIMESTAMP)
                         ) %>% 
                  group_by(MRN) %>%
                  summarise(RBCtrans = n())
            ) %>%
mutate(RBCtrans = ifelse(is.na(RBCtrans), 0, RBCtrans)) %>%
    # plasma transfusion
    left_join(
              democ %>%
                  # join the plasma data
                  left_join(
                            transf %>%
                                filter(CONTENT == "SPEC PLASMA-MOD") 
                            ) %>%  filter(!is.na(CONTENT)) %>%
                  # make sure Transfusion done between admissions for each MRN
                  dplyr::select(CONTENT, MRN, admit_dt, discharge_dt, lastdate, TIMESTAMP) %>%
                  filter(
                         (TIMESTAMP <= (lastdate + 12*60*60)) & (admit_dt <= TIMESTAMP | (lastdate - 12 *60*60) <= TIMESTAMP)
                         ) %>% 
                  group_by(MRN) %>%
                  summarise(plasmatrans = n())
    ) %>%
mutate(plasmatrans = ifelse(is.na(plasmatrans), 0, plasmatrans)) %>%
    left_join(
              democ %>%
                  # join the plasma data
                  left_join(
                            transf %>%
                                filter(CONTENT == "CRYO-TH") 
                            ) %>%  filter(!is.na(CONTENT)) %>%
                  # make sure Transfusion done between admissions for each MRN
                  dplyr::select(CONTENT, MRN, admit_dt, discharge_dt, lastdate, TIMESTAMP) %>%
                  filter(
                         (TIMESTAMP <= (lastdate + 12*60*60)) & (admit_dt <= TIMESTAMP | (lastdate - 12 *60*60) <= TIMESTAMP)
                         ) %>% 
                  group_by(MRN) %>%
                  summarise(cryop = n())
    ) %>%
mutate(cryop = ifelse(is.na(cryop), 0, cryop)) %>%
    left_join(
              democ %>%
                  # join the plasma data
                  left_join(
                            transf %>%
                                filter(CONTENT == "PP") 
                            ) %>%  filter(!is.na(CONTENT)) %>%
                  # make sure Transfusion done between admissions for each MRN
                  dplyr::select(CONTENT, MRN, admit_dt, discharge_dt, lastdate, TIMESTAMP) %>%
                  filter(
                         (TIMESTAMP <= (lastdate + 12*60*60)) & (admit_dt <= TIMESTAMP | (lastdate - 12 *60*60) <= TIMESTAMP)
                         ) %>% 
              group_by(MRN) %>%
                  summarise(pp = n())
    ) %>%
mutate(pp = ifelse(is.na(pp), 0, pp)) %>% 
    # add antibody development
    left_join(
              abdmrn
    ) %>% 
mutate(antibody = ifelse(is.na(antibody), 0, 1)) %>%
    # add type & screen test performed
    left_join(
              tns %>%
                  filter(!is.na(STATUS)) %>%
                  group_by(MRN) %>%
                  summarise(ntest = n())
    ) %>% 
    mutate(los = as.numeric(difftime(discharge_dt, admit_dt, units="days")),
           ntest = ifelse(is.na(ntest), 0, ntest),
           race = ifelse(race == "Black or African American", "African American", "Other"),
           antibody = ifelse(is.na(antibody), 0, antibody))  %>%
    mutate(antibody = ifelse(antibody == 1, "Anti+","Anti-")) 




# remember there are duplicate people in the data
demodf %>% distinct(MRN) %>% nrow


# count of neg and pos
antinegcount <- tab1 %>% filter(antibody == "Anti-") %>% nrow
antiposcount <- tab1 %>% filter(antibody == "Anti+") %>% nrow


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

###--- number of patients admitted with history of antibody formation
library('tidyr')

abd %>% 
    select(contains("pre Y=1"),Dev,MRN) %>%
    mutate(preabd = rowSums(.[1:16])) %>% 
    filter(preabd > 0) %>%
    #filter(preabd > 0 & Dev == 0) %>%
    dplyr::select(MRN) %>% nrow

###--- Antibodies present on admission

preadmabd <- abd %>% 
    select(contains("pre Y=1")) %>%
    mutate(preabd = rowSums(.[1:16])) %>% 
    filter(preabd > 0) %>%
    #filter(preabd > 0 & Dev == 0) %>%
    gather(., antibody, value, -preabd) %>% 
    filter(value == 1) %>%
    mutate(antibody = gsub(pattern="(.+) pre Y=1","\\1", antibody)) %>%
    group_by(antibody) %>%
    summarise(Frequency = n()) 


###-- Post Admission Antibody formed

library(tidyr)
postadmabd <- abd  %>%
    # select only those that have actually developed antibody during admission
    filter(Dev == 1) %>% 
    select(contains("during")) %>%
    .[,-1] %>%
    gather(., antibody, value) %>% 
    filter(value == 1) %>%
    group_by(antibody) %>%
    summarise(Frequency = n()) %>%
    mutate(antibody = gsub(pattern="(.+) during Y=1","\\1", antibody))
    
##-

preadmabd %>%
    full_join(postadmabd, by="antibody") %>%
    mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
    rename(Antibody = antibody,
            `Pre Admission` = 2,
            `Post Admission` = 3)


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
# Table 4 Outcome analysis
# - - - - - - - - - - - - - - - - - - - - - #


## Time from Last RBC transfusion to 1st Antibody Development

### Last RbC transfusion times from cohort
library("data.table")
tab4 <- democ %>%
    # join the plasma data
    left_join(
              transf %>%
                  filter(CONTENT == "RBC") 
              ) %>%  filter(!is.na(CONTENT)) %>%
# make sure Transfusion done between admissions for each MRN
    dplyr::select(CONTENT, MRN, admit_dt, discharge_dt, lastdate, TIMESTAMP) %>%
    filter(
       (TIMESTAMP <= (lastdate + 12*60*60)) & (admit_dt <= TIMESTAMP | (lastdate - 12 *60*60) <= TIMESTAMP)
    ) %>% group_by(MRN) %>%
    top_n(n=1) %>% 
    distinct(MRN, .keep_all=TRUE) %>%
    dplyr::select(MRN, TIMESTAMP) %>%
    rename(ttime = TIMESTAMP)


### Antibody Development Time

temp <- abd  %>%
    # select only those that have actually developed antibody during admission
    filter(Dev > 0) %>% 
    select(contains("during"), MRN, `date devo 1`, `date devo 2`, `date devo 3`) %>%
    .[,-1] %>%
    gather(., antibody, value, -MRN, -`date devo 1`, -`date devo 2`,-`date devo 3`) %>% 
    filter(value == 1)  %>%
    mutate(
           `date devo 1` = as.POSIXct(`date devo 1`, format = "%m/%d/%Y"),
           `date devo 2` = as.POSIXct(`date devo 2`, format = "%m/%d/%Y"),
           `date devo 3` = as.POSIXct(`date devo 3`, format="%m/%d/%Y")) %>%
    rename(adt = `date devo 1`) %>%
    dplyr::select(MRN, adt) %>%
    distinct(MRN, .keep_all=TRUE)


ttimetab <- democ %>%
    # join the plasma data
    left_join(
              transf %>%
                  filter(CONTENT == "RBC") 
              ) %>%  filter(!is.na(CONTENT)) %>%
    left_join(
              temp
            ) %>%  
# make sure Transfusion done between admissions for each MRN
    dplyr::select(CONTENT, MRN, admit_dt, discharge_dt, lastdate, TIMESTAMP,adt) %>%
    filter(
       (TIMESTAMP <= (lastdate + 12*60*60)) & (admit_dt <= TIMESTAMP | (lastdate - 12 *60*60) <= TIMESTAMP)
       ) %>% 
    mutate(adtime = as.numeric(adt - TIMESTAMP)/(60*24)) %>%
    filter(is.na(adtime) | adtime > 0) %>% 
    #filter(adtime < 0) %>% 
    group_by(MRN) %>%
    top_n(n=-1) %>% 
    distinct(MRN, .keep_all=TRUE) %>%
    select(MRN, adtime, TIMESTAMP, adt) 

## Some people are excluded let's check
(temp %>% select(MRN) %>% unlist %>% as.vector)[!(temp %>% select(MRN) %>% unlist %>% as.vector) %in% 
    (ttimetab %>% select(MRN) %>% unlist %>% as.vector)]

## Based on the check I've made the people in the above list do not have transfusions dates that are prior to the 1st day of antibody development

## A) Statistics
summary(ttimetab$adtime)




## B) Time from Last RBC transfusion to first of 2 consec negative antibody screen without any RBC transfusion in middle (median, range, stats


## First need patient data of admission and discharge/death date and transfusion date... since we need the test dates for both those that have had a positive test and those that didn't have a positive test we will split the task

## Part 1: Those that didn't have a positive antibody. 
## function to get opposite of %in%
"%ni%" <- Negate("%in%")


### Patient list that didn't develop antibody
noanti <- democ %>%
    filter(MRN %ni% (abd %>%
                     filter(Dev == 1) %>%
                     select(MRN) %>% unlist %>% as.vector)) %>%
    select(MRN) %>%
    unlist %>% as.vector

### Patient list that didn develop antibody
antimrn <- abd %>%
    filter(Dev == 1) %>%
    select(MRN) %>% unlist %>% as.vector


# time Test for Antibody for those that didn't have antibody developed!

neganttbl <- left_join(
      tns %>%
          mutate(
                 ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
                 complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
                 ) %>%
      filter(MRN %in% (noanti)
             )  %>%
      group_by(MRN) %>%
      top_n(-1, ORD_DT) %>% 
      distinct(MRN, .keep_all = TRUE) %>%
      select(MRN, ORD_DT) %>% 
      rename(negtime1 = ORD_DT),

      tns %>%
          mutate(
                 ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
                 complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
                 ) %>%
      filter(MRN %in% (noanti)
             )  %>%
      group_by(MRN) %>%
      # get the top 2 earliest time
      top_n(-2, ORD_DT) %>% 
      # arrange by descending to get the second from the last
      arrange(MRN, desc(ORD_DT)) %>%
      distinct(MRN, .keep_all = TRUE) %>%
      select(MRN, ORD_DT)  %>%
      rename(negtime2 = ORD_DT)
  ) 

### IMPORTANT THE TIMEZONES SHOULD BE THE SAME ACROSS ALL DATAFILES THIS COULD F UP THE CALCULATIONS

negantgroup <- neganttbl %>%
    left_join(
              transf %>%
                  filter(CONTENT == "RBC")  %>%
                  group_by(MRN) %>%
                  arrange(desc(TIMESTAMP))  
    ) %>%
    select(MRN, negtime1,negtime2, TIMESTAMP) %>%
    filter(TIMESTAMP < negtime1) %>%
    group_by(MRN) %>%
    arrange(desc(TIMESTAMP)) %>%
    distinct(MRN, .keep_all = TRUE) %>%
    mutate(
           antipos = 0,
           transToAnti1 = difftime(negtime1, TIMESTAMP, units="days"),
           transToAnti2 = difftime(negtime2, TIMESTAMP, units="days"),
           Ant1Ant2 = difftime(negtime2, negtime1, units="days")
    ) 

## Part 2: Those that did have a positive antibody. 
## Since these people had a antibody developed we need find first 2 consecutive negative antibody tests that occur 
## possible scenarios are 
    # 1) Anti- Anti+ Anti- : should be excluded
    # 2) Anti+ Anti+ Anti- : should be included
    # 3) Anti- Anti+ Anti- : should be excluded
    # 4) Anti- Anti+ Anti+ : should be included

# Since the Antibody development chart only shows the date... we just need to match the date (not the time) 

temp <- tns %>%
    mutate(
           ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
           complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
           ) %>%
    filter(MRN %in% (antimrn)
           )  %>%
    left_join(
              ttimetab %>% 
                  select(adt) %>% 
                  mutate(adt = as.Date(adt))
    )  %>%
    filter(!is.na(adt)) %>%
    mutate(abspos = if_else(adt < as.Date(ORD_DT), 1, 
                            if_else(adt == as.Date(ORD_DT) , 2, 3))) %>%
    # merge in the transfusion dates before antibody development 
    left_join(
              ttimetab %>% 
                  select(TIMESTAMP) %>% 
                  rename(ttime =TIMESTAMP)
    ) %>% 
    filter(ORD_DT > ttime) %>% 
    group_by(MRN) %>%
    arrange(MRN, ORD_DT) %>%
    mutate(absposlead = lead(abspos)) %>%
    select(MRN, ORD_DT, adt, abspos, absposlead, ttime) 


postantgroup <- left_join(
          temp %>%
              group_by(MRN) %>%
              top_n(-1, ORD_DT) %>% 
              distinct(MRN, .keep_all = TRUE) %>%
              select(MRN, ORD_DT) %>% 
              rename(negtime1 = ORD_DT),
          temp %>%
              group_by(MRN) %>%
              top_n(-2, ORD_DT) %>% 
              arrange(MRN, desc(ORD_DT)) %>%
              distinct(MRN, .keep_all = TRUE) %>%
              select(MRN, ORD_DT,ttime)  %>%
              rename(negtime2 = ORD_DT,
              TIMESTAMP = ttime)
        )  %>%
    mutate(
           antipos = 1,
           transToAnti1 = difftime(negtime1, TIMESTAMP, units="days"),
           transToAnti2 = difftime(negtime2, TIMESTAMP, units="days"),
           Ant1Ant2 = difftime(negtime2, negtime1, units="days")
    ) 

tab4bfin <- bind_rows(postantgroup, negantgroup) %>%
    mutate(
           transToAnti1 = as.numeric(transToAnti1),
           transToAnti2 = as.numeric(transToAnti2),
           Ant1Ant2 = as.numeric(Ant1Ant2)
    )

tab4bfin %>% 
    lapply(., summary) 

tab4bfin %>% data.frame %>% head


### IMPORTANT THE TIMEZONES SHOULD BE THE SAME ACROSS ALL DATAFILES THIS COULD F UP THE CALCULATIONS


## Checck for  errors

tab4bfin %>% 
    filter(MRN %in% c(488920,510773,562313))

transf %>%
    filter(MRN %in% c(488920,510773,562313))

abd %>%
    filter(MRN %in% c(488920,510773,562313))

tns %>%
    filter(MRN %in% c(488920,510773,562313))



# - - - - - - - - - - - - - - - - - - - - - #
# Supplemental Figures
# - - - - - - - - - - - - - - - - - - - - - #

##- Supplemental Antibody frequency by gender and race figure

library("ggplot2")
library("tidyr")

democ %>%
    left_join(
              abd %>% 
                  filter(Dev == 1) %>% 
                  select(contains("during"), MRN) %>%
                  .[,-1] %>%
                  gather(., antibody, value, -MRN) %>% 
                  group_by(MRN, antibody) %>%
                  summarise(Frequency = n()) %>%
                  mutate(antibody = gsub(pattern="(.+) during Y=1","\\1", antibody)) %>%
                  mutate(antibody = toupper(antibody)) %>%
                  group_by(MRN, antibody) %>%
                  summarise(Frequency = n())
    )
dfm$Demo %>%
    select(MRN, age, gender,pid, race, death_date, admit_dt, discharge_dt) %>%
    mutate(death = ifelse(is.na(death_date), 0, 1)) %>%
    filter(!is.na(age) & !is.na(discharge_dt)) %>%
    left_join(
              dfm$Antibody %>%
                  rename(MRN = 1, DESC = 2) %>%
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



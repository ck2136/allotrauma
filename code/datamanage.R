# - - - - - - - - - - - - - - - - - - - - - #
# TITLE: datamanage.R
# Created by:       C.K.
# Created on:       11/07/2018
# Modified by:      C.K.
# Modified on: 		2018 Dec 25
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


library("purrr")

# - - - - - - - - - - - - - - - - - - - - - #
#---- Load Custom Functions
# - - - - - - - - - - - - - - - - - - - - - #

myfunction <- function(data){
    data.frame(
               mean = mean(data, na.rm=TRUE),
               q1 = quantile(data,0.25, na.rm=TRUE),
               median = median(data, na.rm=TRUE),
               q3 = quantile(data,0.75, na.rm=TRUE),
               max = max(data, na.rm=TRUE),
               min = min(data, na.rm=TRUE),
               range = max(data, na.rm=TRUE)-min(data, na.rm=TRUE),
               n = length(data)
    )
}

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
           #MRN = as.integer(MRN),
           death_date = as.POSIXct(death_date, format = "%m/%d/%Y %H:%M",tz="UTC"),
           admit_dt = as.POSIXct(admit_dt, format = "%m/%d/%Y %H:%M",tz="UTC"),
           discharge_dt = as.POSIXct(discharge_dt, format="%m/%d/%Y %H:%M",tz="UTC")) %>%
    mutate(
           death = ifelse(is.na(death_date), 0, 1)
    ) %>%
    mutate(lastdate = if_else(is.na(death_date), discharge_dt, if_else(death_date - discharge_dt >= 0,death_date, discharge_dt))) %>%
    # only those with age and discharge date
    filter(!is.na(age) & !is.na(discharge_dt) & gender != "U") 
    #filter(!is.na(age) & !is.na(discharge_dt) & gender != "U") %>% 
    #distinct(MRN, .keep_all = TRUE)

demodf[duplicated(demodf$MRN), ] %>%
    filter(MRN == 3000277)



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
demodf %>% nrow


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

###--- a. Median number of antibody developed during hospitalization among those that have antibody developed
library(tidyr)
tab2aa <- map_dfr(
                  abd %>%
                      select(Dev) ,
                  myfunction
    )

tab2aai <- map_dfr(
                   abd %>%
                       filter(Dev > 0) %>%
                       select(Dev),
                   myfunction
    )

tab2aain <- abd %>%
    filter(Dev > 0) %>%
    nrow

tab2aan <- abd %>% nrow
    

library('tidyr')
###--- b. Number of patients admitted with history of antibody
tab2abn <- abd %>% 
    select(contains("pre Y=1"),Dev,MRN) %>%
    mutate(preabd = rowSums(.[1:16])) %>% 
    filter(preabd > 0) %>%
    #filter(preabd > 0 & Dev == 0) %>%
    dplyr::select(MRN) %>% unlist %>% length
###--- b.i How many developed new antibody?How many new antibodies?

patwanth <- abd %>% 
    select(contains("pre Y=1"),Dev,MRN) %>%
    mutate(preabd = rowSums(.[1:16])) %>% 
    filter(preabd > 0 & Dev > 0) %>%
    #filter(preabd > 0 & Dev == 0) %>%
    dplyr::select(MRN) %>% unlist %>% as.vector


tab2ab <- map_dfr(
        abd %>%
            filter(MRN %in% patwanth) %>% 
            select(Dev) 
        , myfunction
)

###--- c. How many developed new antibody without history of antibody
"%ni%" <- Negate("%in%")
tab2acn <- abd %>%
    filter(MRN %ni% patwanth)  %>%
    nrow

###--- c.i How many new antibodies
tab2ac <- map_dfr(
        abd %>%
            filter(MRN %ni% patwanth & Dev > 0) %>% 
            select(Dev) 
        , myfunction
)


rbind(
      c(Totnum=tab2aan,tab2aa),
      c(Totnum=tab2abn,tab2ab),
      c(Totnum=tab2acn,tab2ac),
      c(Totnum=tab2aain,tab2aai)
) %>% 
    t %>%
    data.frame %>%
    rename("Median # Antibody during H" = 1,
           "New Ant w H" = 2,
           "New Ant w/o H" = 3,
           "New Anti w+w/o H" = 4
    )




##-- B
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
    filter(Dev >= 1) %>% 
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
    # filter out antibody development that is less than 0.5 days before RBC transfusion
    filter(is.na(adtime) | adtime > 0.5) %>% 
    #filter(is.na(adtime) | adtime > 0) %>% 
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
ttimetab %>%
    ungroup %>%
    select(adtime)  %>%
    data.frame %>%
    psych::describe(.) %>%
    str




## B) Time from Last RBC transfusion to first of 2 consec negative antibody screen without any RBC transfusion in middle (median, range, stats

### Essentially this is figuring out for each transfusion time, the first of 2 consecutive Anti- screen without any transfusion in the middle. We also need to exclude any Anti- happeningg within 12 hours of RBC transfusion

### The algorithm is basically looking for each of the transfusion times and seeing if there are screening times between transfusion times. For those without antibody developmnt, we consider each t&s after admission as the 1st/2nd Anti- test


## First need patient data of admission and discharge/death date and transfusion date... since we need the test dates for both those that have had a positive test and those that didn't have a positive test we will split the task

## Part 1: Those that didn't have a positive antibody. 
## function to get opposite of %in%
"%ni%" <- Negate("%in%")


### Patient list that didn't develop antibody
noanti <- democ %>%
    filter(MRN %ni% (abd %>%
                     filter(Dev >= 1) %>%
                     select(MRN) %>% unlist %>% as.vector)) %>%
    select(MRN) %>%
    unlist %>% as.vector

### Patient list that did develop antibody
antimrn <- abd %>%
    filter(Dev >= 1) %>%
    select(MRN) %>% unlist %>% as.vector

### Num of indiividuals that are Anti- 
tab4bpl <- tns %>%
    mutate(
           ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
           complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
           ) %>%
    filter(MRN %in% (noanti)
           )  %>% 
    # join transfusion data to exclude screen times < 12 hours before transfusion
    left_join(
              # use tab4 because already excludes patients without transfusion times
              #tab4
              transf %>%
                  filter(CONTENT == "RBC" & MRN %in% noanti ) %>% 
                  select(MRN, TIMESTAMP) %>%
                  rename(ttime = TIMESTAMP)
              ) %>%
    filter(
           difftime(ORD_DT, ttime, units="days") > 0.5
           #difftime(ORD_DT, TIMESTAMP, units="days") > 0.5
    ) %>% 
    group_by(MRN) %>%
    summarise(n = n()) %>% 
    #nrow # 715 total 
    filter(n > 1) 

### bind transfusion and t&s by MRN and arrange by date/time
tab4b1 <- tns %>%
    mutate(
           ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
           complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
           ) %>%
    filter(MRN %in% (tab4bpl %>% select(MRN) %>% unlist %>% as.vector)) %>%
    select(MRN, ORD_DT) %>%
    mutate(event = 'tns') %>%
    rename(date = 2)  %>%
    bind_rows(
          transf %>%
              filter(MRN %in% (tab4bpl %>% select(MRN) %>% unlist %>% as.vector)) %>%
              select(MRN, TIMESTAMP) %>%
              mutate(event = 'tf') %>%
              rename(date = 2) 
    ) %>%
    group_by(MRN) %>%
    arrange(MRN, date) %>% 
    mutate(cabn = if_else(event == "tf" & lead(event) == "tns" & lead(event,2) == "tns" & difftime(lead(date), date, units="days") > 0.5 & difftime(lead(date,2), lead(date), units="days") > 0.5, 1, 2)) %>%
    mutate(
           time_fabn = lead(date),
           time_sabn = lead(date, 2)
    ) %>%
    mutate(
           transToAnti1 = as.numeric(difftime(time_fabn, date, units = "days")),
           Ant1Ant2 = as.numeric(difftime(time_sabn, time_fabn, units = "days")),
           transToAnti2 = as.numeric(difftime(time_sabn, date, units = "days"))
    ) %>%
    filter(cabn == 1) %>% 
    select(MRN, transToAnti1,transToAnti2,Ant1Ant2 ) %>%
    ungroup()


# Among those with positive antibody
tab4bpl <- tns %>%
    mutate(
           ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
           complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
           ) %>%
    filter(MRN %in% (antimrn)
           )  %>% 
    # join transfusion data to exclude screen times < 12 hours before transfusion
    left_join(
              # use tab4 because already excludes patients without transfusion times
              #tab4
              transf %>%
                  filter(CONTENT == "RBC" & MRN %in% antimrn ) %>% 
                  select(MRN, TIMESTAMP) %>%
                  rename(ttime = TIMESTAMP)
              ) %>%
    filter(
           difftime(ORD_DT, ttime, units="days") > 0.5
           #difftime(ORD_DT, TIMESTAMP, units="days") > 0.5
    ) %>% 
    group_by(MRN) %>%
    summarise(n = n()) %>% 
    #nrow # 715 total 
    filter(n > 1) 

tab4b2 <- tns %>%
    mutate(
           ORD_DT = as.POSIXct(ORD_DT, format = "%m/%d/%Y %H:%M", tz="UTC"),
           complete_dt = as.POSIXct(complete_dt, format = "%m/%d/%Y %H:%M", tz="UTC")
           ) %>%
    filter(MRN %in% (tab4bpl %>% select(MRN) %>% unlist %>% as.vector)) %>%
    select(MRN, ORD_DT) %>%
    mutate(event = 'tns') %>%
    rename(date = 2)  %>%
    bind_rows(
          transf %>%
              filter(MRN %in% (tab4bpl %>% select(MRN) %>% unlist %>% as.vector)) %>%
              select(MRN, TIMESTAMP) %>%
              mutate(event = 'tf') %>%
              rename(date = 2) ,
          temp %>%
              mutate(event = "Anti+") %>%
              rename(date = 2)
    ) %>%
    group_by(MRN) %>%
    arrange(MRN, date) %>% 
    mutate(cabn = if_else(event == "tf" & lead(event) == "tns" & lead(event,2) == "tns" & difftime(lead(date), date, units="days") > 0.5 & difftime(lead(date,2), lead(date), units="days") > 0.5, 1, 2)) %>%
    mutate(
           time_fabn = lead(date),
           time_sabn = lead(date, 2)
    ) %>%
    mutate(
           transToAnti1 = as.numeric(difftime(time_fabn, date, units = "days")),
           Ant1Ant2 = as.numeric(difftime(time_sabn, time_fabn, units = "days")),
           transToAnti2 = as.numeric(difftime(time_sabn, date, units = "days"))
    ) %>%
    filter(cabn == 1) %>% 
    select(MRN, transToAnti1,transToAnti2,Ant1Ant2 ) %>%
    ungroup()



tab4fin <- bind_rows(
                         tab4b1, tab4b2
)



negantgroup %>%
    summary


library("purrr")

myfunction <- function(data){
    data.frame(
               mean = mean(data, na.rm=TRUE),
               q1 = quantile(data,0.25, na.rm=TRUE),
               median = median(data, na.rm=TRUE),
               q3 = quantile(data,0.75, na.rm=TRUE),
               max = max(data, na.rm=TRUE),
               min = min(data, na.rm=TRUE),
               range = max(data, na.rm=TRUE)-min(data, na.rm=TRUE),
               n = length(data) 
    )
}

map_dfr(negantgroup %>% 
            ungroup %>% 
            select(contains("trans"), contains("Ant1")), 
        myfunction) %>% 
    t(.)  %>%
    cbind(. ,
              map_dfr( ttimetab %>%
                        ungroup %>%
                        select(contains("adtime")),
                      myfunction)  %>%
              t(.)
    ) %>%
    data.frame %>%
    dplyr::select(4,1,2,3) %>%
    rename("RBC to FA"= 1,
           "RBC to 1NS" = 2,
           "RBC to 2NS" = 3,
           "Time F/S NS" = 4
    ) %>%
    kable(., 
          sep.cal='',
          booktabs = TRUE,
          digits=c(2,2,2,2)
    )



### IMPORTANT THE TIMEZONES SHOULD BE THE SAME ACROSS ALL DATAFILES THIS COULD F UP THE CALCULATIONS


# - - - - - - - - - - - - - - - - - - - - - #
# Error Check
# - - - - - - - - - - - - - - - - - - - - - #

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



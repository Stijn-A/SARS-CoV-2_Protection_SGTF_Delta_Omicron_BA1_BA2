
# Cohort A Delta - Omicron BA.1
datum_start   <- as.Date("2021-11-22")
datum_tot_met <- as.Date("2022-01-07")
cohort <- "Delta - Omicron BA.1"


# select the cohort
data_teststraten_org <- data_teststraten_lab %>% 
  select.(!Uitslag) %>% 
  filter.(Cohort == cohort) %>% 
  left_join.(data_teststraten_all %>% select(!Afspraak_start), by = "Monsternummer") %>% 
  filter.(Afspraak_start_datum %in% seq(datum_start,datum_tot_met,1))

# flowchart: count number of tests for each filter
totaal1 <- data_teststraten_org %>% distinct(Pseudoniem) %>% tally() %>% mutate(Totaal = "Geteste personen", Nummer = 1)
totaal2 <- data_teststraten_org %>% tally() %>% mutate(Totaal = "Alle testen", Nummer = 2)



source(file = "scripts/02_data_preparation.R") 

source(file = "scripts/02_exclusions.R") 

# select 1 test pp -
source(file = "scripts/02_test_per_person_random.R")

# create flowchart 
source(file = "scripts/flowchart teststraten ve omikron ba2.R")

flowchart_cohortA <- flowchart
data_teststraten_ve_cohort1 <- data_teststraten_ve_selectie %>% filter(cohort == "Delta - Omicron BA.1")
rm(data_teststraten_ve_selectie)
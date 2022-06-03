# Cohort B Omicron BA.1 - Omicron BA.2
datum_start   <- as_date("2022-01-26") 
datum_tot_met <- as_date("2022-03-31")
cohort <- "Omicron BA.1 - Omicron BA.2"


# select the cohort                                                                                    
data_teststraten_org <- data_teststraten_lab %>% 
  select(!Uitslag) %>% 
  filter.(Cohort == cohort) %>% 
  left_join.(data_teststraten_all %>% select(!Afspraak_start), by = "Monsternummer") %>% 
  filter.(Afspraak_start_datum %in% seq(datum_start,datum_tot_met,1))

#data_teststraten_org %>% count(Uitslag, `S result`, Afspraak_start_datum, Laboratorium_naam) %>% view

# voor flowchart: tel testen in studieperiode per stadium opschoning
totaal1 <- data_teststraten_org %>% distinct(Pseudoniem) %>% tally() %>% mutate(Totaal = "Geteste personen", Nummer = 1)
totaal2 <- data_teststraten_org %>% tally() %>% mutate(Totaal = "Alle testen", Nummer = 2)


source(file = "scripts/02_data_preparation.R") 

source(file = "scripts/02_exclusions.R") 

# selecteer 1 test pp -> #VERANDEREN NAAR RANDOM NEG TEST
source(file = "scripts/02_test_per_person_random.R")

# maak flowchart 
source(file = "flowchart teststraten ve omikron ba2.R")

flowchart_cohortB <- flowchart

data_teststraten_ve_cohort2 <- data_teststraten_ve_selectie %>% filter(cohort == "Omicron BA.1 - Omicron BA.2")
rm(data_teststraten_ve_selectie)
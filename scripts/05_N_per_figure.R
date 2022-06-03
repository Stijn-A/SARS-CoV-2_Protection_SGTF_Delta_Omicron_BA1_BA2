# Number of observations Fig. 1A
data_teststraten_lab_c %>% 
  filter(Afspraak_start_datum %in% seq(data_teststraten_lab_c$Afspraak_start_datum %>% min, 
                                       data_teststraten_lab_c$Afspraak_start_datum %>% max,1)) %>% 
  nrow

# Number of observations Fig. 1B
data_kiemsurv %>% 
  filter(`Sample date` %in% seq(data_teststraten_lab_c$Afspraak_start_datum %>% min, 
                                data_teststraten_lab_c$Afspraak_start_datum %>% max,1)) %>% 
  nrow

# Number of observations Fig. 2A
data_teststraten_ve_cohort1 %>%
  filter(Leeftijd >= 18) %>% nrow

# Number of observations Fig. 2B
data_teststraten_ve_cohort2 %>%
  filter(Leeftijd >= 18) %>% nrow

# Number of observations Fig. 3A
sum(
  data_teststraten_ve_cohort1 %>% 
    filter(Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated")) %>% nrow(),
  data_teststraten_ve_cohort1 %>% 
    # Select age group and remove booster
    filter(Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% nrow,
  data_teststraten_ve_cohort1 %>% 
    filter(Leeftijd_breed == "18-29") %>% nrow,
  data_teststraten_ve_cohort1 %>% 
    filter(Leeftijd_breed == "30-59") %>% nrow,
  data_teststraten_ve_cohort1 %>% 
    filter(Leeftijd_breed == "60+") %>% nrow
)

# Number of observations Fig. 3B
sum(
  data_teststraten_ve_cohort2 %>% 
    filter(Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated")) %>% nrow(),
  data_teststraten_ve_cohort2 %>% 
    # Select age group and remove booster
    filter(Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% nrow,
  data_teststraten_ve_cohort2 %>% 
    filter(Leeftijd_breed == "18-29") %>% nrow,
  data_teststraten_ve_cohort2 %>% 
    filter(Leeftijd_breed == "30-59") %>% nrow,
  data_teststraten_ve_cohort2 %>% 
    filter(Leeftijd_breed == "60+") %>% nrow
)

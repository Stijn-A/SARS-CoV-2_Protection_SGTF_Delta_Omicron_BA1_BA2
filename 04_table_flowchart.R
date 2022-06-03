


data_teststraten_lab %>% 
  left_join.(data_teststraten_all %>% select(!Afspraak_start), by = "Monsternummer") %>% 
  mutate.(
    Cohort = case_when.(
      Afspraak_start_datum <= as.Date("2022-01-07") ~ "Delta - Omicron BA.1",
      Afspraak_start_datum >= as.Date("2022-01-26") ~ "Omicron BA.1 - Omicron BA.2",
      TRUE ~ "intermediate"
    )
  ) %>%
  filter.(Afspraak_start_datum %in% 
            seq(as_date("2021-11-22"),as_date("2022-03-31"),1)) %>% 
  count(Cohort) %>% adorn_totals()
flowchart_cohortA
flowchart_cohortB


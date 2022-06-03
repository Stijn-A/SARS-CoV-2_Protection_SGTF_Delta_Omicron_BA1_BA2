# maak tabel 1 - descriptives van selectie VE

tabel1_c <- data_teststraten_ve_cohort1 %>%
  mutate(
    Cohort = "Cohort A", 
    Uitslag_tabel =
           factor(
             case_when(
               Uitslag_S == "Negatief"     ~ "Negative",
               Uitslag_S == "Detected"     ~ "Delta",
               Uitslag_S == "Not detected" ~ "Omicron BA.1",
               TRUE ~ Uitslag_S %>% as.character()
             ),
             levels = c("Negative", "Delta", "Omicron BA.1")
           )) %>%
  bind_rows(
    data_teststraten_ve_cohort2 %>% 
      mutate(
        Cohort = "Cohort B", 
        Uitslag_tabel =
          factor(
            case_when(
              Uitslag_S == "Negatief"     ~ "Negative",
              Uitslag_S == "Detected"     ~ "Omicron BA.2",
              Uitslag_S == "Not detected" ~ "Omicron BA.1",
              TRUE ~ Uitslag_S %>% as.character()
            ),
            levels = c("Negative", "Omicron BA.1", "Omicron BA.2")))
        ) %>% 
  CreateTableOne(
    vars = c("Leeftijd_breed",
             "Geslacht",
             "Immuunstatus_volgorde",
             "Vaccin",
             "Symptoms"),
    strata = c("Cohort", "Uitslag_tabel")
  )

  
# intervallen
tabel1B <- bind_rows(
  data_teststraten_ve_cohort1 %>%
    filter(Vaccstatus %in% 2:3) %>%
    group_by(Vaccstatus, Uitslag_S) %>%
    summarise(
      `0.25` = quantile(Afspraak_start_datum - Vaccinatie_datum_laatste, probs = 0.25),
      `0.5` = quantile(Afspraak_start_datum - Vaccinatie_datum_laatste, probs = 0.5),
      `0.75` = quantile(Afspraak_start_datum - Vaccinatie_datum_laatste, probs = 0.75)
    ) %>%
    #pivot_wider(names_from = Uitslag_S, values_from = interval_vacc) %>%
    mutate(Cohort = "Delta-Omicron BA.1"),
  
  data_teststraten_ve_cohort2 %>%
    filter(Vaccstatus %in% 2:3) %>%
    group_by(Vaccstatus, Uitslag_S) %>%
    summarise(
      #interval_vacc = median(Afspraak_start_datum - Vaccinatie_datum_laatste),
      `0.25` = quantile(Afspraak_start_datum - Vaccinatie_datum_laatste, probs = 0.25),
      `0.5` = quantile(Afspraak_start_datum - Vaccinatie_datum_laatste, probs = 0.5),
      `0.75` = quantile(Afspraak_start_datum - Vaccinatie_datum_laatste, probs = 0.75)
    ) %>%
    #pivot_wider(names_from = Uitslag_S, values_from = interval_vacc) %>%
    mutate(Cohort = "Omicron BA.1-BA.2")
)

data_teststraten_ve_cohort1 %>%
  filter(
    Immuunstatus %in% c(
      "Previous infection,\nprimary vaccination Booster",
      "Previous infection,\nbooster",
      "Previous infection,\nunvaccinated"
    )
  ) %>%
  group_by(Uitslag_S) %>% 
  summarise(
    `0.25` = quantile(
      Afspraak_start_datum - Afspraak_start_datum_previous_inf,
      probs = 0.25
    ),
    `0.5` = quantile(
      Afspraak_start_datum - Afspraak_start_datum_previous_inf,
      probs = 0.5
    ),
    `0.75` = quantile(
      Afspraak_start_datum - Afspraak_start_datum_previous_inf,
      probs = 0.75
    )
  ) %>%
  mutate(Cohort = "Delta-Omicron BA.1")



data_teststraten_ve_cohort2 %>%
  filter(
    Immuunstatus %in% c(
      "Previous infection,\nprimary vaccination Booster",
      "Previous infection,\nbooster",
      "Previous infection,\nunvaccinated"
    )
  ) %>%
  group_by(Uitslag_S) %>% 
  summarise(
    `0.25` = quantile(
      Afspraak_start_datum - Afspraak_start_datum_previous_inf,
      probs = 0.25
    ),
    `0.5` = quantile(
      Afspraak_start_datum - Afspraak_start_datum_previous_inf,
      probs = 0.5
    ),
    `0.75` = quantile(
      Afspraak_start_datum - Afspraak_start_datum_previous_inf,
      probs = 0.75
    )
  ) %>%
  mutate(Cohort = "Omicron BA.1-BA.2")

tabel1_csv <- print(
  tabel1_c,
  showAllLevels = TRUE,
  printToggle = FALSE,
  catDigits = 1
)


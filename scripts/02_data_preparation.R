cohorts_small <-
  c(5, 12, 16, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90)
cohorts_broad <- c(12, 18, 30, 60)

datum_start_vaccinatie <- as_date("2021-01-06")
datum_start_vaccinatie_booster <- as_date("2021-11-18")

function_cut_and_label <- function(x, breaks, sep = "-"){
  breaks_bound <- c(-Inf, breaks, Inf)  
  n <- length(breaks_bound) - 1
  lbls <- str_c(breaks_bound[1:n]  , sep, breaks_bound[2:(n+1)] - 1)
  lbls <- lbls %>% str_replace(sprintf("^-Inf%s", sep), "<=") %>% str_replace(sprintf("%sInf$", sep), "+")
  cut(x, breaks = breaks_bound, right = F, include.lowest = T, labels = lbls)
}


# haal pseudoniemen op van personen met positieve test voor studieperiode, ongeacht lab
eerder_positief <- data_teststraten_all %>%
  filter(
    Afspraak_start_datum < data_teststraten_org$Afspraak_start_datum %>% min(na.rm = T) &
      Uitslag == "Positief" &
      Pseudoniem %in% data_teststraten_org$Pseudoniem &
      !is.na(Pseudoniem)
  ) %>%
  select(
    Pseudoniem,
    Afspraak_start_datum_previous_inf = Afspraak_start_datum,
    Vaccinatie_status_previous_inf = Vaccinatie_status
  ) %>%
  # previous infection before the vaccination start date in the Netherlands are labelled as unvaccinated.
  mutate(
    Vaccinatie_status_previous_inf = if_else(
      Afspraak_start_datum_previous_inf <= datum_start_vaccinatie,
      "Ongevaccineerd",
      Vaccinatie_status_previous_inf %>% as.character()
    )
  ) %>%
  arrange(Afspraak_start_datum_previous_inf) %>%
  group_by(Pseudoniem) %>%
  slice(1) %>%
  ungroup



data_teststraten <- data_teststraten_org %>%
  mutate(
    Vaccinatie_status =
      case_when(
        # paar onmogelijke vaccinatiedatums op onbekend
        Vaccinatie_merk == "JANSS"  &
          Vaccinatie_datum_laatste <= as.Date("2021-04-21") ~ "Onbekend",
        Vaccinatie_merk == "AZ"     &
          Vaccinatie_datum_laatste <= as.Date("2021-02-12") ~ "Onbekend",
        Vaccinatie_merk == "MOD"    &
          Vaccinatie_datum_laatste <= as.Date("2021-01-22") ~ "Onbekend",
        # verder teststraten opschoning vaccinatiestatus volgen
        TRUE ~ Vaccinatie_status %>% as.character()
      )
  ) %>%
  left_join(eerder_positief, by = "Pseudoniem")

data_teststraten <- data_teststraten %>%
  mutate(
    Previous_infection_interval = as.integer(Afspraak_start_datum - Afspraak_start_datum_previous_inf),
    Previous_infection = if_else(
      Previous_infection_interval > 29 &
        # binnen 30 dagen nemen we aan dat het dezelfde episode is
        Pseudoniem %in% eerder_positief$Pseudoniem,
      1,
      0
    ),
    # Zet deze datum naar NA als de test te snel achter elkaar lag (want zelfde episode)
    Afspraak_start_datum_previous_inf = if_else(
      Previous_infection == 1,
      Afspraak_start_datum_previous_inf,
      NA_Date_
    ),
    
    Uitslag01 = if_else(Uitslag == "Positief", 1, 0),
    
    Geslacht1  = factor(if_else(Geslacht == "Man", 1, 0)),
    
    Vaccinatie_merk = factor(
      case_when(
        Vaccinatie_status == "Ongevaccineerd" ~ "Geen",
        Vaccinatie_merk %>% str_detect("COM") ~ "COM",
        TRUE ~    Vaccinatie_merk %>% as.character()
      ),
      levels = c("COM", "MOD", "AZ", "JANSS", "Geen", "UNK")
    ),
    
    Vaccstatus = case_when(
        Vaccinatie_status == "Ongevaccineerd" ~ 0,
        Vaccinatie_status == "Deels"          ~ 1,
        # Individuals with 2-doses, vaccinated after the start of the booster campaign
        # with an unknown status if they recived JANSS as their first dose (variable implemented end of January 2022) and
        # vaccinated with a vaccin used for the booster.
        # we now label them as booster
        Vaccinatie_status == "Volledig" & 
          is.na(Vaccinatie_eerder_janss) &
          Vaccinatie_datum_laatste >= datum_start_vaccinatie_booster & # is 2021-11-18
          #(Vaccinatie_DUFS_EZD - Vaccinatie_datum_laatste) >= 7 &
          Vaccinatie_aantal == 2 &
          Vaccinatie_merk %in% c("COM", "MOD") ~ NaN,
        Vaccinatie_status == "Volledig"       ~ 2,
        Vaccinatie_status == "Booster"        ~ 3
      ),
    Leeftijd5 = function_cut_and_label(Leeftijd,
                                      cohorts_small) %>% fct_explicit_na("Unk"),
    Leeftijd_breed = function_cut_and_label(Leeftijd,
                                            cohorts_broad) %>% fct_explicit_na("Unk"),
    
    Maand = format(as.Date(Afspraak_start_datum), "%m-%Y"),
    Immuunstatus =
      factor(
        case_when(
          Previous_infection == 0 & Vaccstatus == 0 ~ "Naive",
          Previous_infection == 1 &
            Vaccstatus == 0 ~ "Previous infection,\nunvaccinated",
          Previous_infection == 0 &
            Vaccstatus == 2 ~ "Primary vaccination",
          Previous_infection == 1 &
            Vaccstatus == 2 ~ "Previous infection,\nprimary vaccination",
          Previous_infection == 0 & Vaccstatus == 3 ~ "Booster",
          Previous_infection == 1 &
            Vaccstatus == 3 ~ "Previous infection,\nbooster",
          TRUE ~ na_chr
        ),
        levels = c(
          "Naive",
          "Previous infection,\nunvaccinated",
          "Primary vaccination",
          "Previous infection,\nprimary vaccination",
          "Booster",
          "Previous infection,\nbooster"
        )
      ),
    
    # Combineer vacc + laatste infectie
    immunizatie_datum_laatste = case_when(
      # unused statement in current dataset.
      Vaccinatie_datum_laatste > Afspraak_start_datum                  ~ Afspraak_start_datum_previous_inf,
      # Wanneer de vaccinatiedatum na de afspraak ligt, het niet meenemen
      is.na(Afspraak_start_datum_previous_inf)                         ~ Vaccinatie_datum_laatste,
      is.na(Vaccinatie_datum_laatste)                                  ~ Afspraak_start_datum_previous_inf,
      Vaccinatie_datum_laatste >= Afspraak_start_datum_previous_inf    ~ Vaccinatie_datum_laatste,
      # nieuwere vaccinatie
      Afspraak_start_datum_previous_inf > Vaccinatie_datum_laatste     ~ Afspraak_start_datum_previous_inf,
      #Afspraak_start_datum_previous_inf == Vaccinatie_datum_laatste ~ Vaccinatie_datum_laatste,
      TRUE ~ NA_Date_
    ),
    
    immunizatie_datum_interval = as.integer(
      difftime(Afspraak_start_datum, immunizatie_datum_laatste, units = "days")
    ),
    
    immunizatie_datum_interval_groep = immunizatie_datum_interval %>%
      cut(
        breaks = c(7, 60, 120, 180, Inf),
        right = FALSE,
        include.lowest = TRUE,
        labels = c("start - 59", "60 - 119","120 - 179", "180+")
      ),
    
    immunizatie_datum_interval_groep_specifiek = immunizatie_datum_interval %>%
      cut(
        breaks = c(7, 30, 60, 90, 120, 150, 180, 210, Inf),
        right = FALSE,
        include.lowest = TRUE,
        labels = c(
          "start - 29",
          "30 - 59",
          "60 - 89",
          "90 - 119",
          "120 - 149",
          "150 - 179",
          "180 - 209",
          "210+"
        )
      ),
    
    Uitslag_S = case_when.(
      Uitslag == "Negatief" & (`S result` == "Negative" | is.na(`S result`)) ~ "Negatief",
      Uitslag == "Positief" & `S result` == "Detected" ~ "Detected",
      Uitslag == "Positief" & `S result` == "Not detected" ~ "Not detected") %>%
      factor(levels = c("Negatief", "Not detected", "Detected")),
    
    Immuunstatus_volgorde = case_when(
      Immuunstatus == "Previous infection,\nprimary vaccination" &
        Vaccinatie_status_previous_inf == "Ongevaccineerd"                ~ "First infection,\nthen primary vaccination",
      Immuunstatus == "Previous infection,\nprimary vaccination" &
        Vaccinatie_status_previous_inf %in% c("Volledig", "Deels", "Pas") ~ "First start primary vaccination,\nthen infection",
      Immuunstatus == "Previous infection,\nprimary vaccination" &
        Vaccinatie_status_previous_inf == "Onbekend"                      ~ na_chr,
      TRUE ~ Immuunstatus %>% as.character()
    ) %>% factor(
      levels = c(
        "Naive",
        "Previous infection,\nunvaccinated",
        "Primary vaccination",
        "Booster",
        "First start primary vaccination,\nthen infection",
        "First infection,\nthen primary vaccination",
        "Previous infection,\nbooster"
      )
    ),
    Symptoms = case_when(
      Klachten %>% str_detect("Geen klachten") ~ "No symptoms reported",
      Klachten %>% str_detect("~") ~ "Unknown",
      TRUE ~ "Symptoms reported") %>% 
      factor(levels = c("Symptoms reported", "No symptoms reported", "Unknown")),
    
    GGD_werkgebied = GGD_werkgebied %>% relevel(ref = "Veiligheids- en Gezondheidsregio Gelderland-Midden")
  )

rm(function_cut_and_label, cohorts_broad, cohorts_small)

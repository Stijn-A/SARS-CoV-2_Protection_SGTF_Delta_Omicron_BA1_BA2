
data_kiemsurv <- data_kiemsurv_org %>%
  mutate(
    `Sample date` = as.Date(`Datum-monstername`, format = "%d-%m-%Y"),
    `Variant (WGS)` = case_when(
      Clade %>% str_detect("Delta") ~ "Delta",
      Clade == "21K (Omicron)" ~ "Omicron BA.1",
      Clade == "21L (Omicron)" ~ "Omicron BA.2",
      TRUE ~ "Other"
    )
  ) %>%
  filter(
    Monsterstroom %in% c("TESTSTRAAT", "STUDIE") &
      # `Sample date` %in% seq(
      #   data_teststraten_lab$Afspraak_start %>% min %>% as.Date(),
      #   data_teststraten_lab$Afspraak_start %>% max %>% as.Date(),
      #   1) &
      Instituut == "RIVM"
  ) %>% 
  distinct(`CoronIT Id`, .keep_all = T)





tabel_ppv_cohort_1 <- data_teststraten_ve_cohort1 %>%
  left_join.(data_kiemsurv,
             by = c("Monsternummer" = "CoronIT Id")) %>%
  filter(!is.na(`Variant (WGS)`)) %>%
  # Remove wrong WGS-sampleID links
  filter(!(Laboratorium_naam == "Synlab België" & Afspraak_start_datum %in% seq(as_date("2021-12-21"),as_date("2021-12-26"), 1))) %>% 
  mutate(`Variant (WGS)` = `Variant (WGS)` %>% factor(levels = c("Omicron BA.1", "Delta", "Omicron BA.2", "Other"))) %>%
  count(Uitslag_S, `Variant (WGS)`) %>%
  pivot_wider(names_from = `Variant (WGS)`, values_from = n) %>%
  mutate(
    `Omicron BA.1` = `Omicron BA.1` %>% replace_na(0),
    Delta = Delta %>% replace_na(0),
    #  `Omicron BA.2` = `Omicron BA.2`  %>% replace_na(0),
    PPV = if_else(
      Uitslag_S == "Not detected",
      round(`Omicron BA.1` / (`Omicron BA.1` 
                              #+ `Omicron BA.2` 
                              + `Delta`), 2),
      round(`Delta` / (`Omicron BA.1` 
                       #+ `Omicron BA.2` 
                       + `Delta`), 2)
    ),
    Cohort = "Cohort Delta-Omicron BA.1",
  ) %>%
  select(Cohort, `SGTF result` = Uitslag_S, `Omicron BA.1`, Delta, 
         #`Omicron BA.2`, 
         PPV)

tabel_ppv_cohort_2 <- data_teststraten_ve_cohort2 %>%
  left_join.(data_kiemsurv,
             by = c("Monsternummer" = "CoronIT Id")) %>%
  # Remove wrong WGS-sampleID links
  filter(!(Laboratorium_naam == "Synlab België" & Afspraak_start_datum %in% seq(as_date("2021-12-21"),as_date("2021-12-26"), 1))) %>% 
  filter(!is.na(`Variant (WGS)`)) %>%
  mutate(`Variant (WGS)` = `Variant (WGS)` %>% factor(levels = c("Omicron BA.1", "Delta", "Omicron BA.2", "Other"))) %>%
  count(Uitslag_S, `Variant (WGS)`) %>%
  pivot_wider(names_from = `Variant (WGS)`, values_from = n) %>%
  mutate(
    `Omicron BA.2` = `Omicron BA.2`  %>% replace_na(0),
    `Omicron BA.1` = `Omicron BA.1` %>% replace_na(0),
    PPV = if_else(
      Uitslag_S == "Not detected",
      round(`Omicron BA.1` / (`Omicron BA.1` + `Omicron BA.2`), 2),
      round(`Omicron BA.2` / (`Omicron BA.1` + `Omicron BA.2`), 2)
    ),
    Cohort = "Cohort Omicron BA.1-BA.2"
  ) %>%
  select(Cohort, `SGTF result` = Uitslag_S, `Omicron BA.1`,  `Omicron BA.2`, PPV)

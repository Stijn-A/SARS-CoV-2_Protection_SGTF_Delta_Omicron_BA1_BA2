

# overall VE per Immuunstatus, voor 18+
model_ve_overall_sa_cohort1 <- data_teststraten_ve_cohort1 %>%
  filter(Leeftijd >= 18) %>%
  vglm(
    formula = Uitslag_S ~
      Immuunstatus_volgorde
    + ns(Afspraak_start_datum, df = 4)
    + Leeftijd5
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .
  )


OR_overall_sa_cohort1    <-
  model_ve_overall_sa_cohort1 %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_overall_sa_cohort1 <-
  model_ve_overall_sa_cohort1 %>% confint() %>% exp() %>% as.data.frame()

tabel_VE_overall_sa_cohort1 <-
  bind_cols(OR = OR_overall_sa_cohort1, ci = ci_OR_overall_sa_cohort1) %>% as_tibble() %>%
  filter(str_detect(var, "Imm")) %>%
  mutate(
    var = var %>% str_replace(":1", "_Omicron BA.1") %>% str_replace(":2", "_Delta") %>%
      str_remove("Immuunstatus_volgorde"),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1 - `2.5 %`) * 100, digits = 0),
    VE_low  = round((1 - `97.5 %`) * 100, digits = 0)
  ) %>%
  separate(var,
           into = c("Immuunstatus", "Variant"),
           sep = "_") %>%
  mutate(
    Immuunstatus = Immuunstatus %>% factor(levels = data_teststraten_ve$Immuunstatus_volgorde %>% levels),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Delta-Omicron BA.1") %>%
  arrange(Variant, Immuunstatus)


# overall VE per Immuunstatus, voor 18+
model_ve_overall_sa_cohort2 <- data_teststraten_ve_cohort2 %>%
  filter(Leeftijd >= 18) %>%
  vglm(
    formula = Uitslag_S ~
      Immuunstatus_volgorde
    + ns(Afspraak_start_datum, df = 4)
    + Leeftijd5
    + Geslacht1 
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .
  )


OR_overall_sa_cohort2    <-
  model_ve_overall_sa_cohort2 %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_overall_sa_cohort2 <-
  model_ve_overall_sa_cohort2 %>% confint() %>% exp() %>% as.data.frame()

tabel_VE_overall_sa_cohort2 <-
  bind_cols(OR = OR_overall_sa_cohort2, ci = ci_OR_overall_sa_cohort2) %>% as_tibble() %>%
  filter(str_detect(var, "Imm")) %>%
  mutate(
    var = var %>% str_replace(":1", "_Omicron BA.1") %>% str_replace(":2", "_Omicron BA.2") %>%
      str_remove("Immuunstatus_volgorde"),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1 - `2.5 %`) * 100, digits = 0),
    VE_low  = round((1 - `97.5 %`) * 100, digits = 0)
  ) %>%
  separate(var,
           into = c("Immuunstatus", "Variant"),
           sep = "_") %>%
  mutate(
    Immuunstatus = Immuunstatus %>% factor(levels = data_teststraten_ve$Immuunstatus_volgorde %>% levels),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Omicron BA.1-BA.2") %>%
  arrange(Variant, Immuunstatus)

tabel_VE_overall <- tabel_VE_overall_sa_cohort1 %>% bind_rows(tabel_VE_overall_sa_cohort2) %>% 
  mutate(Time_since_event = "Overall") %>% 
  select(Cohort, Variant, Immuunstatus, Time_since_event, VE, VE_low, VE_high, VE_CI)



model_immuunstatus_tijd_cohort1 <- data_teststraten_ve_cohort1 %>%
  filter(Leeftijd >= 18) %>%
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus_volgorde,
      "_",
      immunizatie_datum_interval_groep_specifiek
    ) %>%
      replace_na("Naive") %>% factor() %>% relevel(ref = "Naive")
  ) %>%
  vglm(
    formula = Uitslag_S ~
      Immuunstatus_tijd
    + ns(Afspraak_start_datum, df = 4)
    + Leeftijd5
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .
  )

OR_tijd_cohort1    <-
  model_immuunstatus_tijd_cohort1 %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_tijd_cohort1 <-
  model_immuunstatus_tijd_cohort1 %>% confint() %>% exp() %>% as.data.frame()

tabel_VE_tijd_cohort1 <-
  bind_cols(OR = OR_tijd_cohort1, ci = ci_OR_tijd_cohort1) %>% as_tibble() %>%
  filter(str_detect(var, "Imm")) %>%
  mutate(
    var = var %>% str_replace(":1", "_Omicron BA.1") %>% str_replace(":2", "_Delta") %>%
      str_remove("Immuunstatus_tijd"),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1 - `2.5 %`) * 100, digits = 0),
    VE_low  = round((1 - `97.5 %`) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")")
  ) %>%
  separate(
    var,
    into = c("Immuunstatus", "Time_since_event", "Variant"),
    sep = "_"
  ) %>%
  mutate(
    Immuunstatus = Immuunstatus %>% factor(levels = data_teststraten_ve$Immuunstatus_volgorde %>% levels),
    
    Variant = Variant %>% factor(levels = c("Delta", "Omicron BA.1")),
    # Legenda altijd in deze volgorde
    Cohort = "Delta-Omicron BA.1"
  ) %>%
  arrange(Variant, Immuunstatus, Time_since_event)



model_immuunstatus_tijd_cohort2 <- data_teststraten_ve_cohort2 %>%
  filter(Leeftijd >= 18) %>%
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus_volgorde,
      "_",
      immunizatie_datum_interval_groep_specifiek
    ) %>%
      replace_na("Naive") %>% factor() %>% relevel(ref = "Naive")
  ) %>%
  vglm(
    formula = Uitslag_S ~
      Immuunstatus_tijd
    + ns(Afspraak_start_datum, df = 4)
    + Leeftijd5
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .
  )

OR_tijd_cohort2    <-
  model_immuunstatus_tijd_cohort2 %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_tijd_cohort2 <-
  model_immuunstatus_tijd_cohort2 %>% confint() %>% exp() %>% as.data.frame()

tabel_VE_tijd_cohort2 <-
  bind_cols(OR = OR_tijd_cohort2, ci = ci_OR_tijd_cohort2) %>% as_tibble() %>%
  filter(str_detect(var, "Imm")) %>%
  mutate(
    var = var %>% str_replace(":1", "_Omicron BA.1") %>% str_replace(":2", "_Omicron BA.2") %>%
      str_remove("Immuunstatus_tijd"),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1 - `2.5 %`) * 100, digits = 0),
    VE_low  = round((1 - `97.5 %`) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")")
  ) %>%
  separate(
    var,
    into = c("Immuunstatus", "Time_since_event", "Variant"),
    sep = "_"
  ) %>%
  mutate(
    Immuunstatus = Immuunstatus %>% factor(levels = data_teststraten_ve$Immuunstatus_volgorde %>% levels),
    Variant = Variant %>% factor(levels = c("Omicron BA.2", "Omicron BA.1")),
    # Legenda altijd in deze volgorde
    Cohort = "Omicron BA.1-BA.2"
  ) %>%
  arrange(Variant, Immuunstatus, Time_since_event)

tabel_VE_tijd <- bind_rows(tabel_VE_tijd_cohort1,
                           tabel_VE_tijd_cohort2) %>%
  select(Cohort, Variant, Immuunstatus, Time_since_event, VE, VE_low, VE_high, VE_CI)



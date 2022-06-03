## nu per leeftijdsgroep, groep erbij

model_ve_0_11_time_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_0_11_cohort1 <- data_teststraten_ve_cohort1 %>% 
  # Select age group and previous infected, not vaccinated
  filter(Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_12_17_time_cohort1 <- data_teststraten_ve_cohort1 %>% 
  # Select age group and remove booster
  filter(Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_12_17_cohort1 <- data_teststraten_ve_cohort1 %>% 
  # Select age group and remove booster
  filter(Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_18_29_time_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "18-29") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_18_29_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "18-29") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_30_59_time_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "30-59") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_30_59_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "30-59") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_60_time_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "60+") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)


model_ve_60_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd_breed == "60+") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

OR_perleeftijd_cohort1    <- model_ve_0_11_time_cohort1 %>%
  coef() %>%
  exp() %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  rename("OR" = ".") %>%
  mutate(Leeftijd_breed = "<=11") %>%
  bind_rows(
    model_ve_0_11_cohort1 %>%
      coef() %>%
      exp() %>%
      as.data.frame() %>%
      rownames_to_column(var = "var") %>%
      rename("OR" = ".") %>%
      mutate(Leeftijd_breed = "<=11")
  ) %>% 
  bind_rows(
    model_ve_12_17_time_cohort1 %>%
      coef() %>%
      exp() %>%
      as.data.frame() %>%
      rownames_to_column(var = "var") %>%
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "12-17")
  ) %>%
  bind_rows(
    model_ve_12_17_cohort1 %>%
      coef() %>%
      exp() %>%
      as.data.frame() %>%
      rownames_to_column(var = "var") %>%
      rename("OR" = ".") %>%
      mutate(Leeftijd_breed = "12-17")
  ) %>% 
  bind_rows(
    model_ve_18_29_time_cohort1 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "18-29")
  ) %>%
  bind_rows(
    model_ve_18_29_cohort1 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "18-29")
  ) %>%
  bind_rows(
    model_ve_30_59_time_cohort1 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "30-59")
  ) %>%
  bind_rows(
    model_ve_30_59_cohort1 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "30-59")
  ) %>%
  bind_rows(
    model_ve_60_time_cohort1 %>% 
      coef() %>% exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "60+")
  ) %>% 
  bind_rows(
    model_ve_60_cohort1 %>% 
      coef() %>% exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "60+")
  )

ci_OR_perleeftijd_cohort1 <- model_ve_0_11_time_cohort1 %>% 
  confint() %>% 
  exp() %>% 
  as.data.frame() %>% 
  bind_rows(
    model_ve_0_11_cohort1 %>% 
      confint() %>% 
      exp() %>% 
      as.data.frame()
  ) %>% 
  bind_rows(model_ve_12_17_time_cohort1 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_12_17_cohort1 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_18_29_time_cohort1 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_18_29_cohort1 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_30_59_time_cohort1 %>% 
              confint() %>% exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_30_59_cohort1 %>% 
              confint() %>% exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_60_time_cohort1 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_60_cohort1 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  )

tabel_VE_time_age_cohort1 <- 
  bind_cols(OR = OR_perleeftijd_cohort1, ci = ci_OR_perleeftijd_cohort1) %>% as_tibble() %>%
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
    Immuunstatus = Immuunstatus %>% factor(levels = data_teststraten_ve$Immuunstatus %>% levels),
    Time_since_event = Time_since_event %>% factor(
      levels = c(
        data_teststraten_ve_cohort1$immunizatie_datum_interval_groep %>% levels,
        "Overall"
      )
    ),
    Variant = Variant %>% factor(levels = c("Omicron BA.1", "Delta"))
  )



## nu per leeftijdsgroep, groep erbij
## nu per leeftijdsgroep, groep erbij

model_ve_0_11_time_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_0_11_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_12_17_time_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_12_17_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_18_29_time_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "18-29") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_18_29_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "18-29") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_30_59_time_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "30-59") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)

model_ve_30_59_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "30-59") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

model_ve_60_time_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "60+") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      immunizatie_datum_interval_groep
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
    data    = .)


model_ve_60_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd_breed == "60+") %>% 
  mutate(
    Immuunstatus_tijd = str_c(
      Immuunstatus,
      "_",
      "Overall"
    ) %>%
      factor() %>% relevel(ref = "Naive_Overall")
  ) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus_tijd 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

OR_perleeftijd_cohort2    <- model_ve_0_11_time_cohort2 %>%
  coef() %>%
  exp() %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  rename("OR" = ".") %>%
  mutate(Leeftijd_breed = "<=11") %>%
  bind_rows(
    model_ve_0_11_cohort2 %>%
      coef() %>%
      exp() %>%
      as.data.frame() %>%
      rownames_to_column(var = "var") %>%
      rename("OR" = ".") %>%
      mutate(Leeftijd_breed = "<=11")
  ) %>% 
  bind_rows(
    model_ve_12_17_time_cohort2 %>%
      coef() %>%
      exp() %>%
      as.data.frame() %>%
      rownames_to_column(var = "var") %>%
      rename("OR" = ".") %>% mutate(Leeftijd_breed = "12-17")
  ) %>%
  bind_rows(
    model_ve_12_17_cohort2 %>%
      coef() %>%
      exp() %>%
      as.data.frame() %>%
      rownames_to_column(var = "var") %>%
      rename("OR" = ".") %>%
      mutate(Leeftijd_breed = "12-17")
  ) %>% 
  bind_rows(
    model_ve_18_29_time_cohort2 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "18-29")
  ) %>%
  bind_rows(
    model_ve_18_29_cohort2 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "18-29")
  ) %>%
  bind_rows(
    model_ve_30_59_time_cohort2 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "30-59")
  ) %>%
  bind_rows(
    model_ve_30_59_cohort2 %>% 
      coef() %>% 
      exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "30-59")
  ) %>%
  bind_rows(
    model_ve_60_time_cohort2 %>% 
      coef() %>% exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "60+")
  ) %>% 
  bind_rows(
    model_ve_60_cohort2 %>% 
      coef() %>% exp() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "var") %>% 
      rename("OR" = ".") %>% 
      mutate(Leeftijd_breed = "60+")
  )

ci_OR_perleeftijd_cohort2 <- model_ve_0_11_time_cohort2 %>% 
  confint() %>% 
  exp() %>% 
  as.data.frame() %>% 
  bind_rows(
    model_ve_0_11_cohort2 %>% 
      confint() %>% 
      exp() %>% 
      as.data.frame()
  ) %>% 
  bind_rows(model_ve_12_17_time_cohort2 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_12_17_cohort2 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_18_29_time_cohort2 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_18_29_cohort2 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_30_59_time_cohort2 %>% 
              confint() %>% exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_30_59_cohort2 %>% 
              confint() %>% exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_60_time_cohort2 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  ) %>% 
  bind_rows(model_ve_60_cohort2 %>% 
              confint() %>% 
              exp() %>% 
              as.data.frame() 
  )

tabel_VE_time_age_cohort2 <- 
  bind_cols(OR = OR_perleeftijd_cohort2, ci = ci_OR_perleeftijd_cohort2) %>% as_tibble() %>%
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
      Immuunstatus = Immuunstatus %>% factor(levels = data_teststraten_ve$Immuunstatus %>% levels),
      Time_since_event = Time_since_event %>% factor(
        levels = c(
          data_teststraten_ve_cohort2$immunizatie_datum_interval_groep %>% levels,
          "Overall"
        )
      ),
      Variant = Variant %>% factor(levels = c("Omicron BA.1", "Omicron BA.2")),
    )

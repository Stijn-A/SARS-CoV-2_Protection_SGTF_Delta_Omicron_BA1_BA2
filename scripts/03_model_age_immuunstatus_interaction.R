
model_ve_interactie_Delta_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Uitslag_S != "Not detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Immuunstatus:Leeftijd_breed 
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5,
    family  = binomial(link = "logit"),
    data    = .)

model_ve_interactie_Delta_cohort1_zonder_interacties <- data_teststraten_ve_cohort1 %>% 
  filter(Uitslag_S != "Not detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    #+ Immuunstatus:Leeftijd_breed 
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5,
    family  = binomial(link = "logit"),
    data    = .)


tabel_VE_interactie_Delta_cohort1 <- 
  model_ve_interactie_Delta_cohort1 %>% tidy() %>% 
  filter(str_detect(term, "Immuunstatus")) %>% 
  mutate(
    term = term %>% str_remove("Immuunstatus") %>% str_remove("Leeftijd_breed"),
    low  = estimate - (1.96 * std.error),
    high = estimate + (1.96 * std.error),
    OR = exp(estimate),
    OR_low = exp(low),
    OR_high = exp(high),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1- OR_low) * 100, digits = 0),
    VE_low  = round((1- OR_high) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Delta-Omicron BA.1",
    Variant = "Delta") %>% 
  select(Cohort, Variant, interaction_term = term, VE_CI, p.value)


model_ve_interactie_BA1_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Uitslag_S != "Detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
    Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Immuunstatus:Leeftijd_breed 
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5,
    family  = binomial(link = "logit"),
    data    = .)

model_ve_interactie_BA1_cohort1_zonder_int <- data_teststraten_ve_cohort1 %>% 
  filter(Uitslag_S != "Detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5,
    family  = binomial(link = "logit"),
    data    = .)


tabel_VE_interactie_BA1_cohort1 <- 
  model_ve_interactie_BA1_cohort1 %>% tidy() %>% 
  filter(str_detect(term, "Immuunstatus")) %>% 
  mutate(
    term = term %>% str_remove("Immuunstatus") %>% str_remove("Leeftijd_breed"),
    low  = estimate - (1.96 * std.error),
    high = estimate + (1.96 * std.error),
    OR = exp(estimate),
    OR_low = exp(low),
    OR_high = exp(high),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1- OR_low) * 100, digits = 0),
    VE_low  = round((1- OR_high) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Delta-Omicron BA.1",
    Variant = "Omicron BA.1") %>% 
  select(Cohort, Variant, interaction_term = term, VE_CI, p.value)




model_ve_interactie_BA2_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Uitslag_S != "Not detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Immuunstatus:Leeftijd_breed 
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5,
    family  = binomial(link = "logit"),
    data    = .)

model_ve_interactie_BA2_cohort2_zonder_int <- data_teststraten_ve_cohort2 %>% 
  filter(Uitslag_S != "Not detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5,
    family  = binomial(link = "logit"),
    data    = .)



tabel_VE_interactie_BA2_cohort2 <- 
  model_ve_interactie_BA2_cohort2 %>% tidy() %>% 
  filter(str_detect(term, "Immuunstatus")) %>% 
  mutate(
    term = term %>% str_remove("Immuunstatus") %>% str_remove("Leeftijd_breed"),
    low  = estimate - (1.96 * std.error),
    high = estimate + (1.96 * std.error),
    OR = exp(estimate),
    OR_low = exp(low),
    OR_high = exp(high),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1- OR_low) * 100, digits = 0),
    VE_low  = round((1- OR_high) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Omicron BA.1-BA.2",
    Variant = "Omicron BA.2") %>% 
  select(Cohort, Variant, interaction_term = term, VE_CI, p.value)


model_ve_interactie_BA1_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Uitslag_S != "Detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Immuunstatus:Leeftijd_breed 
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5
    ,
    family  = binomial(link = "logit"),
    data    = .)


model_ve_interactie_BA1_cohort2_zonder_int <- data_teststraten_ve_cohort2 %>% 
  filter(Uitslag_S != "Detected") %>% 
  filter(
    !Leeftijd_breed %in% c("<=11", "12-17") |
      Leeftijd_breed == "<=11" & Immuunstatus %in% c("Naive", "Previous infection,\nunvaccinated") |
      Leeftijd_breed == "12-17" & !str_detect(Immuunstatus, "ooster")) %>% 
  mutate(Leeftijd_breed = fct_relevel(Leeftijd_breed, "18-29")) %>% 
  glm(
    formula = Uitslag_S ~ 
      Immuunstatus 
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd_breed
    + Geslacht1
    + GGD_werkgebied
    + Leeftijd5
    ,
    family  = binomial(link = "logit"),
    data    = .)



tabel_VE_interactie_BA1_cohort2 <- 
  model_ve_interactie_BA1_cohort2 %>% tidy() %>% 
  filter(str_detect(term, "Immuunstatus")) %>% 
  mutate(
    term = term %>% str_remove("Immuunstatus") %>% str_remove("Leeftijd_breed"),
    low  = estimate - (1.96 * std.error),
    high = estimate + (1.96 * std.error),
    OR = exp(estimate),
    OR_low = exp(low),
    OR_high = exp(high),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1- OR_low) * 100, digits = 0),
    VE_low  = round((1- OR_high) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    p.value = round(p.value, digits = 5),
    Cohort = "Omicron BA.1-BA.2",
    Variant = "Omicron BA.1") %>% 
  select(Cohort, Variant, interaction_term = term, VE_CI, p.value)



lrtest(model_ve_interactie_BA1_cohort1_zonder_int,model_ve_interactie_BA1_cohort1)
tabel_VE_interactie_Delta_cohort1 %>% filter(p.value < 0.05)

lrtest(model_ve_interactie_Delta_cohort1_zonder_interacties,model_ve_interactie_Delta_cohort1)
tabel_VE_interactie_BA1_cohort1 %>% filter(p.value < 0.05)

lrtest(model_ve_interactie_BA2_cohort2_zonder_int,model_ve_interactie_BA2_cohort2)
tabel_VE_interactie_BA1_cohort2 %>% filter(p.value < 0.05)

lrtest(model_ve_interactie_BA1_cohort2_zonder_int,model_ve_interactie_BA1_cohort2)
tabel_VE_interactie_BA2_cohort2 %>% filter(p.value < 0.05)

write_xlsx(
  bind_rows(
    tabel_VE_interactie_Delta_cohort1 %>% filter(str_detect(interaction_term, ":")),
    tabel_VE_interactie_BA1_cohort1 %>% filter(str_detect(interaction_term, ":")),
    tabel_VE_interactie_BA1_cohort2 %>% filter(str_detect(interaction_term, ":")),
    tabel_VE_interactie_BA2_cohort2 %>% filter(str_detect(interaction_term, ":"))
  ),
  path = str_c(
    "Scripts/werkmappen/Stijn/VE_omicron_delta/BA2/output/Table_S4_age_interactions_",
    format(now(), format = "%Y%m%d_%H%M"),
    ".xlsx"
  )
)

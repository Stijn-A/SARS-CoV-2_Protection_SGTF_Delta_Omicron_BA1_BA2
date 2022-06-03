# VEs with primairy vaccinated as reference

model_ve_overall_B_cohort1 <- data_teststraten_ve_cohort1 %>% 
  filter(Leeftijd >= 18) %>% 
  mutate(Immuunstatus = fct_relevel(Immuunstatus, "Primary vaccination")) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

OR_overall_B_cohort1    <- model_ve_overall_B_cohort1 %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_overall_B_cohort1 <- model_ve_overall_B_cohort1 %>% confint() %>% exp() %>% as.data.frame()

tabel_VE_overall_B_cohort1 <- bind_cols(OR = OR_overall_B_cohort1, ci = ci_OR_overall_B_cohort1) %>% as_tibble() %>% 
  filter(str_detect(var, "Imm")) %>% 
  mutate(
    var = var %>% str_replace(":1", "_Omicron") %>% str_replace(":2", "_Delta") %>% 
      str_remove("Immuunstatus"),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1- `2.5 %`) * 100, digits = 0),
    VE_low  = round((1- `97.5 %`) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Delta-Omicron BA.1") %>% 
  separate(var, into = c("Immuunstatus", "Variant"),  sep = "_") 

tabel_booster_vs_basis_cohort1 <- tabel_VE_overall_B_cohort1 %>% 
  filter(Immuunstatus %in% c("Booster","Previous infection,\nbooster")) %>%
  select(Cohort,Immuunstatus, Variant, VE_CI)




# overall VE per Immuunstatus_volgorde, voor 12+
model_ve_overall_B_cohort2 <- data_teststraten_ve_cohort2 %>% 
  filter(Leeftijd >= 18) %>% 
  mutate(Immuunstatus = fct_relevel(Immuunstatus, "Primary vaccination")) %>% 
  vglm(
    formula = Uitslag_S ~ 
      Immuunstatus
    + ns(Afspraak_start_datum, df = 4)    
    + Leeftijd5 
    + Geslacht1
    + GGD_werkgebied,
    family  = multinomial(refLevel = 1),
    data    = .)

OR_overall_B_cohort2    <- model_ve_overall_B_cohort2 %>% coef() %>% exp() %>% as.data.frame() %>% rownames_to_column(var = "var") %>% rename("OR" = ".")
ci_OR_overall_B_cohort2 <- model_ve_overall_B_cohort2 %>% confint() %>% exp() %>% as.data.frame()

tabel_VE_overall_B_cohort2 <- bind_cols(OR = OR_overall_B_cohort2, ci = ci_OR_overall_B_cohort2) %>% as_tibble() %>% 
  filter(str_detect(var, "Imm")) %>% 
  mutate(
    var = var %>% str_replace(":1", "_Omicron BA.1") %>% str_replace(":2", "_Omicron BA.2") %>% 
      str_remove("Immuunstatus"),
    VE = round((1 - OR) * 100, digits = 0),
    VE_high = round((1- `2.5 %`) * 100, digits = 0),
    VE_low  = round((1- `97.5 %`) * 100, digits = 0),
    VE_CI = paste0(VE, "% (", VE_low, "-", VE_high, ")"),
    Cohort = "Omicron BA.1-BA.2") %>% 
  separate(var, into = c("Immuunstatus", "Variant"),  sep = "_") 

tabel_booster_vs_basis_cohort2 <- tabel_VE_overall_B_cohort2 %>% 
  filter(Immuunstatus %in% c("Booster", "Previous infection,\nbooster")) %>% 
  select(Cohort,Immuunstatus, Variant, VE_CI)




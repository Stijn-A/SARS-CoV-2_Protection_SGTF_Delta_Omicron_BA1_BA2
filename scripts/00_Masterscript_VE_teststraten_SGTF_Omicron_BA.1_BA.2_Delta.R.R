### Masterscript teststraten VE
### VE SGTF

setwd("/PATH/SARS-CoV-2_Protection_SGTF_Delta_Omicron_BA1_BA2/")

## packages
lapply(c("rlang", "tidyverse","tidytable", "lubridate", "janitor", "cowplot", "VGAM", 
         "splines", "broom", "lmtest", "tableone", "readxl", "writexl"), require, character.only = TRUE)

## Import data
source(file = "scripts/01_import_data.R")

## data cleaning 
### Cohort Delta vs BA.1 ###
source(file = "scripts/02_clean_cohort_A.R")
### Cohort BA.1 vs BA.2 ###
source(file = "scripts/02_clean_cohort_B.R")

## models
# Model for Figure 2
source(file = "scripts/03_model_VE.R") 

# Model for Figure 3
source(file = "scripts/03_model_VE_age.R")

# VE compaired to primary vaccination
source(file = "scripts/03_model_VE_immunestatus_booster_vs_basis.R")

# age - vaccination and previous infection status interaction 
source(file = "scripts/03_model_age_immuunstatus_interaction.R")

# Supplementary analysis
source(file = "scripts/03_model_VE_symptomatic.R") # zonder time since event

## tables and figures
# table 1
source(file = "scripts/04_table_overview.R")

# flowchart S1
source(file = "scripts/04_table_flowchart.R")

# Table S2
source(file = "scripts/04_table_ppv.R")

# Figure 1
source(file = "scripts/05_figure_variants.R") # zonder time since event

# Figure 2
source(file = "scripts/05_figure_VE_time_overall.R") 

# Figure 3
source(file = "scripts/05_figure_VE_time_age_overall.R") 

# Figure S2
source(file = "scripts/05_figure_%SGTF_age_groups.R")

## Save output
PATH_figures <- "output/"

# Figure 1 Cohorts
ggsave(file = str_c(PATH_figures, 'F1_figuur_periodes_data_', format(now(), format = "%Y%m%d_%H%M"), ".pdf"), 
       plot = figuur_periodes_data_7,
       units = "mm",
       width = 180, height = 110)

# Figure 2 (time since event)
ggsave(file = str_c(PATH_figures, 'F2_VE_immuunstatus_tijd_18plus_', format(now(), format = "%Y%m%d_%H%M"), ".pdf"), 
       plot = figuur_VE_tijd_cohorts,
       units = "mm",
       width = 180, height = 220)

# Figure 3 (time since event + age)
ggsave(file = str_c(PATH_figures, 'F3_VE_immuunstatus_age_tijd_', format(now(), format = "%Y%m%d_%H%M"), ".pdf"), 
       plot = figuur_leeftijd_cohorts,
       units = "mm",
       width = 180, height = 220)

ggsave(file = str_c(PATH_figures, 'S2_VE_immuunstatus_leeftijd_overall_', format(now(), format = "%Y%m%d_%H%M"), ".tiff"), 
       plot = figuur_procent_omicron_leeftijd,
       width = 9, height = 6
)

write.xlsx(
  tabel1_csv,
  file = str_c(
    PATH_figures, "tabel1_teststraten_ve_",
    format(now(), format = "%Y%m%d_%H%M"),
    ".xlsx"
  )
)

# Data S1
tabel_VE_overall %>%
  bind_rows(tabel_VE_tijd) %>%
  select(!c(VE, VE_low, VE_high)) %>% 
  left_join(
    tabel_VE_overall_symptoms %>%
      bind_rows(tabel_VE_tijd_symptoms) %>% 
      rename(VE_CI_symptomatic_infection = VE_CI),
    by = c("Cohort", "Variant", "Immuunstatus", "Time_since_event")
  ) %>% 
  mutate(Time_since_event = Time_since_event %>% factor(levels = c("Overall", "start - 29", 
                                                                   "30 - 59", "60 - 89",
                                                                   "90 - 119", "120 - 149",
                                                                   "150 - 179","180 - 209",  
                                                                   "210+"))) %>% 
  arrange(Cohort, Variant, Immuunstatus, Time_since_event) %>% 
  write_xlsx(
    path = str_c(
      PATH_figures, "Table_S2_VE_estimates_",
      format(now(), format = "%Y%m%d_%H%M"),
      ".xlsx"
    )
  )

# Data S2
tabel_VE_time_age_cohort1  %>%
  mutate(Cohort = "Cohort Delta - Omicron BA.1") %>% 
  bind_rows(tabel_VE_time_age_cohort2 %>% 
              mutate(Cohort = "Cohort Omicron BA.1 - BA.2")) %>%
  select(Cohort, Variant, Immuunstatus, Age_group = Leeftijd_breed,Time_since_event, VE_CI) %>% 
  mutate(Age_group = Age_group %>% factor(levels = c("<=11", "12-17", "18-29", "30-59", "60+"))) %>% 
  arrange(Cohort, Variant, Immuunstatus, Age_group, Time_since_event) %>% 
  write_xlsx(
    path = str_c(
      PATH_figures, "Table_S4_age_VE_estimates_",
      format(now(), format = "%Y%m%d_%H%M"),
      ".xlsx"
    )
  )

# Table S2
tabel_booster_vs_basis_cohort1 %>% 
  bind_rows(tabel_booster_vs_basis_cohort2) %>% 
  write_xlsx(
    path = str_c(
      PATH_figures, "Table_S3_VE_estimates_primairy_vacc_reference_",
      format(now(), format = "%Y%m%d_%H%M"),
      ".xlsx"
    )
  )

# data S6
write_xlsx(
  list(
    `Fig. 1A` = data_teststraten_lab_c %>% rename(`Sample date` = Afspraak_start_datum) %>% 
      count(`Sample date`, `S result`, name = "Counts"),
    `Fig. 1B` = data_kiemsurv %>% filter(`Sample date` %in% seq(data_teststraten_lab_c$Afspraak_start_datum %>% min, 
                                                                data_teststraten_lab_c$Afspraak_start_datum %>% max,1)) %>% 
      count(`Sample date`, `Variant (WGS)`, name = "Counts")
  ),
  path = str_c(
    PATH_figures, "Date_S4_",
    format(now(), format = "%Y%m%d_%H%M"),
    ".xlsx"
  )
)

tabel_ppv_cohort_1 %>%
  bind_rows(tabel_ppv_cohort_2) %>%
  mutate(Delta  = Delta %>% replace_na(0)) %>%
  write_xlsx(
    str_c(
      "output/S2_PPV_table_",
      format(now(), format = "%Y%m%d_%H%M"),
      ".xlsx"
    )
  )

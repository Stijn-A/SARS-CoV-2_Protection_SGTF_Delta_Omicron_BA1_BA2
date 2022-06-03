

# stukje opschonen
#cohorts_breed <- c(12, 30, 60)


tabel_S_dropout_age_tijd <- data_teststraten_lab %>% 
  mutate(Afspraak_start_datum = as.Date(Afspraak_start)) %>% 
  filter(`S result` %in% c("Detected", "Not detected") & 
           Afspraak_start_datum %in% 
           seq(as_date("2021-11-22"),as_date("2022-03-31"),1)) %>% 
  left_join(data_teststraten_all %>% select(Monsternummer, Leeftijd)) %>% 
  mutate(Leeftijd_breed = functie_cut_and_label(Leeftijd,
                                                cohorts_breed,
                                                right = FALSE, sep = "-") %>% fct_explicit_na("Onbekend"),
  ) %>%
  count(Leeftijd_breed, Afspraak_start_datum,`S result`) %>% 
  group_by(Leeftijd_breed, Afspraak_start_datum) %>% 
    mutate(
      `% SGTF` = n / sum(n) * 100,
      total = sum(n)
    ) %>% 
  ungroup %>% 
  filter(`S result` == "Not detected")

figuur_procent_omicron_leeftijd <- tabel_S_dropout_age_tijd %>% 
  filter(!Leeftijd_breed == "Onbekend") %>% 
  rename(Date = Afspraak_start_datum, Age = Leeftijd_breed) %>% 
  ggplot(aes(x = Date, y = `% SGTF`, color = Age)) + 
  geom_point() +
  geom_line() +
  theme_minimal() +
  xlab("")+
  scale_x_date(breaks = seq(as_date("2021-11-22"),as_date("2022-03-31"), 7),
               expand = expansion(add = 1),) +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0,100),
                     expand = expansion(add = 0.2),) +
  scale_color_brewer(type = "qual",
                     palette = 2) +
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))


# N is:
sum(tabel_S_dropout_age_tijd$total )

# voor overall percentage:
tabel_S_dropout_tijd <- data_teststraten_lab %>% 
  mutate(Afspraak_start_datum = as.Date(Afspraak_start)) %>% 
  filter(`S result` %in% c("Detected", "Not detected") & 
           Afspraak_start_datum %in% 
           seq(as_date("2021-11-22"),as_date("2022-03-31"),1)) %>% 
  count(Afspraak_start_datum,`S result`) %>% 
  pivot_wider(names_from =`S result`, values_from = n ) %>% 
  mutate(
    `% SGTF` = `Not detected` / (`Not detected` + `Detected`) * 100
  ) 


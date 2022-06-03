

positief_studieperiode <- data_teststraten_ve %>%
  filter(Uitslag == "Positief") %>% # neem alleen test uit de huidige studie periode mee
  pull(Pseudoniem)

data_teststraten_ve_omi_pos <- data_teststraten_ve %>%
  # van iedereen die ooit positief was in studeiperiode
  filter(Pseudoniem %in% positief_studieperiode) %>%
  # tellen we alleen de positieve uitslag
  filter(Uitslag == "Positief") %>%
  # indien meerdere positieve testen, alleen de eerste tellen
  arrange(Afspraak_start_datum) %>%
  distinct(Pseudoniem, .keep_all = TRUE)

set.seed(10)
data_teststraten_ve_omi_neg <- data_teststraten_ve %>%
  # van iedereen die nooit positief was in studieperiode # SA2, neem alle neg testen mee
  filter(!Pseudoniem %in% positief_studieperiode) %>%
  # #take random negative result
  sample_n(# set size is equal to tibble length
    size = data_teststraten_ve %>% filter(!Pseudoniem %in% positief_studieperiode) %>% nrow) %>%
  distinct(Pseudoniem, .keep_all = TRUE)

# plak weer aan elkaar
data_teststraten_ve_selectie <- data_teststraten_ve_omi_pos %>%
  bind_rows(data_teststraten_ve_omi_neg)

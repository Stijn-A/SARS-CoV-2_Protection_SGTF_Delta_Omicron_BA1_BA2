
totaal9 <- data_teststraten_ve_selectie %>% tally() %>% mutate(Totaal = "Na selectie 1 test pp", Nummer = 9)

flowchart <- bind_rows(totaal1, totaal2, totaal3,
                       totaal4, totaal5, totaal6, totaal7,
                       totaal9) %>% 
  mutate(Geexcludeerd = if_else(
    Nummer > 2, 
    lag(n) - n,
    na_int
  ))

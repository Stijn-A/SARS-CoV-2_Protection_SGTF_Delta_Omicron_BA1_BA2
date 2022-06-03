

data_teststraten_ve <- data_teststraten %>%
  filter(!is.na(Immuunstatus_volgorde)) %>% # changed from Immuunstatus: individuals with a previous infection with unknown vacc status
  filter(!(Vaccinatie_aantal == 0 & !is.na(Vaccinatie_datum_laatste))) # person having a vaccination date but zero vaccines.

totaal3  <- data_teststraten_ve %>% tally() %>% mutate(Totaal = "exclusie immuunstatus onbekend of deels", Nummer = 3)

data_teststraten_ve <- data_teststraten_ve %>%
  filter(Uitslag %in% c("Positief", "Negatief") ) %>% 
  filter(!is.na(Uitslag_S))

totaal4 <- data_teststraten_ve %>% tally() %>% mutate(Totaal = "Exclusie uitslag onbeoordeelbaar of Ct >30", Nummer = 4)

data_teststraten_ve <- data_teststraten_ve %>%
  filter(!(Leeftijdsgroep == "Niet vermeld" |
           Geslacht == "Niet vermeld"))

totaal5 <- data_teststraten_ve %>% tally() %>% mutate(Totaal = "exclusie leeftijd of geslacht onbekend", Nummer = 5)


# mensen die net voor de studieperiode negatief testen ook eruit, zij tellen niet als herinfecties.
data_teststraten_ve <- data_teststraten_ve %>% 
  filter(Previous_infection_interval > 30 | is.na(Previous_infection_interval))

totaal6 <- data_teststraten_ve %>% tally() %>% mutate(Totaal = "exclusie positieve test binnen 30 dagen voor huidige test", Nummer = 6)

# Geen conformatietesten.
data_teststraten_ve <- data_teststraten_ve %>% 
  filter(Na_Zelftest == "Nee")

totaal7 <- data_teststraten_ve %>% tally() %>% mutate(Totaal = "exclusie confirmatietesten", Nummer = 7)

# maak backup data including kinderen <12
# data_teststraten_ve_kids <- data_teststraten_ve
# 
# data_teststraten_ve <- data_teststraten_ve %>%
#   filter(Leeftijdsgroep != "0-11") 
# 
# totaal8 <- data_teststraten_ve %>% tally() %>% mutate(Totaal = "exclusie leeftijd <12", Nummer = 8)


data_teststraten_ve <- data_teststraten_ve %>%
  mutate(Leeftijdsgroep = Leeftijdsgroep %>% 
           fct_drop(),
         Immuunstatus = Immuunstatus %>% 
           fct_drop(),
         Vaccin = Vaccinatie_merk %>% 
           fct_drop(),
         Leeftijd5 = Leeftijd5 %>% 
           fct_drop())

# data_teststraten_ve_kids <- data_teststraten_ve_kids %>%
#   mutate(Leeftijdsgroep = Leeftijdsgroep %>% 
#            fct_drop(),
#          Immuunstatus = Immuunstatus %>% 
#            fct_drop(),
#          Vaccin = Vaccinatie_merk %>% 
#            fct_drop(),
#          Leeftijd5 = Leeftijd5 %>% 
#            fct_drop())



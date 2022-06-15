message("Import data")
# Import 'raw' data 
# custom data import function (only usefull within rivm), use readRDS readFST, etc.
source(file = "Scripts/01_Initialiseren/functies/functie_laad_data.R") 

# SGTF result from test
data_teststraten_lab <-
  functie_laad_data(loc = "/PATH/data/",
                    datum = as_date("2022-04-12"))

# CoronIT data 
data_teststraten_all <-
  functie_laad_data("/PATH/data/", as_date("2022-04-12"))

# kiemsurveillance data (WGS)
data_kiemsurv_org <-
  "/PATH/data/" %>%
  list.files(full.names = T) %>%
  str_subset("Lijst_voor_Epi") %>%
  sort() %>%
  last() %>%
  read_xlsx()

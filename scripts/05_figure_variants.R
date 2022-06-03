# Figure S1
data_teststraten_lab_c <- data_teststraten_lab %>%
  left_join.(data_teststraten_all %>% select(!Afspraak_start), by = "Monsternummer") %>%
  mutate(`S result` = `S result` %>% factor(levels = c("Not detected", "Detected"))) %>% 
  filter(`S result` %in% c("Not detected", "Detected"))


plot_1 <- ggplot() +
  geom_bar(data = data_teststraten_lab_c,
           aes(x = Afspraak_start_datum,
               fill = `S result`), width = 0.9
           ) +
  geom_rect(
    mapping = aes(linetype = "Delta - Omicron BA.1"),
    xmin = data_teststraten_ve_cohort1$Afspraak_start_datum %>% min - 0.7,
    xmax = data_teststraten_ve_cohort1$Afspraak_start_datum %>% max + 0.5 ,
    ymin = -20,
    ymax = 20000,
    col = "black",
    fill = NA
  ) +
  geom_rect(
    mapping = aes(linetype = "Omicron BA.1 - BA.2"),
    xmin = data_teststraten_ve_cohort2$Afspraak_start_datum %>% min - 0.5,
    xmax = data_teststraten_ve_cohort2$Afspraak_start_datum %>% max + 0.7 ,
    ymin = -20,
    ymax = 20000,
    col = "black",
    fill = NA
  ) +
  scale_colour_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  scale_linetype_manual(values = c(3,4), name = "Cohort") +
  scale_x_date(breaks = seq(data_teststraten_lab$Afspraak_start %>% min %>% as.Date(), data_teststraten_lab$Afspraak_start %>% max %>% as.Date(), 7),
               limits = c(data_teststraten_lab_c$Afspraak_start_datum %>% min - 1, data_teststraten_lab_c$Afspraak_start_datum %>% max + 1),
               expand = expansion(add = 0.5)) +
  #scale_y_continuous(breaks = seq(0, 6000, 1000)) +
  theme_minimal() +
  theme(
    text = element_text(size = 7),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.justification="left",
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-10,-1,-10,-10),
  )

# Figuur kiemsurv
plot_2 <-
  ggplot() +
  geom_bar(data = data_kiemsurv %>% filter(`Sample date` %in% seq(data_teststraten_lab_c$Afspraak_start_datum %>% min, 
                                                                  data_teststraten_lab_c$Afspraak_start_datum %>% max,1)),
           aes(x = `Sample date`, color = `Variant (WGS)`, fill = `Variant (WGS)`), width = 0.53
           ) +
  geom_rect(
    mapping = aes(linetype = "Delta - Omicron BA.1"),
    xmin = data_teststraten_ve_cohort1$Afspraak_start_datum %>% min - 0.7,
    xmax = data_teststraten_ve_cohort1$Afspraak_start_datum %>% max + 0.5 ,
    ymin = -3,
    ymax = 215,
    col = "black",
    fill = NA
  ) +
  geom_rect(
    mapping = aes(linetype = "Omicron BA.1 - Omicron BA.2"),
    xmin = data_teststraten_ve_cohort2$Afspraak_start_datum %>% min - 0.5,
    xmax = data_teststraten_ve_cohort2$Afspraak_start_datum %>% max + 0.7 ,
    ymin = -3,
    ymax = 215,
    col = "black",
    fill = NA
  ) +
  scale_colour_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2) +
  scale_linetype_manual(values = c(3,4)) +
  scale_x_date(breaks = seq(data_teststraten_lab$Afspraak_start %>% min %>% as.Date(), data_teststraten_lab$Afspraak_start %>% max %>% as.Date(), 7),
               expand = expansion(add = 0.5),
               limits = c(data_teststraten_lab_c$Afspraak_start_datum %>% min - 1, data_teststraten_lab_c$Afspraak_start_datum %>% max + 1)) +
  guides(linetype = "none") +
  theme_minimal() +
  theme(text = element_text(size = 7),
        axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5),
        #legend.position="top",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-1,-10,-10),)

N_wgs_samples <- data_kiemsurv %>% 
  filter(`Sample date` %in% seq(data_teststraten_lab_c$Afspraak_start_datum %>% min, 
                                                                 data_teststraten_lab_c$Afspraak_start_datum %>% max,1)) %>% 
  nrow

figuur_periodes_data_7 <- plot_grid(plot_1, plot_2,
                            align = "v", labels = c("A", "B"),
                            ncol = 1, 
                            rel_heights = c(1,1.2))

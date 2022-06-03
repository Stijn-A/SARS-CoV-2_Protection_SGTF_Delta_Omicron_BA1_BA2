

tabel_VE_figure <- tabel_VE_overall %>%
  # add empty rows in between the different tables
  bind_rows(tabel_VE_overall %>% 
              select(Cohort, Variant, Immuunstatus) %>% 
              mutate(Time_since_event = " ")) %>% 
  bind_rows(tabel_VE_tijd) %>%
  mutate(Time_since_event = Time_since_event %>%
           factor(
             levels = c(
               data_teststraten_ve$immunizatie_datum_interval_groep_specifiek %>% levels,
               " ", "Overall"
             )
           ),
         Variant = Variant %>% factor(levels = c("Omicron BA.1", "Omicron BA.2", "Delta"))
         )

bold.time_since_event <- c("Overall")
bold.labels <- ifelse(levels(tabel_VE_figure$Time_since_event) %in% bold.time_since_event, yes = "bold", no = "plain")

figuur_VE_tijd_cohort1 <- tabel_VE_figure %>% 
  filter(Cohort == "Delta-Omicron BA.1") %>% 
  filter(!(str_detect(Immuunstatus, "Naive"))) %>% 
  ggplot(data = ., aes(x = Time_since_event, y = VE, color = Variant,)) + 
    geom_hline(aes(yintercept = 0), size = .25, linetype = "dashed") + 
    geom_errorbar(aes(ymax = VE_high, ymin = VE_low, color = Variant), width = 0.2, 
                  position=position_dodge(width=0.2)) +
    geom_point(size = 1,position=position_dodge(width=0.2)) +
    facet_wrap(~Immuunstatus, ncol = 3) +
    ylab("Relative reduction (%)") +
    xlab("Time since last event (days)") +
    scale_colour_grey(start = 0.6, end = 0.1) +
    scale_y_continuous(breaks = seq(-20,100,10)) + 
    coord_cartesian(ylim=c(-25, 100)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1,face = bold.labels),
      text = element_text(size = 8),
      legend.position="top",
      legend.justification="right",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-10,-1,-10,-10),
      plot.title = element_text(margin=margin(0,0,0,0)),
      panel.grid.minor.y = element_blank()) +
    # ggtitle(str_c("A)\t", data_teststraten_ve_cohort1$Afspraak_start_datum %>% min, 
    #               " - ", data_teststraten_ve_cohort1$Afspraak_start_datum %>% max))
  ggtitle("A. Delta-Omicron BA.1 cohort")

figuur_VE_tijd_cohort2 <- tabel_VE_figure %>% 
  filter(Cohort == "Omicron BA.1-BA.2") %>% 
  filter(!(str_detect(Immuunstatus, "Naive"))) %>% 
  ggplot(data = ., aes(x = Time_since_event, y = VE, color = Variant,
  )) + 
  geom_hline(aes(yintercept = 0), size = .25, linetype = "dashed") + 
  geom_errorbar(aes(ymax = VE_high, ymin = VE_low, color = Variant), width = 0.2,
                position=position_dodge(width=0.2)) +
  geom_point(size = 1,position=position_dodge(width=0.2)) +
  facet_wrap(~Immuunstatus, ncol = 3) +
  ylab("Relative reduction (%)") +
  xlab("Time since last event (days)") +
  scale_colour_grey(start = 0.6, end = 0.1) +
  scale_y_continuous(breaks = seq(-20,100,10)) + 
  coord_cartesian(ylim=c(-25, 100)) +
  theme_minimal() +
  theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1,face = bold.labels),
        text = element_text(size = 8),
        legend.position="top",
        legend.justification="right",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-1,-10,-10),
        plot.title = element_text(margin=margin(0,0,0,0)),
        panel.grid.minor.y = element_blank()) +
  ggtitle("B. Omicron BA.1-BA.2 cohort")



figuur_VE_tijd_cohorts <- plot_grid(figuur_VE_tijd_cohort1, figuur_VE_tijd_cohort2,
                                     align = "v",
                                     ncol = 1)


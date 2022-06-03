tabel_VE_time_age_cohort1_figuur <- tabel_VE_time_age_cohort1 %>% 
  # add empty rows in between the different tables
  bind_rows(tabel_VE_time_age_cohort1 %>% 
              select(Variant, Immuunstatus,Leeftijd_breed) %>% 
              mutate(Time_since_event = " ")) %>% 
  mutate(Time_since_event = Time_since_event %>%
           factor(
             levels = c(
               data_teststraten_ve_cohort1$immunizatie_datum_interval_groep %>% levels,
               " ", "Overall"
             )
           ),
         Variant = Variant %>% factor(levels = c("Omicron BA.1", "Omicron BA.2", "Delta")))


bold.time_since_event <- c("Overall")
bold.labels <- ifelse(levels(tabel_VE_time_age_cohort1_figuur$Time_since_event) %in% bold.time_since_event, yes = "bold", no = "plain")

figuur_overall_per_leeftijd_cohort1 <- tabel_VE_time_age_cohort1_figuur %>%
  ggplot(aes(x = Time_since_event, y = VE, color = Variant,)) +
    geom_errorbar(aes(ymax = VE_high, ymin = VE_low, color = Variant), width = 0.2,
                  position=position_dodge(width=0.2)) +
    geom_point(size = 1, position=position_dodge(width=0.2)) +
    facet_grid(Leeftijd_breed ~ Immuunstatus) +
    ylab("Relative reduction (%)") +
    xlab("Time since last event (days)") +
    scale_y_continuous(breaks = seq(-20, 100, 20)) +
    scale_colour_grey(start = 0.6, end = 0.1) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1,face = bold.labels),
      text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.justification = "right",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-10, -1, -10, -10),
      plot.title = element_text(margin = margin(0, 0, 0, 0)),
      panel.grid.minor.y = element_blank()
    ) +
    # ggtitle(
    #   str_c(
    #     "A)\t",
    #     data_teststraten_ve_cohort1$Afspraak_start_datum %>% min,
    #     " - ",
    #     data_teststraten_ve_cohort1$Afspraak_start_datum %>% max
    #   )
    # )
  ggtitle("A. Delta-Omicron BA.1 cohort")

tabel_VE_time_age_cohort2_figuur <- tabel_VE_time_age_cohort2 %>% 
  # add empty rows in between the different tables
  bind_rows(tabel_VE_time_age_cohort2 %>% 
              select(Variant, Immuunstatus,Leeftijd_breed) %>% 
              mutate(Time_since_event = " ")) %>% 
  mutate(Time_since_event = Time_since_event %>%
           factor(
             levels = c(
               data_teststraten_ve_cohort2$immunizatie_datum_interval_groep %>% levels,
               " ", "Overall"
             )
           ),
         Variant = Variant %>% factor(levels = c("Omicron BA.1", "Omicron BA.2", "Delta")))

figuur_overall_per_leeftijd_cohort2 <- tabel_VE_time_age_cohort2_figuur %>% 
  ggplot(aes(x = Time_since_event, y = VE, color = Variant)) +
    geom_errorbar(
      aes(ymax = VE_high, ymin = VE_low, color = Variant),
      width = 0.2,
      position = position_dodge(width = 0.2)
    ) +
    geom_point(size = 1, position = position_dodge(width = 0.2)) +
    facet_grid(Leeftijd_breed ~ Immuunstatus) +
    ylab("Relative reduction (%)") +
    xlab("Time since last event (days)") +
    scale_y_continuous(breaks = seq(-20, 100, 20)) +
    scale_colour_grey(start = 0.6, end = 0.1) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1,face = bold.labels),
      text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.justification = "right",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-10, -1, -10, -10),
      plot.title = element_text(margin = margin(0, 0, 0, 0)),
      panel.grid.minor.y = element_blank()
    ) +
  ggtitle("B. Omicron BA.1-BA.2 cohort")


figuur_leeftijd_cohorts <-
  plot_grid(
    figuur_overall_per_leeftijd_cohort1,
    figuur_overall_per_leeftijd_cohort2,
    align = "v",
    ncol = 1
  )

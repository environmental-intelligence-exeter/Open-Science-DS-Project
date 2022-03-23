## ---------------------------
##
## Script name: plot.r
##
## Purpose of script: Visualise results
##
## Author: Nathanael Sheehan
##
## Date Created: 2022-03-21
##
## Copyleft (c) Nathanael Sheehan, 2022
## Email: nathanaelsheehan@gmail.co.uk
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

#################################################################
##              Temporal Submissions per database              ##
#################################################################
gisaid_temporal_subs = gisaid %>% group_by(Date) %>% summarise(sum_gisaid = sum(GISAID.monthly.submissions)) %>% arrange(Date)
cd19dp_temporal_subs = cd19_agg_data %>% group_by(Date) %>% summarise(sum_cd19dp = sum(CD19DP.monthly.submissions)) %>% arrange(Date)
gisaid_temporal_subs_av = gisaid %>% group_by(Date) %>% summarise(sum_gisaid_av = mean(GISAID.monthly.submissions)) %>% arrange(Date)
cd19dp_temporal_subs_av = cd19_agg_data %>% group_by(Date) %>% summarise(sum_cd19dp_av = mean(CD19DP.monthly.submissions)) %>% arrange(Date)

temporal_sub_all =right_join(gisaid_temporal_subs,cd19dp_temporal_subs) %>%
  right_join(gisaid_temporal_subs_av) %>%
  right_join(cd19dp_temporal_subs_av) %>%
  mutate_all(~ replace(., is.na(.), 0))

ggplot(data=temporal_sub_all) +
  geom_line(aes(x=Date,y=sum_gisaid, group=1, color='GISAID Monthly Total')) +
  geom_line(aes(x=Date,y=sum_cd19dp, group=1, color ='CD19DP Monthly Total')) +
  annotate("text", x = 7, y = 60000, label = "WHO Declares\n Pandemic") +
  annotate("text", x = 17, y = 6000, label = "AstraZeneca's \nVaccine Authorised") +
  annotate("text", x = 18, y = 600000, label = "EBI Open letter") +
  annotate("text", x = 27, y = 600000, label = "Global Covid-19 Deaths \nPass Five Million") +
  scale_colour_manual("",
                      breaks = c("GISAID Monthly Total", "CD19DP Monthly Total"),
                      values = c("green", "blue")) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(x = "Date",
       y = "Sequence Submissions",
       title = "Monthly Total SARS-CoV-2 Sequence Submissions",
       caption = "\nFig. 1: Monthly totals of global SARS-CoV-2 cases sequenced and shared on the GISAID and Covid-19 Data Platform database until Febuary 22 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ ") + dark_theme() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


ggplot(data=temporal_sub_all) +
geom_line(aes(x=Date,y=sum_gisaid_av, group=1, color='GISAID Monthly Mean')) +
  geom_line(aes(x=Date,y=sum_cd19dp_av, group=1, color ='CD19DP Monthly Mean')) +
  annotate("text", x = 7, y = 600, label = "WHO Declares\n Pandemic") +
  annotate("text", x = 17, y = 2000, label = "AstraZeneca's \nVaccine Authorised") +
  annotate("text", x = 18, y = 4000, label = "EBI Open letter") +
  annotate("text", x = 27, y = 6000, label = "Global Covid-19 Deaths \nPass Five Million") +
  scale_colour_manual("",
                      breaks = c("GISAID Monthly Mean", "CD19DP Monthly Mean"),
                      values = c("green", "blue")) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1)) +
  labs(x = "Date",
       y = "Sequence Submissions",
       title = "Monthly Mean SARS-CoV-2 Sequence Submissions",
       caption = "\nFig. 2:  Monthly mean of global SARS-CoV-2 cases sequenced and shared on the GISAID and Covid-19 Data Platform database until Febuary 22 2022\n\n\nGISAID Metadata: https://www.epicov.org/\nCovid-19 Data Platform Metadata: https://www.ebi.ac.uk/ena/portal/api/ ") + dark_theme() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#################################################################
##                        Log-Log plots                        ##
#################################################################
log_df = main_df %>% filter(Date == "2022/02")

gisaid_1 = ggplot(log_df, aes((GISAID.total.Submissions), (log_df$cases))) +
  geom_point(aes(color = continent, size = `Genomes per confirmed cases (GISAID)`),alpha = 75 /100) +
  geom_text(aes(label = Country, color = continent),
            nudge_y = 0.06) + dark_theme() +
  labs(x = "Sequenced Genomes",
       y = "Confirmed Cases",
       title = "Confirmed Covid-19 Cases vs. \nSequenced Genomes in the GISAID Open database",
       caption = "\nFig. 3: The proportion of global SARS-CoV-2 cases sequenced and shared on the GISAID database until Febuary 22 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins")  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col =rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))

gisaid_2 = ggplot(log_df, aes((GISAID.total.Submissions),(log_df$People_fully_vaccinated))) +
  geom_point(aes(color = continent, size = `Genomes per confirmed full vaccine (GISAID)`),alpha = 75 /100) +
  geom_text(aes(label = Country,color = continent),nudge_y = 0.06) +
  dark_theme() +
  labs(x = "Sequenced Genomes",
       y = "People Fully Vaccinated",
       title = "People Fully Vaccinated vs. \nSequenced Genomes in the GISAID Open Database",
       caption = "\nFig. 4: The proportion of people fully vaccinated and global SARS-CoV-2 cases sequenced and shared on the GISAID database until Febuary 28 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins",)  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col =rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))


gisaid_plot = ggarrange(ncol  = 1, gisaid_1, gisaid_2)
gisaid_plot


####
c19dp_1 = ggplot(log_df, aes(C19DP.total.Submissions, cases)) +
  geom_point(aes(color = continent, size = `Genomes per confirmed cases (C19DP)`),
             alpha = 75 / 100) +
  geom_text(aes(label = Country, color = continent), nudge_y = 0.06) +
  dark_theme() +
  labs(
    x = "Sequenced Genomes",
    y = "Confirmed Cases",
    title = "Confirmed Covid-19 Cases vs. \nSequenced Genomes in the Covid-19 Data Portal",
    caption = "\nFig. 5: The proportion of global SARS-CoV-2 cases sequenced and shared on the C19DP database until Febuary 28 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins"
  )  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col = rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))

c19dp_2 = ggplot(log_df, aes((CD19DP.total.Submissions),
                              (log_df$People_fully_vaccinated))) +
  geom_point(aes(size = `Genomes per confirmed full vaccine (C19DP)`, color = continent),
             alpha = 75 / 100) + geom_text(aes(label = Country, color = continent), nudge_y = 0.06) +
  geom_smooth() +
  dark_theme() +
  labs(
    x = "Sequenced Genomes",
    y = "People Fully Vaccinated",
    title = "People Fully Vaccinated vs. \nSequenced Genomes in the Covid-19 Data Portal",
    caption = "\nFig. 6: The proportion of people fully vaccinated and global SARS-CoV-2 cases sequenced and shared on the C19DP database until Febuary 28 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins",
  )  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col = rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))


c19dp_plot = ggarrange(c19dp_1, c19dp_2)
c19dp_plot


##################################################################
##                        Bar Chart Race                        ##
##################################################################

cd19_agg_data %>%
  barChartRace(
    x = "CD19DP.total.Submissions",
    y = "Country",
    time = "Date",
    ytitle = "Country",
    xtitle = "Count (n submissions)",
    title = "Global GISAID EpiCov Database Submissions",
    paddingWidth = 0.1,
    xFontSize = 10,
    yFontSize = 10,
    xticks = 12,
    xtitleFontSize = 14,
    ytitleFontSize = 14,
    titleFontSize = 22,
    stroke = "black",
    strokeWidth = NULL,
    font = "gochi",
    bgcol = "#cf2e2e",
    panelcol = "#fcb900",
    xgridlinecol = "#8ed1fc",
    opacity = 1,
    timeLabel = TRUE,
    timeLabelOpts = list(
      size = 28,
      prefix = "",
      suffix = "",
      xOffset = 0.5,
      yOffset = 1
    ),
    width = NULL,
    height = NULL
  )

who_world_map = dimaqdata::who_world_map

data = main_df %>%
  dplyr::filter(Date == "2022/02") %>%
  dplyr::rename(CNTRY_TERR=Country) %>%
  as.data.frame()

data = left_join(who_world_map,data)

mat = data
rownames(mat) = mat[,1]
mat = mat %>% dplyr::select(-Country, -Group, -Continent)

# Matrix format
mat <- data
rownames(mat) <- mat[,1]

mat <- as.matrix(mat)

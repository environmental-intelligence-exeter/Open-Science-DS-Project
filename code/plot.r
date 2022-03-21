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

##################################################################
##                             Plot                             ##
##################################################################
gisaid_1 = ggplot(main_df, aes((GISAID.total.Submissions), (main_df$cases))) +
  geom_point(aes(color = continent, size = `Genomes per confirmed cases (GISAID)`),alpha = 75 /100) +
  geom_text(aes(label = Country, color = continent),
            nudge_y = 0.06) + dark_theme() +
  labs(x = "Sequenced Genomes",
       y = "Confirmed Cases",
       title = "Confirmed Covid-19 Cases vs. \nSequenced Genomes in the GISAID Open database",
       caption = "\nFig. 1: The proportion of global SARS-CoV-2 cases sequenced and shared on the GISAID database until Febuary 22 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins")  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col =rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))




gisaid_2 = ggplot(main_df, aes((GISAID.total.Submissions),(main_df$People_fully_vaccinated))) +
  geom_point(aes(color = continent, size = `Genomes per confirmed full vaccine (GISAID)`),alpha = 75 /100) +
  geom_text(aes(label = Country,color = continent),nudge_y = 0.06) +
  dark_theme() +
  labs(x = "Sequenced Genomes",
       y = "People Fully Vaccinated",
       title = "People Fully Vaccinated vs. \nSequenced Genomes in the GISAID Open Database",
       caption = "\nFig. 2: The proportion of people fully vaccinated and global SARS-CoV-2 cases sequenced and shared on the GISAID database until Febuary 28 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins",)  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col =rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))


gisaid_plot = ggarrange(ncol  = 1, gisaid_1, gisaid_2)
gisaid_plot

ggplot(data = cd19_agg_data %>% filter(Country == "Nigeria"), aes(x=Date, y=CD19DP.total.Submissions, group = 1)) +   geom_line( color="#69b3a2") +
  xlab("")
ggplot(data = gisaid %>% filter(Country == "Nigeria"), aes(x=Date, y=GISAID.total.Submissions, group = 1)) +   geom_line( color="#69b3a2") +
  xlab("")
####
c19dp_1 = ggplot(main_df, aes(C19DP.total.Submissions, cases)) +
  geom_point(aes(color = continent, size = `Genomes per confirmed cases (C19DP)`),
             alpha = 75 / 100) +
  geom_text(aes(label = Country, color = continent), nudge_y = 0.06) +
  dark_theme() +
  labs(
    x = "Sequenced Genomes",
    y = "Confirmed Cases",
    title = "Confirmed Covid-19 Cases vs. \nSequenced Genomes in the Covid-19 Data Portal",
    caption = "\nFig. 3: The proportion of global SARS-CoV-2 cases sequenced and shared on the C19DP database until Febuary 28 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins"
  )  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col = rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))

c19dp_2 = ggplot(main_df, aes((C19DP.total.Submissions),
                              (main_df$People_fully_vaccinated))) +
  geom_point(aes(size = `Genomes per confirmed full vaccine (C19DP)`, color = continent),
             alpha = 75 / 100) + geom_text(aes(label = Country, color = continent), nudge_y = 0.06) +
  dark_theme() +
  labs(
    x = "Sequenced Genomes",
    y = "People Fully Vaccinated",
    title = "People Fully Vaccinated vs. \nSequenced Genomes in the Covid-19 Data Portal",
    caption = "\nFig. 4: The proportion of people fully vaccinated and global SARS-CoV-2 cases sequenced and shared on the C19DP database until Febuary 28 2022\n The size of each circle is proportional to the number of SARS-CoV-2 genomes sequenced per number of confirmed cases. Countries are colored by continent.\n\nSARS-CoV-2 Data: John Hopkins",
  )  +
  scale_y_log10() +
  scale_x_log10() +
  geom_rug(col = rgb(.5, 0, 0, alpha = .2)) +
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1))


c19dp_plot = ggarrange(c19dp_1, c19dp_2)
c19dp_plot

cd19_agg_data %>%
  barChartRace(
    x = "CD19DP.total.Submissions",
    y = "Country",
    time = "month",
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

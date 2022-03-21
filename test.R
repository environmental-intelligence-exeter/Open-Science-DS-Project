

##########################################################################
##  Download latest John Hopkins global Covid-19 cases & vaccines data  ##
##########################################################################
##### CASES
jh_global_covid = fetch_data(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
           path = "data/covid-jh.csv" ## fetch latest github data
           ) %>%
  select(-c(Lat, Long)) %>% # be gone lat long
  # select(-1) %>%
  rename(Country = 2) %>%
  mutate(Country = ifelse(Province.State == "French Guiana" & Country == "France", "French Guiana", Country)) %>%
  mutate(Country = ifelse(Province.State == "French Polynesia" & Country == "France", "French Polynesia", Country)) %>%
  mutate(Country = ifelse(Province.State == "Guadeloupe" & Country == "France", "Guadeloupe", Country)) %>%
  mutate(Country = ifelse(Province.State == "Martinique" & Country == "France", "Martinique", Country)) %>%
  mutate(Country = ifelse(Province.State == "Mayotte" & Country == "France", "Mayotte", Country)) %>%
  mutate(Country = ifelse(Province.State == "New Caledonia" & Country == "France", "New Caledonia", Country)) %>%
  mutate(Country = ifelse(Province.State == "Reunion" & Country == "France", "Reunion", Country)) %>%
  mutate(Country = ifelse(Province.State == "Saint Barthelemy" & Country == "France", "Saint Barthelemy", Country)) %>%
  mutate(Country = ifelse(Province.State == "Saint Pierre and Miquelon" & Country == "France", "Saint Pierre and Miquelon", Country)) %>%
  mutate(Country = ifelse(Province.State == "St Martin" & Country == "France", "St Martin", Country)) %>%
  mutate(Country = ifelse(Province.State == "Wallis and Futuna" & Country == "France", "Wallis and Futuna", Country)) %>%
  mutate(Country = ifelse(Province.State == "Curacao" & Country == "Netherlands", "Curacao", Country)) %>%
  mutate(Country = ifelse(Province.State == "Sint Maarten" & Country == "Netherlands", "	Sint Maarten", Country)) %>%
  mutate(Country = ifelse(Province.State == "Aruba" & Country == "Netherlands", "Aruba", Country)) %>%
  mutate(Country = ifelse(Province.State == "Bonaire, Sint Eustatius and Saba" & Country == "Netherlands", "Bonaire, Sint Eustatius and Saba", Country)) %>%
  mutate(Country = ifelse(Province.State == "Anguilla" & Country == "United Kingdom", "Anguilla", Country)) %>%
  mutate(Country = ifelse(Province.State == "Bermuda" & Country == "United Kingdom", "Bermuda", Country)) %>%
  mutate(Country = ifelse(Province.State == "British Virgin Islands" & Country == "United Kingdom", "British Virgin Islands", Country)) %>%
  mutate(Country = ifelse(Province.State == "Cayman Islands" & Country == "United Kingdom", "Cayman Islands", Country)) %>%
  mutate(Country = ifelse(Province.State == "Channel Islands" & Country == "United Kingdom", "Channel Islands", Country)) %>%
  mutate(Country = ifelse(Province.State == "Gibraltar" & Country == "United Kingdom", "Gibraltar", Country)) %>%
  mutate(Country = ifelse(Province.State == "Montserrat" & Country == "United Kingdom", "Montserrat", Country)) %>%
  mutate(Country = ifelse(Province.State == "Turks and Caicos Islands" & Country == "United Kingdom", "Turks and Caicos Islands", Country)) %>%
  select(-c(Province.State)) %>%
  pivot_longer(!Country, names_to = "Date", values_to = "cases") %>%
  mutate(Date = paste0(sub('.', '', Date))) %>%
  filter(Date == "2.22.22") %>%
  select(-c(Date)) %>%
  mutate(Country = case_when(
    Country == "US" ~ "USA",
    Country == "The Bahamas" ~ "Bahamas",
    Country == "Taiwan*" ~ "Taiwan",
    Country == "Korea, South" ~ "South Korea",
    Country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
    Country == "Congo (Brazzaville)" ~ "Republic of the Congo",
    Country == "Czechia" ~ "Czech Republic",
    Country == "Wallis and Futuna" ~ "Wallis and Futuna Islands",
    Country == "Bonaire, Sint Eustatius and Saba" ~ "Bonaire",
    TRUE ~ Country
  ))

jh_global_covid = aggregate(. ~ Country, jh_global_covid,  FUN = sum)
# > head(jh_global_covid)
# Country  cases
# 1 Afghanistan 172716
# 2     Albania 270455
# 3     Algeria 264365
# 4     Andorra  37820
# 5      Angola  98671
# 6  Antarctica     11

#### VACCINES
jh_vaccine = fetch_data(url = "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/vaccine_data_global.csv",
           path = "data/js-vac.csv") %>%
  select(Province_State,Country_Region, Doses_admin,People_partially_vaccinated,People_fully_vaccinated,Date) %>%
  select(-c(Date)) %>%
  rename(Country = Country_Region) %>%
  mutate(
    Country = ifelse(
      Province_State == "French Guiana" &
        Country == "France",
      "French Guiana",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "French Polynesia" &
        Country == "France",
      "French Polynesia",
      Country
    )
  ) %>%
  mutate(Country = ifelse(
    Province_State == "Guadeloupe" &
      Country == "France",
    "Guadeloupe",
    Country
  )) %>%
  mutate(Country = ifelse(
    Province_State == "Martinique" &
      Country == "France",
    "Martinique",
    Country
  )) %>%
  mutate(Country = ifelse(
    Province_State == "Mayotte" &
      Country == "France",
    "Mayotte",
    Country
  )) %>%
  mutate(
    Country = ifelse(
      Province_State == "New Caledonia" &
        Country == "France",
      "New Caledonia",
      Country
    )
  ) %>%
  mutate(Country = ifelse(
    Province_State == "Reunion" &
      Country == "France",
    "Reunion",
    Country
  )) %>%
  mutate(
    Country = ifelse(
      Province_State == "Saint Barthelemy" &
        Country == "France",
      "Saint Barthelemy",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Saint Pierre and Miquelon" &
        Country == "France",
      "Saint Pierre and Miquelon",
      Country
    )
  ) %>%
  mutate(Country = ifelse(
    Province_State == "St Martin" &
      Country == "France",
    "St Martin",
    Country
  )) %>%
  mutate(
    Country = ifelse(
      Province_State == "Wallis and Futuna" &
        Country == "France",
      "Wallis and Futuna",
      Country
    )
  ) %>%
  mutate(Country = ifelse(
    Province_State == "Curacao" &
      Country == "Netherlands",
    "Curacao",
    Country
  )) %>%
  mutate(
    Country = ifelse(
      Province_State == "Sint Maarten" &
        Country == "Netherlands",
      "	Sint Maarten",
      Country
    )
  ) %>%
  mutate(Country = ifelse(
    Province_State == "Aruba" &
      Country == "Netherlands",
    "Aruba",
    Country
  )) %>%
  mutate(
    Country = ifelse(
      Province_State == "Bonaire, Sint Eustatius and Saba" &
        Country == "Netherlands",
      "Bonaire, Sint Eustatius and Saba",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Anguilla" &
        Country == "United Kingdom",
      "Anguilla",
      Country
    )
  ) %>%
  mutate(Country = ifelse(
    Province_State == "Bermuda" &
      Country == "United Kingdom",
    "Bermuda",
    Country
  )) %>%
  mutate(
    Country = ifelse(
      Province_State == "British Virgin Islands" &
        Country == "United Kingdom",
      "British Virgin Islands",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Cayman Islands" &
        Country == "United Kingdom",
      "Cayman Islands",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Channel Islands" &
        Country == "United Kingdom",
      "Channel Islands",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Gibraltar" &
        Country == "United Kingdom",
      "Gibraltar",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Montserrat" &
        Country == "United Kingdom",
      "Montserrat",
      Country
    )
  ) %>%
  mutate(
    Country = ifelse(
      Province_State == "Turks and Caicos Islands" &
        Country == "United Kingdom",
      "Turks and Caicos Islands",
      Country
    )
  ) %>%
  mutate(
    Country = case_when(
      Country == "US" ~ "USA",
      Country == "The Bahamas" ~ "Bahamas",
      Country == "Taiwan*" ~ "Taiwan",
      Country == "Korea, South" ~ "South Korea",
      Country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
      Country == "Congo (Brazzaville)" ~ "Republic of the Congo",
      Country == "Czechia" ~ "Czech Republic",
      Country == "Wallis and Futuna" ~ "Wallis and Futuna Islands",
      Country == "Bonaire, Sint Eustatius and Saba" ~ "Bonaire",
      TRUE ~ Country
    )
  ) %>%
  select(-c(Province_State))

jh_vaccine = aggregate(. ~ Country, jh_vaccine, FUN = sum)
# > head(jh_vaccine)
# Country Doses_admin People_partially_vaccinated People_fully_vaccinated
# 1         Afghanistan     5535254                     4907058                 4231984
# 2             Albania     2707658                     1269746                 1196277
# 3             Algeria    13461201                     7456361                 6076272
# 4             Andorra      142420                       57797                   53250
# 5              Angola    15505389                    10591264                 5448403
# 6 Antigua and Barbuda      124726                       63492                   60963
##################################################################
##                  GISAID Global Monthly Data                  ##
##################################################################
gisaid = as.data.frame(readxl::read_xlsx("../../../Downloads/gisaid_monthly_submissions_global_2022-02-21.xlsx") %>%
                         rename(Country = ...1) %>%
                         mutate(Country = case_when(
                           Country == "US" ~ "USA",
                           Country == "The Bahamas" ~ "Bahamas",
                           TRUE ~ Country
                         ))  %>%
                         pivot_longer(!Country, names_to = "Date", values_to = "Count") %>%
                         filter(Date != "country_total") %>%
                         mutate(Date = paste0(substr(Date, 4, 8), "/", substr(Date, 1, 2))) %>%
                         filter(Country != "monthly total:"))

gisaid$GISAID.total.Submissions = ave(gisaid$Count, gisaid$Country, FUN = cumsum)

latest_gisaid = gisaid %>% filter(Date == "2022/02") %>% arrange(desc(GISAID.total.Submissions))




#continents_df = aggregate(. ~ continent, main_df, FUN = sum)

# Missing Covid data from JH
# [1] "Hong Kong"                "Puerto Rico"              "Myanmar"                  "Guam"
# [5] "Palestine"                "Northern Mariana Islands" "Sint Maarten"             "U.S. Virgin Islands"
# [9] "Canary Islands"           "British Virgin Islands"   "Crimea"                   "Saint Martin"
# [13] "Faroe Islands"            "American Samoa"           "Sint Eustatius"

# plot log plots
# plot bivariate plots
# cluster analysis using DBSCAN, k-means, Hierarchical
# Output globe dataset for web vis
#
#
#


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

##################################################################
##                          Clustering                          ##
##################################################################

b = test %>% select(
  c(
    Country,
    cases,
    csum,
    size,
    Doses_admin,
    People_partially_vaccinated,
    People_fully_vaccinated
  )
) %>% na.omit()
b[2:7] = scale(b[2:7])
b = b[-157, ]

b <- data.frame(b[, -1], row.names = b[, 1])



# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(
  k.values,
  wss_values,
  type = "b",
  pch = 19,
  frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total within-clusters sum of squares"
)

res.dist <- get_dist(b, stand = TRUE, method = "pearson")

fviz_dist(res.dist,
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(b, centers = 4, nstart = 25)

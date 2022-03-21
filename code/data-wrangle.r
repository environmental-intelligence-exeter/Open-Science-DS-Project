##########################################################################
##  Download latest John Hopkins global Covid-19 cases & vaccines data  ##
##########################################################################
##### CASES
jh_global_covid = fetch_data(url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                             path = "data/covid-jh.csv" ## fetch latest github data
) %>%
  select(-c(Lat, Long)) %>% # be gone lat long
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

#################################################################
##             Covid-19 Data Platform Monthly Data             ##
#################################################################
if (!file.exists("data/emi-api-data.tsv")) {
  # This file is very large, and in fact does not use the traditional API for the CD19DP due to its one million cap limit.
  url = "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&query=tax_tree(2697049)&fields=accession%2Cagricola_id%2Caltitude%2Cbase_count%2Cbio_material%2Ccell_line%2Ccell_type%2Ccollected_by%2Ccollection_date%2Ccountry%2Ccultivar%2Cculture_collection%2Cdataclass%2Cdescription%2Cdev_stage%2Cdoi%2Cdoi_url%2Cecotype%2Cenvironmental_sample%2Cfirst_public%2Cgermline%2Chaplotype%2Chost%2Cidentified_by%2Cisolate%2Cisolation_source%2Ckeywords%2Clab_host%2Clast_updated%2Clocation%2Cmating_type%2Cmetagenome_source%2Cmol_type%2Corganelle%2Cpatent_number%2Cplasmid%2Cpubmed_id%2Cpubmed_url%2Csample_accession%2Cscientific_name%2Csequence_md5%2Csequence_version%2Cserotype%2Cserovar%2Csex%2Cspecimen_voucher%2Cstrain%2Cstudy_accession%2Csub_species%2Csub_strain%2Csubmitted_sex%2Ctax_division%2Ctax_id%2Ctissue_lib%2Ctissue_type%2Ctopology%2Cvariety&format=tsv&limit=0"
  path = "emi-api-data.csv"
  download.file(url, path)
} else {
  cd19_agg_data_raw = data.table::fread("data/emi-api-data.tsv")
  cd19_agg_data_raw$country = gsub("(.*):.*", "\\1", cd19_agg_data_raw$country)

  cd19_agg_data = cd19_agg_data_raw %>%
    select(collection_date, country) %>%
    mutate(month = substr(collection_date, start = 1, stop = 7)) %>%
    rename(Country = country) %>%
    group_by(month, Country) %>%
    summarise(Count = n()) %>%
    drop_na(month) %>%
    distinct(Country, .keep_all = TRUE) %>%
    rename(Date = month) %>%
    mutate(Date = paste0(substr(Date, 1, 4), "/", substr(Date, 6, 7))) %>%
    filter(Country != "") %>%
    arrange(Date) %>% pivot_wider(names_from = Date, values_from = Count) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    pivot_longer(!Country, names_to = "Date", values_to = "Count") %>%
    mutate(
      Country = case_when(
        Country == "West Bank" ~ "Palestine",
        Country == "Viet Nam" ~ "Vietnam",
        TRUE ~ Country
      )
    )

  cd19_agg_data$CD19DP.total.Submissions = ave(cd19_agg_data$Count, cd19_agg_data$Country, FUN = cumsum)
  rm(cd19_agg_data_raw)
}

##################################################################
##                  GISAID Global Monthly Data                  ##
##################################################################
# GISAID monthly metadata submissions downloaded from https://www.epicov.org/epi3/frontend#5dc229
gisaid = as.data.frame(
  readxl::read_xlsx("data/gisaid_monthly_submissions_global_2022-03-20.xlsx") %>%
    rename(Country = ...1) %>%
    mutate(
      Country = case_when(
        Country == "US" ~ "USA",
        Country == "The Bahamas" ~ "Bahamas",
        TRUE ~ Country
      )
    )  %>%
    pivot_longer(!Country, names_to = "Date", values_to = "Count") %>%
    filter(Date != "country_total") %>%
    mutate(Date = paste0(substr(Date, 4, 8), "/", substr(Date, 1, 2))) %>%
    filter(Country != "monthly total:")
)
gisaid$GISAID.total.Submissions = ave(gisaid$Count, gisaid$Country, FUN = cumsum)

latest_gisaid = gisaid %>% filter(Date == "2022/02") %>% arrange(desc(GISAID.total.Submissions))

#################################################################
##                          Join Data                          ##
#################################################################
main_df = cd19_agg_data %>%
  right_join(gisaid)
  select(-c(Date, Raw.reads.submitted, Count)) %>%
  rename(C19DP.total.Submissions = Sequences.submitted) %>%
  left_join(jh_global_covid) %>%
  left_join(jh_vaccine) %>%
  na.omit(cases) %>%
  mutate("Genomes per confirmed cases (GISAID)" = GISAID.total.Submissions / cases) %>%
  mutate("Genomes per confirmed cases (C19DP)" =  C19DP.total.Submissions / cases) %>%
  mutate("Genomes per confirmed full vaccine (GISAID)" = GISAID.total.Submissions / cases) %>%
  mutate("Genomes per confirmed full vaccine (C19DP)" =  C19DP.total.Submissions / cases)

main_df$continent = countrycode(sourcevar = main_df[, "Country"],
                                origin = "country.name",
                                destination = "continent")

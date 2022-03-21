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
    pivot_longer(!Country, names_to = "Date", values_to = "Count")

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
main_df = read.csv("../../../Downloads/summary-data-countries (1).csv") %>%
  rename(Country =   ï..Country) %>%
  mutate(
    Country = case_when(
      Country == "State of Palestine" ~ "Palestine",
      Country == "Viet Nam" ~ "Vietnam",
      TRUE ~ Country
    )
  ) %>%
  right_join(latest_gisaid)  %>%
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

#################################################################
##             Covid-19 Data Platform Monthly Data             ##
#################################################################
url = "https://www.ebi.ac.uk/ena/portal/api/search?result=sequence&query=tax_tree(2697049)&fields=accession%2Cagricola_id%2Caltitude%2Cbase_count%2Cbio_material%2Ccell_line%2Ccell_type%2Ccollected_by%2Ccollection_date%2Ccountry%2Ccultivar%2Cculture_collection%2Cdataclass%2Cdescription%2Cdev_stage%2Cdoi%2Cdoi_url%2Cecotype%2Cenvironmental_sample%2Cfirst_public%2Cgermline%2Chaplotype%2Chost%2Cidentified_by%2Cisolate%2Cisolation_source%2Ckeywords%2Clab_host%2Clast_updated%2Clocation%2Cmating_type%2Cmetagenome_source%2Cmol_type%2Corganelle%2Cpatent_number%2Cplasmid%2Cpubmed_id%2Cpubmed_url%2Csample_accession%2Cscientific_name%2Csequence_md5%2Csequence_version%2Cserotype%2Cserovar%2Csex%2Cspecimen_voucher%2Cstrain%2Cstudy_accession%2Csub_species%2Csub_strain%2Csubmitted_sex%2Ctax_division%2Ctax_id%2Ctissue_lib%2Ctissue_type%2Ctopology%2Cvariety&format=tsv&limit=0"
path = "emi-api-data.csv"
download.file(url,path)

cd19_agg_data_raw =data.table::fread("emi-api-data.tsv")
cd19_agg_data_raw$country = gsub("(.*):.*","\\1",cd19_agg_data_raw$country)

cd19_agg_data = cd19_agg_data_raw %>%
  select(collection_date,country) %>%
  mutate(month = substr(collection_date, start = 1, stop = 7)) %>%
  rename(Country = country) %>%
  group_by(month,Country) %>%
  summarise(Count = n()) %>%
  drop_na(month) %>%
  distinct(Country, .keep_all = TRUE) %>%
  rename(Date = month) %>%
  mutate(Date = paste0(substr(Date, 1, 4), "/", substr(Date, 6, 7))) %>%
  filter(Country != "") %>%
  arrange(Date) %>% pivot_wider(names_from = Date,values_from = Count) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  pivot_longer(!Country, names_to = "Date", values_to = "Count")

cd19_agg_data$CD19DP.total.Submissions = ave(cd19_agg_data$Count, cd19_agg_data$Country, FUN = cumsum)

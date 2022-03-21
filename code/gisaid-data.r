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

                                                                                      gisaid$GISAID.total.Submissions = ave(gisaid$Count, gisaid$Country, FUN = cumsum)

latest_gisaid = gisaid %>% filter(Date == "2022/02") %>% arrange(desc(GISAID.total.Submissions))

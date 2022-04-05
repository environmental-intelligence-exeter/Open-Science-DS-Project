## ---------------------------
##
## Script name: build.r
##
## Purpose of script: Run analysis
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
## Notes: Run full analysis at ones own peril. The process involves a 2GB tsv file being downloaded onto your local machine.
##
##
## ---------------------------

#################################################################
##                          Read Data                          ##
#################################################################
main_df = readRDS("data/main_df.rds")
temporal_sub_all = readRDS("data/temporal_sub_all.rds")
cd19_agg_data = readRDS("data/cd19_agg_data.RDS")
gisaid = readRDS("data/gisaid.RDS")

present = main_df %>% dplyr::filter(Date == "2022/02")


r_score1 = data.frame(matrix(ncol = 3, nrow = 10))

r_score_rn = c("Metric", "GISAID: EpiCov", "EBI: Covid-19 Data Portal")
colnames(r_score1) = r_score_rn

metric_col = c(
  # DATA Responsibility
  "R1.1 Sufficent Metadata",
  # metadata which documents the progress of data in the repo to date
  "R1.2 Techincal Documentation",
  # documentation online on how to use data services
  "R1.3 Format Control",
  # uploading data
  "R1.4 Content Control",
  # peer review
  "R1.5  Database Authentication ",
  # log in
  # SERVICES Responsibility
  "R2.1 Reliable Data Services",
  # a range of data tools which permit the download of data in various formats
  # LEGAL Responsibility
  "R3.1 Manage IP of Data Producers",
  # legal agreement much can be done
  "R3.2 Security of System",
  # does the system have vulnerabilities
  "R3.3 Security of Content" ,
  # how is content ensured its not reshared ?
  "Total Score"

)
r_score1$Metric = metric_col

r_score1$`EBI: Covid-19 Data Portal`[1] = 0.9
r_score1$`EBI: Covid-19 Data Portal`[2] = 0.9
r_score1$`EBI: Covid-19 Data Portal`[3] = 0.9
r_score1$`EBI: Covid-19 Data Portal`[4] = 0.9
r_score1$`EBI: Covid-19 Data Portal`[5] = 0
r_score1$`EBI: Covid-19 Data Portal`[6] = 0.4
r_score1$`EBI: Covid-19 Data Portal`[7] = 0
r_score1$`EBI: Covid-19 Data Portal`[8] = 0.4
r_score1$`EBI: Covid-19 Data Portal`[9] = 0
r_score1$`EBI: Covid-19 Data Portal`[10] = sum(r_score1$`EBI: Covid-19 Data Portal`[1:9])

r_score1$`GISAID: EpiCov`[1] = 0.4
r_score1$`GISAID: EpiCov`[2] = 0.4
r_score1$`GISAID: EpiCov`[3] = 0.9
r_score1$`GISAID: EpiCov`[4] = 0.9
r_score1$`GISAID: EpiCov`[5] = 0.9
r_score1$`GISAID: EpiCov`[6] = 0.9
r_score1$`GISAID: EpiCov`[7] = 0.4
r_score1$`GISAID: EpiCov`[8] = 0.4
r_score1$`GISAID: EpiCov`[9] = 0.9
r_score1$`GISAID: EpiCov`[10] = sum(r_score1$`GISAID: EpiCov`[1:9])
library(formattable)
formattable(r_score1)

formattable(
  r_score1,
  align = c("l", "c", "c", "r"),
  list(
    `Metric` = formatter("span", style = ~ style(
      color = "grey", font.weight = "bold"
    )),
    `GISAID: EpiCov` = color_tile(customRed, customGreen0),
    `EBI: Covid-19 Data Portal` = color_tile(customRed, customGreen0),
    `Score` = color_bar(customRed)
  )
)

# Define color_tile_mean function
color_tile_mean <- function (...) {
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px",
          `border-radius` = "4px",
          `background-color` = ifelse(x < mean(x) , red, green)) # Remember to change the colors!
  })}

formattable(r_score1, align=c("l", "c", "c"),list(
  `Metric` = formatter("span", style = ~ style(
    color = "grey", font.weight = "bold"
  )),
  area(row = row_change:row_change_percent,
       col = -1) ~ red_green_formatter),
  `EBI: Covid-19 Data Portal`=color_tile_mean(),
  `GISAID: EpiCov`=color_tile_mean()
)
)

#################################################################
##                        Run Dashboard                        ##
#################################################################
source("code/dash.rmd")
#################################################################
##                      Run Full Analysis                      ##
#################################################################
# source("code/set-up.r")
# source("code/data-wrangle.r")
# source("code/cluster.r")
# source("code/dash.rmd")



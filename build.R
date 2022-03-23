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
## Notes: Run for analysis at ones own peril. The process involves a 2GB tsv file being downloaded onto your local machine.
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


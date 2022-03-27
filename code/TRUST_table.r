r_score = data.frame(matrix(ncol=8, nrow = 2))
cols = c("Repository",
         # DATA Responsibility
         "R1.1 Sufficent Metadata", # metadata which documents the progress of data in the repo to date
         "R1.2 Techincal Documentation", # documentation online on how to use data services
         "R1.3 Format Control", # uploading data
         "R1.4 Content Control", # peer review
         "R1.5 Authenticated Provinence",# log in
         # SERVICES Responsibility
         "R2.1 Reliable Data Services", # a range of data tools which permit the download of data in various formats
         # LEGAL Responsibility
         "R3.1 Manage IP of Data Producers", # legal agreement much can be done
         "R3.2 Security of System", # does the system have vulnerabilities
         "R3.3 Security of Content" # how is content ensured its not reshared ?
)
colnames(r_score) = cols

# Complete score for GISAID
r_score$Repository[1] = "GISAID"
r_score$`R1.1 Complete Metadata`[1] = 0.5
r_score$`R1.2 Techincal Documentation`[1] = 0.5
r_score$`R1.3 Quality Control`[1] = 1.0
r_score$`R1.4 Authenticity Protection`[1] = 1.0
r_score$`R2.1 Reliable Data Services`[1] = 1.0
r_score$`R3.1 Manage IP of Data Producers`[1] = 1.0
r_score$`R3.2 Security of System & Content`[1] = 0.5


# Complete score for Covid-19 Data Platform
r_score$Repository[2] = "Covid-19 Data Platform"
r_score$`R1.1 Complete Metadata`[2] = 0.5
r_score$`R1.2 Techincal Documentation`[2] = 0.5
r_score$`R1.3 Quality Control`[2] = 1.0
r_score$`R1.4 Authenticity Protection`[2] = 0
r_score$`R2.1 Reliable Data Services`[2] = 0.5
r_score$`R3.1 Manage IP of Data Producers`[2] = 0
r_score$`R3.2 Security of System & Content`[2] = 0.5

# Sum results
r_score$Score[1] = sum(r_score[1,2:8])
r_score$Score[2] = sum(r_score[2,2:8])

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"
formattable(r_score, align =c("l","c","c","c","c", "c", "c", "c", "r"), list(
  `Repository` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `R1.1 Complete Metadata`= color_tile(customGreen0, customGreen0),
  `R1.2 Techincal Documentation`= color_tile(customGreen0, customGreen0),
  `R1.3 Quality Control`= color_tile(customGreen, customGreen),
  `R1.4 Authenticity Protection`= color_tile(customRed, customGreen),
  `R2.1 Reliable Data Services`= color_tile(customGreen0, customGreen),
  `R3.1 Manage IP of Data Producers`= color_tile(customRed, customGreen),
  `R3.2 Security of System & Content`= color_tile(customGreen, customGreen0),
  `Score`= color_tile(customRed, customGreen)
))

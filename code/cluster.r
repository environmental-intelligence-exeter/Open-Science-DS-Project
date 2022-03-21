## ---------------------------
##
## Script name: cluster.r
##
## Purpose of script: Apply unsupervised learning to dataset
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

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

# Clustering Dataframe
cluster_df = main_df %>% dplyr::filter(Date == "2022/02") %>%
  select(
  c(
    Country,
    cases,
    GISAID.monthly.submissions,
    CD19DP.monthly.submissions,
    GISAID.total.Submissions,
    CD19DP.total.Submissions,
    Doses_admin,
    People_partially_vaccinated,
    People_fully_vaccinated
  )
) %>% na.omit()
cluster_df = cluster_df[-102,] # remove turkey duplicate
cluster_df[2:9] = scale(cluster_df[2:9])
cluster_df = data.frame(cluster_df[, -1], row.names = cluster_df[, 1])


#################################################################
##            Agglomerative Hierarchical Clustering            ##
#################################################################
# Calculate optimal number of clusters
fviz_nbclust(cluster_df, kmeans, method = "silhouette") # = 3
fviz_nbclust(cluster_df, kmeans, method = "wss") # = 4
fviz_nbclust(cluster_df, kmeans, method = "gap_stat") # = 9

## HCLUST
# vector of methods to compare
meth = c( "average", "single", "complete", "ward", "weighted")
names(meth) = c( "average", "single", "complete", "ward", "weighted")
# function to compute coefficient
method_test = function(x) {
  agnes(d, method = x)$ac
}
map_dbl(meth, method_test)
# average    single  complete      ward  weighted
# 0.9722815 0.9704600 0.9791666 0.9856494 0.9747848
## Congratulations, Ward - once again  you are the winner.

# vector of metrrics to compare
met = c( "euclidean", "manhattan")
names(met) = c( "euclidean", "manhattan")
# function to compute coefficient
metric_test = function(x) {
  agnes(d, metric = x, method = "ward")$ac
}
map_dbl(met, metric_test)
# euclidean manhattan
# 0.9856494 0.9856494

# Dissimilarity matrix
d = dist(cluster_df,metric = "euclidean")
# Hierarchical clustering
hc1 = hclust(d, method = "ward" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

k4 = cutree(hc1, k = 4)
k9 = cutree(hc1, k = 9)
fviz_cluster(list(data = cluster_df, cluster = k4))
fviz_cluster(list(data = cluster_df, cluster = k9))


# mat = as.matrix(cluster_df)
#
# cor = cor(cluster_df)
# corrplot(cor, order = 'hclust', addrect = 2)
#
# cluster_df$cluster_4 = k4
# cluster_df$cluster_9 = k9
#
#
# #k4
# k4_clus_1 = cluster_df %>% dplyr::filter(cluster_4 == 1)
# k4_clus_2 = cluster_df %>% dplyr::filter(cluster_4 == 2)
# k4_clus_3 = cluster_df %>% dplyr::filter(cluster_4 == 3)
# k4_clus_4 = cluster_df %>% dplyr::filter(cluster_4 == 4)
#
# k9_clus_1 = cluster_df %>% dplyr::filter(cluster_9 == 1)
# k9_clus_2 = cluster_df %>% dplyr::filter(cluster_9 == 2)
# k9_clus_3 = cluster_df %>% dplyr::filter(cluster_9 == 3)
# k9_clus_4 = cluster_df %>% dplyr::filter(cluster_9 == 4)
# k9_clus_5 = cluster_df %>% dplyr::filter(cluster_9 == 5)
# k9_clus_6 = cluster_df %>% dplyr::filter(cluster_9 == 6)
# k9_clus_7 = cluster_df %>% dplyr::filter(cluster_9 == 7)
# k9_clus_8 = cluster_df %>% dplyr::filter(cluster_9 == 8)
# k9_clus_9 = cluster_df %>% dplyr::filter(cluster_9 == 9)

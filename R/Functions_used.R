####################################################
# Functions used for cluster and realted analysis
####################################################

# Helper function for GAP statistic
mycluster_hc <- function(x, k) {
  list(cluster = cutree(hclust(as.dist(x),
                               method = "ward.D"
  ),
  k = k
  ))
}

# Function to find the statistical mode
fun_mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# Function for plotting dendrograms
fun_dendrog_pl <- function(hc,
                           optimal_nog,
                           labels,
                           hang_height = 0.001) {
  hc %>% 
    as.dendrogram() %>%
    color_branches(k = optimal_nog) %>%
    # hang.dendrogram(hang_height = hang_height) %>%
    set("labels_cex", 0.7) %>%
    dendextend::ladderize() %>%
    set("labels", labels) 
}

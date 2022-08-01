####################################################
# Functions used for cluster and realted analysis
####################################################

# Helper function for GAP statistic
mycluster_hc <- function(x, k) {
  list(cluster = cutree(hclust(as.dist(x),
                               method = "ward.D2"
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
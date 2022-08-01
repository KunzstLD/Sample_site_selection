
# TODO:
# - What's the difference between Q and D when establishing the 
# distance matrix?
# Other cluster approach? KMediods?
# Normalise values?
# Standardize the values beforehand! (not for binary/dichotomous variables)
# Constrained clustering -> e.g. spatial constraints, see Legrendre or Everitt!

# Read data and check format
test_dat <- read.csv2(file.path(data_in, "Data.csv"),
    dec = ",",
    sep = "\t"
)
rownames(test_dat) <- test_dat$Site
# str(test_dat)
# View(test_dat)
# Mostly numerical, three columns integer, which are probably categories
# test_dat[, sites := paste0("site_", 1:nrow(test_dat))]

# Create datasets for individual data types
d_Num <- Filter(is.numeric, test_dat)
d_Dich <- Filter(is.integer, test_dat)
ktab <- ktab.list.df(list(d_Num, d_Dich))
dist_mat <- dist.ktab(ktab, type = c("Q", "D"))

hc <- hclust(dist_mat, method = "ward.D")
# dend_label <- hc %>%
#     as.dendrogram() %>%
#     labels()
# plot(hc) 

# Optimal number of groups using the gap statistic
# The gap statstic, works also well when data
# fall into "one cluster" (i.e. indication that there is no cluster structure
# if this is the case)
gap <- clusGap(
    x = as.matrix(dist_mat),
    FUN = mycluster_hc,
    K.max = 10,
    B = 500
)
optimal_nog <- maxSE(gap$Tab[, "gap"],
    gap$Tab[, "SE.sim"],
    method = "Tibs2001SEmax"
)
# plot(hc, horiz = TRUE)

# Could use NbClust for a "brute force"
# approach to find the optimal number of clusters
# Maximises dissimilarity between groups and 
# minimizes dissimilarity within groups
# groups_CCA <- NbClust(
#     diss = dist_mat, distance = NULL, min.nc = 2, max.nc = 10,
#     method = "ward.D2", index = "silhouette"
# )

# Obtain "centers" of each group
# Then find those sites that are closest to the center
# Or better make a ranking of the closeness to the center

# Add groups to dataset
groups <- data.table(
    Site = names(cutree(
        hc,
        k = optimal_nog
    )),
    Group = cutree(
        hc,
        k = optimal_nog
    )
)
setDT(test_dat)
test_dat[groups, Group := i.Group, on = "Site"]
cols <- c(
    "Fertility", "Agriculture", "Examination",
    "Education", "Catholic",
    "Infant.Mortality"
)
# test_dat[, lapply(.SD, mean),
#     .SDcols = !c(
#         "Site",
#         "Season",
#         "Group"
#     ),
#     by = Group
# ]

test_dat_lf <- melt(test_dat,
    id.vars = c("Site", "Season", "Group"),
    variable.name = "Variable",
    value.name = "Value"
)
test_dat_lf[, Centroids := mean(Value), by = c("Group", "Variable")]

# Best sites in terms of difference to the centroid?
# Could use the SSE
test_dat_lf[, Diff := (Value - Centroids)^2]
test_dat_lf[, sum(Diff), by = c("Group", "Site")] %>%
    .[order(Group, V1), ]
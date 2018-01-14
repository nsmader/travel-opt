library(ggplot2)
library(combinat)

### Generate list of pairwise distances ----------------------------------------
DistLong <- function(data, id){
  nr <- nrow(data)
  dist <- as.matrix(dist(data, diag = TRUE, upper = TRUE))
  dist_l <- matrix(dist, nrow = nr^2)
  df <- data.frame(from = rep(id, times = nr),
                   to   = rep(id, each =  nr),
                   dist = dist_l)
}

### Return total distance of iterary -------------------------------------------
RouteDist <- function(route, locs, idvar = "id"){
  dists <- DistLong(data = locs, id = locs[, idvar])
  n <- length(route)
  pairs <- data.frame(from = route, to = c(route[-1], route[1]))
  travel <- merge(dists, pairs, by = c("from", "to"))
  return(sum(travel$dist))
}

### Brute force calculations ---------------------------------------------------
# /!\ There are likely ways to reduce some redundance 
# Some applicable articles are:
# * the traveling salesman problem -- https://en.wikipedia.org/wiki/Travelling_salesman_problem
# * Hamiltonian paths -- https://en.wikipedia.org/wiki/Hamiltonian_path
#   - see R's hamiltonian function -- https://www.rdocumentation.org/packages/adagio/versions/0.6.5/topics/hamiltonian
BruteOpt <- function(points, locs, idvar = "id"){
  # without loss of generality, leave off the first location to reduce permutation
  # due to different starting points
  dists <- DistLong(data = locs, id = locs[, idvar])
  routes <- permn(points[-1])
  routes <- lapply(routes, function(x) c(points[1], x, points[1]))
  route_dists <- sapply(routes, function(route) RouteDist(route, locs))
  mindist <- min(unlist(route_dists))
  opt_route <- c(routes[which(route_dists == mindist)][[1]])
  return(opt_route)
}

### Display routes -------------------------------------------------------------
DisplayRoute <- function(route, locs, x = "x", y = "y", idvar = "id"){
  locs_route <- locs[match(route, locs[, idvar]), ]
  ggplot(locs_route, aes_string(x, y)) +
    geom_label(aes_string(label = idvar)) +
    geom_path()
}


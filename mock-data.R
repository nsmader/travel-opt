# Generate locations and initial random path
n <- 6
myLocs <- data.frame(id = 1:n, x = runif(n), y = runif(n))
DisplayRoute(route = c(myLocs$id, myLocs$id[1]), locs = myLocs)

# Optimize and display route
system.time(opt_route <- BruteOpt(points = unique(loc$i), locs = myLocs))
DisplayRoute(route = opt_route, locs = myLocs)

# 

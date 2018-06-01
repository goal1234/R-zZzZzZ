library(parallel)

library(parallel)
library(MASS)
RNGkind("L'Ecuyer-CMRG")
mc.cores <- detectCores()
results <- mclapply(rep(25, 4),
function(nstart) kmeans(Boston, 4, nstart=nstart),
mc.cores=mc.cores)
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]

options(mc.cores=detectCores())
library(parallel)
cl <- makeCluster(detectCores())
clusterSetRNGStream(cl)
clusterEvalQ(cl, library(MASS))
results <- clusterApply(cl, rep(25, 4), function(nstart) kmeans(Boston, 4,
nstart=nstart))

i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]
stopCluster(cl)

cl <- makeCluster(4)
cl <- makeCluster(detectCores())
cl <- makeCluster(c("n1", "n2", "n3", "n4"))
cl <- makeCluster(4, type="FORK")

type <- if (exists("mcfork", mode="function")) "FORK" else "PSOCK"
cores <- getOption("mc.cores", detectCores())
cl <- makeCluster(cores, type=type)
results <- parLapply(cl, 1:100, sqrt)
stopCluster(cl)

RNGkind("L'Ecuyer-CMRG")
mclapply(1:2, function(i) rnorm(1))

RNGkind("L'Ecuyer-CMRG")
set.seed(7777442)
mc.reset.stream()
unlist(mclapply(1:2, function(i) rnorm(1)))

set.seed(7777442)
mc.reset.stream()
unlist(mclapply(1:2, function(i) rnorm(1)))

cl <- makeCluster(4, type = "FORK")
clusterSetRNGStream(cl, 7777442)
unlist(clusterEvalQ(cl, rnorm(1)))
clusterSetRNGStream(cl, 7777442)

unlist(clusterEvalQ(cl, rnorm(1)))
stopCluster(cl)


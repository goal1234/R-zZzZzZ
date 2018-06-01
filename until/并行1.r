install.packages('snow')

# 创建一个Clusters
library(snow)
cl <- makeCluster(4, type = 'SOCK')

# socket transport
spec <- c('n1', 'n2', 'n3', 'n4')
cl <- makeCluster(spec, type = 'SOCK')

# type :: SOCK,MPI,PVM,NWS
cl <- makeCluster(4, type = 'MPI')

# stop
stopCluster(cl)

# example kmeans
library(MASS)
result <- kmeans(Boston, 4, nstart = 100)

results <- lapply(rep(25, 4), function(nstart) kmeans(Boston, 4, nstart = nstart))
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]

# clusterApply()
# clusterApplyLB()
# parlapply()
# clusterApply()

ignore <- clusterEvalQ(cl, {library(MASS); NULL})
results <- clusterApply(cl, rep(25, 4), function(nstart) kmeans(Boston, 4, nstart = nstart))
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]

# 在集群中载入包
worker.init <- function(packages) {
    for (p in packages) {
        library(p, character.only = TRUE)
    }
    NULL
}

clusterCall(cl, worker.init, c('MASS', 'boot'))
clusterApply(cl, seq(along = cl), function(id) WORKER.ID <<- id)


# ---
set.seed(7777442)
sleeptime <- abs(rnorm(10, 10, 10))
tm <- snow.time(clusterApplyLB(cl, sleeptime, Sys.sleep))
plot(tm)

tm <- snow.time(clusterApply(cl, sleeptime, Sys.sleep))
plot(tm)

parLapply

bigsleep <- function(sleeptime, mat) Sys.sleep(sleeptime)
bigmatrix <- matrix(0, 2000, 2000)
sleeptime <- rep(1, 100)

tm <- snow.time(clusterApply(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)


tm <- snow.time(parLapply(cl ,sleeptime, bigsleep, bigmatrix))
plot(tm)


# 一个向量分成几段计算之后进行运算又重合
clusterSplit(cl, 1:30)

parVapply <- function(cl, x, fun, ...) {
    do.call('c', clusterApply(cl, clusterSplit(cl, x), fun, ...))
}

parVapply(cl, 1:10, "^", 1/3)


parLapplyLB <- function(cl, x, fun, ...) {
    clusterCall(cl, LB.init, fun, ...)
    r <- clusterApplyLB(cl, x, LB.worker)
    clusterEvalQ(cl, rm('.LB.fun','.LB.args', pos = globalenv()))
    r
}
LB.init <- function(fun, ...) {
    assign('.LB.fun',fun, pos = globalenv())
    assign('.LB.args'， list(...), pos = globalenv())
    NULL
}
LB.worker <- function(x) {
    do.call('.LB.fun', c(list(x), .LB.args))
}


bigsleep <- function(sleeptime, mat) Sys.sleep(sleeptime)
bigmatrix <- matrix(0, 2000, 2000)
sleeptime <- rep(1, 100)

tm <- snow.time(clusterApplyLB(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)

tm <- snow.time(parLapplyLB(cl, sleeptime, bigsleep, bigmatrix))
plot(tm)

a <- 1:4
x <- rnorm(4)
clusterExport(cl, 'x')
mult <- function(s) s*x
parLapply(cl, a, mult)
  
pmult <- function(cl) {
    a <- 1:4
    x <- rnorm(4)
    mult <- function(s) s*x
    parLapply(cl, a, mult)
}
pmult(cl)

pmult <- function(cl, a, x) {
x  # force x
mult <- function(s) s * x
parLapply(cl, a, mult)
}
scalars <- 1:4
dat <- rnorm(4)
pmult(cl, scalars, dat)

clusterSetupRNG(cl, type='RNGstream')
clusterSetupRNG(cl, type='SPRNG')
clusterSetupRNG(cl, type='RNGstream', seed=c(1,22,333,444,55,6))

unlist(clusterEvalQ(cl, rnorm(1)))

cl <- makeCluster(3, type="SOCK", master="192.168.1.100")
setDefaultClusterOptions(outfile="")

workerList <- list(list(host = "n1"), list(host = "n2", user = "steve"))
cl <- makeSOCKcluster(workerList)
clusterEvalQ(cl, Sys.info()[["user"]])

stopCluster(cl)

workerList <- list(list(host = "n1", outfile = "n1.log", user = "weston"),
                   list(host = "n2", outfile = "n2-1.log"),
                   list(host = "n2", outfile = "n2-2.log"))
cl <- makeSOCKcluster(workerList, user = "steve")
clusterEvalQ(cl, Sys.glob("*.log"))

stopCluster(cl)

install.packages("Rmpi")
cl <- makeCluster(4, type="MPI")
cl <- makeMPIcluster(4)
stopCluster(cl)


library(snow)
library(Rmpi)
cl <- makeMPIcluster(mpi.universe.size() - 1)
r <- clusterEvalQ(cl, R.version.string)
print(unlist(r))
stopCluster(cl)
mpi.quit()

cl <- makeCluster(2, type="SOCK", manual=TRUE, outfile="")
cl <- makeCluster(c('n1', 'n2'), type="SOCK", manual=TRUE, outfile="")


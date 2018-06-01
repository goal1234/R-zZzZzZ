install.packages("multicore")
library(multicore)

library(multicore)
library(MASS)
results <- mclapply(rep(25, 4), function(nstart) kmeans(Boston, 4, nstart=nstart))
i <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(i)]]

unique(unlist(mclapply(1:100, function(i) Sys.getpid(), mc.cores = 2)))

options(cores = 3)
unique(unlist(mclapply(1:100, function(i) Sys.getpid())))
mclapply(1:3, function(i) rnorm(3), mc.cores = 3, mc.set.seed = FALSE)

rnorm(1)
mclapply(1:3, function(i) rnorm(3), mc.cores = 3, mc.set.seed = FALSE)

set.seed(7777442)
mclapply(1:3, function(i) rnorm(3), mc.cores = 3, mc.set.seed = TRUE)


set.seed(93564990)
sleeptime <- abs(rnorm(10, 10, 10))
system.time(mclapply(sleeptime, Sys.sleep, mc.cores = 4))

system.time(mclapply(sleeptime, Sys.sleep, mc.cores = 4, mc.preschedule = FALSE))
x <- 1:10
pvec(x, "^", 1/3)

library(multicore)
fun1 <- function() {Sys.sleep(10); 1}
fun2 <- function() {Sys.sleep(5);  2}
fun3 <- function() {Sys.sleep(1);  3}

f1 <- parallel(fun1())
f2 <- parallel(fun2())
f3 <- parallel(fun3())
collect(list(f1, f2, f3))

f1 <- parallel(fun1())
f2 <- parallel(fun2())
f3 <- parallel(fun3())
collect(list(f1, f2, f3), wait=FALSE)

Sys.sleep(15)
collect(list(f1, f2, f3), wait=FALSE)

collect(list(f1, f2, f3), wait=FALSE)
collect(list(f1, f2, f3), wait=FALSE)


f1 <- parallel(fun1())
f2 <- parallel(fun2())
f3 <- parallel(fun3())
collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)

collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)
collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)
collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)
collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)
collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)
collect(list(f1, f2, f3), wait=FALSE, timeout=1000000)

library(snow)
nw <- 3
seed <- 7777442
kind <- 0
para <- 0

f1 <- parallel({
    initSprngNode(0, nw, seed, kind, para)
    rnorm(1)
})
f2 <- parallel({
    initSprngNode(1, nw, seed, kind, para)
    rnorm(1)
})
f3 <- parallel({
    initSprngNode(2, nw, seed, kind, para)
    rnorm(1)
})
unlist(collect(list(f1, f2, f3)), use.names = FALSE)


cl <- makeCluster(3, type = "SOCK")
seed <- 7777442
clusterSetupSPRNG(cl, seed = seed)
unlist(clusterEvalQ(cl, rnorm(1)), use.names = FALSE)
stopCluster(cl)


mclapply.init <- function(X, FUN, ..., mc.cores=4, mc.init=NULL) {
cores <- max(min(mc.cores, length(X)), 1)
ix <- lapply(1:cores, function(i) seq(i, length(X), by=cores))
forkloop <- function(core) {
proc <- fork()
if (inherits(proc, "masterProcess")) {
    sendMaster(tryCatch({
        suppressWarnings(rm(".Random.seed", pos=.GlobalEnv))
        if (is.function(mc.init))
            mc.init(core, cores)
            lapply(X[ix[[core]]], FUN, ...)
        },
        error=function(e) {
            lapply(ix[[core]], function(i) e)
        }))
        exit(0)
    }
    proc$pid
}
pids <- sapply(1:cores, forkloop)
results <- vector("list", length(X))
while (! is.null(ready <- selectChildren(pids, 1))) {
    if (is.integer(ready)) {
        for (pid in ready) {
            data <- readChild(pid)
        if (is.raw(data)) {
            core <- which(pid == pids)
            results[ix[[core]]] <- unserialize(data)
            }
        }
    }
}
names(results) <- names(X)
results
}


set.worker.id <- function(id, cores) {
    assign(".MC.WORKER.ID", id, pos = .GlobalEnv)
}

mclapply.init(11:13, function(i) c(i, .MC.WORKER.ID), mc.cores = 2, mc.init = set.worker.id)


set.worker.seed <- function(id, cores) {
    set.seed(Sys.getpid())
}

mclapply.init(1:3, function(i) rnorm(1), mc.init = set.worker.seed)


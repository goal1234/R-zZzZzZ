pred_sigma <- sqrt(sigma^2 + apply((T%*%posterior_sigma)*T, MARGIN=1,
FUN=sum))
upper_bound <- T%*%posterior_beta + qnorm(0.95)*pred_sigma
lower_bound <- T%*%posterior_beta - qnorm(0.95)*pred_sigma


# Probabilistic Graphical Models (PGM for short) are exactly that: you want to
# describe relationships between variables


# This principle of updating a probabilistic model was frst discovered by Thomas
# Bayes and publish by his friend Richard Price in 1763 in the now famous An Essay
# toward solving a Problem in the Doctrine of Chances

# 利用概率来管理你的直觉
# Probability theory is nothing but common sense reduced to calculation


prior <- c(working = 0.99, broken = 0.01)
likelihood <- rbind(
            working = c(good=0.99, bad=0.01), broken = c(good=0.6,
            bad=0.4))
data <- c("bad","bad","bad","bad")


bayes <- function(prior, likelihood, data)
{
posterior <- matrix(0, nrow=length(data), ncol=length(prior))
dimnames(posterior) <- list(data, names(prior))
initial_prior <- prior
for(i in 1:length(data))
{
posterior[i, ] <-
prior*likelihood[ , data[i]]/
sum(prior * likelihood[ , data[i]])
prior <- posterior[i , ]
}
return(rbind(initial_prior,posterior))
}

matplot( bayes(prior,likelihood,data), t='b', lty=1, pch=20,
col=c(3,2))

prior <- c(working = 0.5, broken = 0.5)

matplot( bayes(prior,likelihood,data), t='b', lty=1, pch=20,
col=c(3,2))

prior=c(working=0.99,broken=0.01)
data=c("bad","good","good","good","good","good","good","good","good","go
od")
matplot(bayes(prior,likelihood,data),t='b',pch=20,col=c(3,2))



##########################################
#########################################

source("http://bioconductor.org/biocLite.R")
biocLite()
install.packages("gRain")
library("gRbase")


graph <- ug("A:B:E + C:E:D")
class(graph)

install.packages("Rgraphviz")
plot(graph)

dag <- dag("A + B:A + C:B + D:B + E:C:D")
dag
plot(dag)


machine_val <- c("working","broken")
light_bulb_val <- c("good","bad")

machine_prob <- c(99,1)
light_bulb_prob <- c(99,1,60,40)

M <- cptable(~machine, values=machine_prob, levels=machine_val)
L <- cptable(~light_bulb|machine, values=light_bulb_prob, levels=light_
bulb_val)


plist <- compileCPT(list(M,L))
plist

plist$machine
plist$light_bulb

plist$machine
plist$light_bulb

net <- grain(plist)
net2 <- setEvidence(net, evidence=list(light_bulb="bad"))
querygrain(net2, nodes=c("machine"))

# putting questions and queries to the model


• Building graphical models
• Variable elimination
• Sum-product and belief updates
• The junction tree algorithm


A=matrix(c(.8,.2),2,1)
B=matrix(c(.6,.4,.3,.7),2,2)
C=matrix(c(.5,.5,.8,.8),2,2)
D=matrix(c(.3,.7,.4,.6),2,2)

library(gRain)
val=c("true","false")
F = cptable(~F, values=c(10,90),levels=val)
C = cptable(~C|F, values=c(10,90,20,80),levels=val)
E = cptable(~E|F, values=c(50,50,30,70),levels=val)
A = cptable(~A|C, values=c(50,50,70,30),levels=val)
D = cptable(~D|E, values=c(60,40,70,30),levels=val)
B = cptable(~B|A:D, values=c(60,40,70,30,20,80,10,90),levels=val)


plist = compileCPT(list(F,E,C,A,D,B))
plist
print(plist$F)
print(plist$B)

jtree = grain(plist)

querygrain(jtree, nodes=c("F"), type="marginal")
querygrain(jtree, nodes=c("C"), type="marginal")
querygrain(jtree, nodes=c("B"), type="marginal")
querygrain(jtree, nodes=c("A","B"), type="joint")
querygrain(jtree, nodes=c("A","B","C"), type="joint")
jtree2 = setEvidence(jtree, evidence=list(F="true"))
querygrain(jtree, nodes=c("F"), type="marginal")
querygrain(jtree2, nodes=c("F"), type="marginal")

querygrain(jtree, nodes=c("A"), type="marginal")
querygrain(jtree2, nodes=c("A"), type="marginal")
querygrain(jtree, nodes=c("B"), type="marginal")

querygrain(jtree2, nodes=c("B"), type="marginal")
jtree3 = setEvidence(jtree, evidence=list(F="true",A="false"))
querygrain(jtree, nodes=c("C"), type="marginal")
querygrain(jtree2, nodes=c("C"), type="marginal")
querygrain(jtree3, nodes=c("C"), type="marginal")


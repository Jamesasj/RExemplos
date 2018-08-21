library(CRF)

n.nodes <- 10
n.states <- 2
prior.prob <- c(0.8, 0.2)
trans.prob <- matrix(0, nrow=2, ncol=2)
trans.prob[1,] <- c(0.95, 0.05)
trans.prob[2,] <- c(0.05, 0.95)

# produzida uma matriz d
adj <- matrix(0, n.nodes, n.nodes)
for (i in 1:(n.nodes-1))
{
  adj[i, i+1] <- 1
}

mc <- make.crf(adj, n.states)

mc$node.pot[1,] <- prior.prob
for (i in 1:mc$n.edges)
{
  mc$edge.pot[[i]] <- trans.prob
}

mc.samples <- sample.chain(mc, 10000)
mc.samples[1:10,]


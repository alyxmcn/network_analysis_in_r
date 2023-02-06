## ----setup, include=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message=FALSE)
knitr::opts_chunk$set(warning=FALSE)


## --------------------------------------------------------------------------------------------------
library(igraph)


## --------------------------------------------------------------------------------------------------
m = matrix(data = sample(0:1, 25, replace = TRUE), nrow = 5, ncol = 5)
colnames(m) = c("Joe", "Jill", "Jeff", "Maya", "Rachel")
rownames(m) = c("Costa", "Starbucks", "Cafe2U", "Petes", "Philz")


## --------------------------------------------------------------------------------------------------
bg = graph.incidence(m)


## --------------------------------------------------------------------------------------------------
V(bg)$type 
V(bg)$name
shape = ifelse(V(bg)$type, "circle", "square") # assign shape by node type
col = ifelse(V(bg)$type, "red", "yellow") # assign color by node type


## --------------------------------------------------------------------------------------------------
plot(bg, vertex.shape = shape, vertex.color = col, vertex.size = 30)


## --------------------------------------------------------------------------------------------------
m_row <- m %*% t(m) #creates adjacency matrix of rows
m_column <- t(m) %*% m #creates adjacency matrix of columns


## --------------------------------------------------------------------------------------------------
bg_row <- graph_from_adjacency_matrix(m_row,   
                          mode = "undirected",
                          diag = FALSE,
                          weighted=TRUE)
e<-E(bg_row)$weight

plot(bg_row, vertex.shape = shape, 
     vertex.color = col, vertex.size = 30,
     edge.width = e)

bg_column <- graph_from_adjacency_matrix(m_column,
                            mode = "undirected",
                             diag = FALSE, 
                            weighted=TRUE)
e1<-E(bg_column)$weight

plot(bg_column, vertex.shape = shape, 
     vertex.color = col, vertex.size = 40,
     edge.width = e1)


## --------------------------------------------------------------------------------------------------
library(readr)


## --------------------------------------------------------------------------------------------------
df = read.csv("data/bimodal_Committees & People.csv", header = T, sep = ";")


## --------------------------------------------------------------------------------------------------
mat = as.matrix(table(df))


## --------------------------------------------------------------------------------------------------
bg_com = graph.incidence(mat)


## ----include=FALSE---------------------------------------------------------------------------------
V(bg_com)$type 
V(bg_com)$name


## --------------------------------------------------------------------------------------------------
shape = ifelse(V(bg_com)$type, "square", "circle") # assign shape by node type
col = ifelse(V(bg_com)$type, "tomato3", "lightblue") # assign color by node type


## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
degree_com = degree(bg_com)

plot(bg_com, 
     vertex.shape = shape, 
     vertex.color = col, 
     vertex.size = ifelse(shape == "square", degree_com/2, 4),
     vertex.label = ifelse(shape=="circle", NA, V(bg_com)$name),
     vertex.label.color = "black",
     main = "Members of commitees network")

## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
degree_com = degree(bg_com)

plot(bg_com,
     layout = layout_as_bipartite(bg_com),
     vertex.shape = shape, 
     vertex.color = col, 
     vertex.size = ifelse(shape == "square", degree_com/2, 4),
     vertex.label = NA,
     vertex.label.color = "black",
     main = "Members of commitees network\n(bipartite layout)")

## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
degree_com = degree(bg_com)

plot(bg_com,
     layout = layout_with_kk(bg_com),
     vertex.shape = shape, 
     vertex.color = col, 
     vertex.size = ifelse(shape == "square", degree_com/2, 4),
     vertex.label = ifelse(shape=="circle", NA, V(bg_com)$name),
     vertex.label.color = "black",
     main = "Members of commitees network\n(Kamada-Kawai layout)")


## --------------------------------------------------------------------------------------------------
m_row <- mat %*% t(mat) 
m_column <- t(mat) %*% mat 


## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
bg_row <- graph_from_adjacency_matrix(m_row,   
                          mode = "undirected",
                          diag = FALSE,
                          weighted=TRUE)
e<-E(bg_row)$weight

plot(bg_row, vertex.shape = shape,
     vertex.label = NA,
     layout = layout_with_fr(bg_row),
     vertex.color = col, vertex.size = 5,
     edge.width = e,
     main="Network of committee members")

## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
bg_column <- graph_from_adjacency_matrix(m_column,
                            mode = "undirected",
                             diag = FALSE, 
                            weighted=TRUE)
e1<-E(bg_column)$weight

plot(bg_column, vertex.shape = "square",
     vertex.label.color = "black",
     vertex.color = "tomato3", vertex.size = 7,
     edge.width = e1,
     main="Network of committees")


## --------------------------------------------------------------------------------------------------
vk = read_csv("data/vk_affilation_matrix.csv")


## --------------------------------------------------------------------------------------------------
# Data preprocessing -----
vk_df = vk
vk_df$groups = NULL
rownames(vk_df) = vk$groups


## --------------------------------------------------------------------------------------------------
M = as.matrix(vk_df) # a matrix to convert to a bipartite network


## --------------------------------------------------------------------------------------------------
bg = graph.incidence(M) # a bipartite network


## ----include=FALSE---------------------------------------------------------------------------------
V(bg)$type
V(bg)$name


## --------------------------------------------------------------------------------------------------
shape = ifelse(V(bg)$type, "circle", "square") # assign shape by node type
col = ifelse(V(bg)$type, "#aeb0d4", "burlywood") # assign color by node type


## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5) 


## --------------------------------------------------------------------------------------------------
degree_vk = degree(bg)
summary(degree_vk)
q1 = quantile(degree_vk, 0.95) # 41.8
q2 = quantile(degree_vk, 0.98) # 55.64
q3 = quantile(degree_vk, 0.99) # 80.48
hist(degree_vk, col = 0, main = "VK subscriptions degree", xlab = "")
abline(v = mean(degree_vk), col="red", lwd=3, lty=2)
abline(v = median(degree_vk), col="blue", lwd=3, lty=2)
abline(v = q1, col=1, lwd=3, lty=2)
abline(v = q2, col="lightblue", lwd=3, lty=2)
abline(v = q3, col="lightsalmon", lwd=3, lty=2)
axis(side=1, at=seq(0, 150, by=20))


## --------------------------------------------------------------------------------------------------
edge.weights <- function(community, network, weight.within = 100, weight.between = 1) {
bridges <- crossing(communities = community, graph = network)
weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
return(weights) 
}


## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
layout = layout_with_fr(bg)
community = cluster_louvain(bg)


## --------------------------------------------------------------------------------------------------
E(bg)$weight = edge.weights(community, bg, weight.within = 50) # weight.within is a positional argument to change distance between communities detected by by multi-level optimization of modularity with the cluster_louvain method

plot(bg,
     vertex.size = ifelse(shape == "square", degree_vk/15, 2),
     vertex.shape = shape, 
     vertex.color = col, 
     vertex.size = 10,
     vertex.label = ifelse(degree_vk >= 80 & shape == "square", V(bg)$name, NA),
     vertex.label.color = 'black',
     vertex.label.cex = degree_vk/70,
     vertex.label.font = 2,
     edge.width = .1,
     edge.color = "lightgrey",
     main = "VK friends' public groups subscriptions")


## --------------------------------------------------------------------------------------------------
m_row <- M %*% t(M) 
m_column <- t(M) %*% M 


## --------------------------------------------------------------------------------------------------
bg_row <- graph_from_adjacency_matrix(m_row,   
                          mode = "undirected",
                          diag = FALSE,
                          weighted=TRUE)


## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
e<-E(bg_row)$weight

degree_vk_row = degree(bg_row)

plot(bg_row, 
     vertex.shape = shape, 
     layout = layout_nicely(bg_row),
     vertex.color = col,
     vertex.label = ifelse(degree_vk_row >= 90, V(bg_row)$name, NA),
     vertex.label.size = degree_vk_row,
     vertex.size = ifelse(degree_vk_row >= 90, degree_vk_row/10, degree_vk_row/20),
     vertex.label.color = "black",
     edge.width = e/1000,
     edge.color = "grey",
     main = "VK groups network")


## --------------------------------------------------------------------------------------------------
bg_column <- graph_from_adjacency_matrix(m_column,
                            mode = "undirected",
                            diag = FALSE, 
                            weighted = TRUE)


## --------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=10, fig.height=10) 


## --------------------------------------------------------------------------------------------------
e1<-E(bg_column)$weight

plot(bg_column, 
     layout = layout_with_kk(bg_column),
     vertex.shape = "circle", 
     vertex.color = "#aeb0d4", 
     vertex.size = 5,
     edge.width = e1/2,
     vertex.label = NA,
     main = "VK friends network")


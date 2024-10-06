library(igraph)
library(ggplot2)
library(RColorBrewer)

coops2016 <- read.csv("coauthorship2016_graph.csv", header = TRUE,sep = ",")
coops2017 <- read.csv("coauthorship2017_graph.csv", header = TRUE,sep = ",")
coops2018 <- read.csv("coauthorship2018_graph.csv", header = TRUE,sep = ",")
coops2019 <- read.csv("coauthorship2019_graph.csv", header = TRUE,sep = ",")
coops2020 <- read.csv("coauthorship2020_graph.csv", header = TRUE,sep = ",")

graph2016 <-graph_from_data_frame(d=coops2016,directed = FALSE)
graph2017 <-graph_from_data_frame(d=coops2017,directed = FALSE)
graph2018 <-graph_from_data_frame(d=coops2018,directed = FALSE)
graph2019 <-graph_from_data_frame(d=coops2019,directed = FALSE)
graph2020 <-graph_from_data_frame(d=coops2020,directed = FALSE)


#### counting vertices ####
v16 <- vcount(graph2016)
v17 <- vcount(graph2017)
v18 <- vcount(graph2018)
v19 <- vcount(graph2019)
v20 <- vcount(graph2020)

#### counting edges ####
e16 <- ecount(graph2016)
e17 <- ecount(graph2017)
e18 <- ecount(graph2018)
e19 <- ecount(graph2019)
e20 <- ecount(graph2020)

#### calculating average degrees ####
avg_degree16 <- mean(degree(graph2016))
avg_degree17 <- mean(degree(graph2017))
avg_degree18 <- mean(degree(graph2018))
avg_degree19 <- mean(degree(graph2019))
avg_degree20 <- mean(degree(graph2020))


avg_degree16 ; avg_degree17 ;avg_degree18 ; avg_degree19 ; avg_degree20

diam16 <-diameter(graph2016, directed = FALSE, weights = E(graph2016)$weight)
diam17<- diameter(graph2017, directed = FALSE, weights = E(graph2017)$weight)
diam18<- diameter(graph2018, directed = FALSE, weights = E(graph2018)$weight)
diam19<- diameter(graph2019, directed = FALSE, weights = E(graph2019)$weight)
diam20 <- diameter(graph2020, directed = FALSE, weights = E(graph2020)$weight)

diam16 ; diam17 ; diam18 ; diam19 ; diam20


#### visualizing edges and vertices evolution ####
years <- 2016:2020
vnum <- c(v16, v17, v18, v19, v20)
enum <- c(e16, e17, e18, e19, e20)

ev_num <- data.frame(
  Year = rep(years, 2),
  Count = c(vnum, enum),
  Type = rep(c("Number of Vertices", "Number of Edges"), each = length(years))
)


ggplot(ev_num, aes(x = Year, y = Count, color = Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Number of Vertices" = "blue", "Number of Edges" = "red")) +
  labs(title = "Evolution of the Number of Vertices and Edges during 2016-2020",
       x = "Year",
       y = "Count",
       color = "Metric") + ylim(1200,15000)+  scale_y_continuous(breaks = seq(2000, 14000, 1000))
  theme_minimal()

  
#### visualizing average degree evolution ####
avgdegree_all <- c(avg_degree16, avg_degree17, avg_degree18, avg_degree19, avg_degree20)
avgdegree_evol <- cbind.data.frame(years,avgdegree_all)
barplot(avgdegree_evol$avgdegree_all~factor(avgdegree_evol$years),ylim = c(1.5, 6), ylab = "Avg. Degree", xlab = "Years", col = "lightblue",xpd=FALSE, main="Average (Simple) Degree \n Evolution over 2016-2020")

#### visualizing diameter evolution ####
diam_all <- c(diam16, diam17, diam18, diam19, diam20)
diam_evol <- cbind.data.frame(years,diam_all)
diam_evol
plot(diam_evol$diam_all ~ diam_evol$years, type = "l", col = "darkred", ylim = c(5, 25), ylab = "Diameter", xlab = "Years", main="Evolution of the (weighted)\n longest-shortest path over 2016-2020")
points(diam_evol$diam_all ~ diam_evol$years, col = "firebrick2", pch = 20, cex = 1.5)



##### Finding the authors with the highest degree for each year ####

deg2016 <- degree(graph2016, mode = "all")
deg2017 <- degree(graph2017, mode = "all")
deg2018 <- degree(graph2018, mode = "all")
deg2019 <- degree(graph2019, mode = "all")
deg2020 <- degree(graph2020, mode = "all")

get_top10_authors <- function(degree_vector, year) {
  top10_indices <- order(degree_vector, decreasing = TRUE)[1:10]
  top10_degrees <- degree_vector[top10_indices]
  top10_authors <- names(degree_vector)[top10_indices]
  data.frame(
    Rank = 1:10,
    Author = top10_authors,
    Degree = top10_degrees,
    Year = year
  )
}

top10_2016 <- get_top10_authors(deg2016, 2016)
top10_2017 <- get_top10_authors(deg2017, 2017)
top10_2018 <- get_top10_authors(deg2018, 2018)
top10_2019 <- get_top10_authors(deg2019, 2019)
top10_2020 <- get_top10_authors(deg2020, 2020)

print("Top-10 authors in 2016:")
print(top10_2016)

print("Top-10 authors in 2017:")
print(top10_2017)

print("Top-10 authors in 2018:")
print(top10_2018)

print("Top-10 authors in 2019:")
print(top10_2019)

print("Top-10 authors in 2020:")
print(top10_2020)


##### Finding the authors with the highest pagerank for each year ####

pr2016 <- page_rank(graph2016)$vector
pr2017 <- page_rank(graph2017)$vector
pr2018 <- page_rank(graph2018)$vector
pr2019 <- page_rank(graph2019)$vector
pr2020 <- page_rank(graph2020)$vector

get_top10_authors_pagerank <- function(pagerank_vector, year) {
  top10_indices <- order(pagerank_vector, decreasing = TRUE)[1:10]
  top10_pageranks <- pagerank_vector[top10_indices]
  top10_authors <- names(pagerank_vector)[top10_indices]
  data.frame(
    Rank = 1:10,
    Author = top10_authors,
    PageRank = top10_pageranks,
    Year = year
  )
}

top10_pr2016 <- get_top10_authors_pagerank(pr2016, 2016)
top10_pr2017 <- get_top10_authors_pagerank(pr2017, 2017)
top10_pr2018 <- get_top10_authors_pagerank(pr2018, 2018)
top10_pr2019 <- get_top10_authors_pagerank(pr2019, 2019)
top10_pr2020 <- get_top10_authors_pagerank(pr2020, 2020)

print("Top-10 authors by PageRank in 2016:")
print(top10_pr2016)

print("Top-10 authors by PageRank in 2017:")
print(top10_pr2017)

print("Top-10 authors by PageRank in 2018:")
print(top10_pr2018)

print("Top-10 authors by PageRank in 2019:")
print(top10_pr2019)

print("Top-10 authors by PageRank in 2020:")
print(top10_pr2020)



apply_clustering_algorithms <- function(graph) {
  list(
    fast_greedy = cluster_fast_greedy(graph),
    infomap = cluster_infomap(graph),
    louvain = cluster_louvain(graph)
  )
}

clustering_2016 <- apply_clustering_algorithms(graph2016)
clustering_2017 <- apply_clustering_algorithms(graph2017)
clustering_2018 <- apply_clustering_algorithms(graph2018)
clustering_2019 <- apply_clustering_algorithms(graph2019)
clustering_2020 <- apply_clustering_algorithms(graph2020)

extract_clustering_results <- function(clustering_results) {
  data.frame(
    Algorithm = c("Fast Greedy", "Infomap", "Louvain"),
    Num_Communities = sapply(clustering_results, function(x) length(sizes(x))),
    Modularity = sapply(clustering_results, function(x) modularity(x))
  )
}

results_2016 <- extract_clustering_results(clustering_2016)
results_2017 <- extract_clustering_results(clustering_2017)
results_2018 <- extract_clustering_results(clustering_2018)
results_2019 <- extract_clustering_results(clustering_2019)
results_2020 <- extract_clustering_results(clustering_2020)

print(results_2016)
print(results_2017)
print(results_2018)
print(results_2019)
print(results_2020)


find_author_community <- function(graph, author_name) {
  communities <- cluster_louvain(graph)
  membership <- communities$membership
  author_vertex <- which(V(graph)$name == author_name)
  
  if (length(author_vertex) == 0) {
    return(NA)
  }
  
  return(membership[author_vertex])
}

community_2016 <- find_author_community(graph2016, "Philip S. Yu")
community_2017 <- find_author_community(graph2017, "Philip S. Yu")
community_2018 <- find_author_community(graph2018, "Philip S. Yu")
community_2019 <- find_author_community(graph2019, "Philip S. Yu")
community_2020 <- find_author_community(graph2020, "Philip S. Yu")

community_evolution <- data.frame(
  Year = c(2016, 2017, 2018, 2019, 2020),
  Community = c(community_2016, community_2017, community_2018, community_2019, community_2020)
)
community_evolution


find_author_community_members <- function(graph, author_name) {
  communities <- cluster_louvain(graph)
  membership <- communities$membership
  author_vertex <- which(V(graph)$name == author_name)
  
  if (length(author_vertex) == 0) {
    return(NA)
  }
  
  author_community <- membership[author_vertex]
  community_members <- V(graph)$name[which(membership == author_community)]
  
  return(community_members)
}

community_members_2016 <- find_author_community_members(graph2016, "Philip S. Yu")
community_members_2017 <- find_author_community_members(graph2017, "Philip S. Yu")
community_members_2018 <- find_author_community_members(graph2018, "Philip S. Yu")
community_members_2019 <- find_author_community_members(graph2019, "Philip S. Yu")
community_members_2020 <- find_author_community_members(graph2020, "Philip S. Yu")

print(list("2016" = community_members_2016))
print(list("2017" = community_members_2017))
print(list("2018" = community_members_2018))
print(list("2019" = community_members_2019))
print(list("2020" = community_members_2020))

common_members <- Reduce(intersect, list(community_members_2016, community_members_2017, community_members_2018, community_members_2019, community_members_2020))
common_members
Reduce(intersect, list(community_members_2016, community_members_2017))
Reduce(intersect, list(community_members_2017, community_members_2018))
Reduce(intersect, list(community_members_2018, community_members_2019))
Reduce(intersect, list(community_members_2019, community_members_2020))

jaccard_sim <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  return(intersection / union)
}

jaccard_2016_2017 <- jaccard_sim(community_members_2016, community_members_2017)
jaccard_2017_2018 <- jaccard_sim(community_members_2017, community_members_2018)
jaccard_2018_2019 <- jaccard_sim(community_members_2018, community_members_2019)
jaccard_2019_2020 <- jaccard_sim(community_members_2019, community_members_2020)


jaccard_2016_2017 ; jaccard_2017_2018 ; jaccard_2018_2019 ; jaccard_2019_2020



louvain_communities2016 <- cluster_louvain(graph2016)
louvain_communities2017 <- cluster_louvain(graph2017)
louvain_communities2018 <- cluster_louvain(graph2018)
louvain_communities2019 <- cluster_louvain(graph2019)
louvain_communities2020 <- cluster_louvain(graph2020)

sizes16 <- sizes(louvain_communities2016)
sizes17 <- sizes(louvain_communities2017)
sizes18 <- sizes(louvain_communities2018)
sizes19 <- sizes(louvain_communities2019)
sizes20 <- sizes(louvain_communities2020)

sizes16
hist(sizes16)
boxplot(sizes16)
sizes17
boxplot(sizes17)
boxplot(sizes18)
boxplot(sizes19)
boxplot(sizes20)
median(sizes16);median(sizes17);median(sizes18);median(sizes19);median(sizes20)
mean(sizes16);mean(sizes17);mean(sizes18);mean(sizes19);mean(sizes20)

small_threshold <- 30
large_threshold <- 55

valid_communities16 <- which(sizes16 >= small_threshold & sizes16 <= large_threshold)
valid_communities17 <- which(sizes17 >= small_threshold & sizes17 <= large_threshold)
valid_communities18 <- which(sizes18 >= small_threshold & sizes18 <= large_threshold)
valid_communities19 <- which(sizes19 >= small_threshold & sizes19 <= large_threshold)
valid_communities20 <- which(sizes20 >= small_threshold & sizes20 <= large_threshold)

membership16 <- membership(louvain_communities2016)
membership17 <- membership(louvain_communities2017)
membership18 <- membership(louvain_communities2018)
membership19 <- membership(louvain_communities2019)
membership20 <- membership(louvain_communities2020)

valid_nodes16 <- which(membership16 %in% valid_communities16)
valid_nodes17 <- which(membership17 %in% valid_communities17)
valid_nodes18 <- which(membership18 %in% valid_communities18)
valid_nodes19 <- which(membership19 %in% valid_communities19)
valid_nodes20 <- which(membership20 %in% valid_communities20)

subgraph16 <- induced_subgraph(graph2016, vids = valid_nodes16)
subgraph17 <- induced_subgraph(graph2017, vids = valid_nodes17)
subgraph18 <- induced_subgraph(graph2018, vids = valid_nodes18)
subgraph19 <- induced_subgraph(graph2019, vids = valid_nodes19)
subgraph20 <- induced_subgraph(graph2020, vids = valid_nodes20)

V(subgraph16)$community <- membership16[valid_nodes16]
V(subgraph17)$community <- membership17[valid_nodes17]
V(subgraph18)$community <- membership18[valid_nodes18]
V(subgraph19)$community <- membership19[valid_nodes19]
V(subgraph20)$community <- membership20[valid_nodes20]


num_communities16 <- length(valid_communities16)
num_communities17 <- length(valid_communities17)
num_communities18 <- length(valid_communities18)
num_communities19 <- length(valid_communities19)
num_communities20 <- length(valid_communities20)

num_colors <- max(num_communities16, num_communities17, num_communities18, num_communities19, num_communities20)
colors <- brewer.pal(min(num_colors, 12), "Set3")
if (num_colors > 12) {
  colors <- colorRampPalette(brewer.pal(12, "Set3"))(num_colors) # Generate more colors if needed
}


colors16 <- sapply(membership16[valid_nodes16], function(x) colors[which(valid_communities16 == x)])
colors17 <- sapply(membership17[valid_nodes17], function(x) colors[which(valid_communities17 == x)])
colors18 <- sapply(membership18[valid_nodes18], function(x) colors[which(valid_communities18 == x)])
colors19 <- sapply(membership19[valid_nodes19], function(x) colors[which(valid_communities19 == x)])
colors20 <- sapply(membership20[valid_nodes20], function(x) colors[which(valid_communities20 == x)])


par(mfrow=c(1,3))
plot(subgraph16, vertex.color = colors16, main = "Subgraph 2016", vertex.label=NA,vertex.size=3.5,edge.width=3)
plot(subgraph17, vertex.color = colors17, main = "Subgraph 2017", vertex.label=NA,vertex.size=3.5,edge.width=3)
plot(subgraph18, vertex.color = colors18, main = "Subgraph 2018", vertex.label=NA,vertex.size=3.5,edge.width=3)
par(mfrow=c(1,2))
plot(subgraph19, vertex.color = colors19, main = "Subgraph 2019", vertex.label=NA,vertex.size=3.5,edge.width=3)
plot(subgraph20, vertex.color = colors20, main = "Subgraph 2020", vertex.label=NA,vertex.size=3.5,edge.width=3)

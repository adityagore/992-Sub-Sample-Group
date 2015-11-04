library(data.table)  # so fast!
library(igraph)
drug <- fread("PARTD_PRESCRIBER_PUF_NPI_DRUG_13.tab",colClasses = c(rep("character",9),"integer",
      "integer","integer","numeric","integer","character","integer","character","integer","numeric"))
setkey(drug,NPI)
wi.data <- drug[NPPES_PROVIDER_STATE=="WI"]
Et = fread("dataFiles/physicianReferral/physician-referrals-2015-days365.txt",sep = ",",  colClasses = c("character", "character","numeric", "numeric", "numeric"))
setkey(Et, V1)
b= c(rep("character", 6),rep("factor",4), "numeric", rep("factor",6), "character", "character", "character", "numeric", rep("character",2), "factor", "character", "factor", "character", rep("character", 10), rep("factor", 6))
DT = fread("dataFiles/physicianReferral/National_Downloadable_File.csv",colClasses = b)
setkey(DT, NPI)
rm(b)
tmp <- Et[unique(wi.data$NPI)]
Ewi <- tmp[complete.cases(tmp)]
el<-as.matrix(Ewi)[,1:2]
g<-graph.edgelist(el,directed = F)
g <- simplify(g)
core <- graph.coreness(g)
hist(core)
g1 = induced.subgraph(graph = g,vids = V(g)[core>10])
plot(g1)
wigraph = induced.subgraph(graph = g, vids = which(V(g)$name %in% unique(wi.data$NPI)))
core = graph.coreness(wigraph)
sum(core > 1)
wigraph = induced.subgraph(graph = wigraph,vids = (core > 1))
plot(wigraph, vertex.label= NA) 
clust = clusters(wigraph)
names(clust)
clust$csize
tmp = induced.subgraph(graph = wigraph,vids = (clust$mem ==9))   
plot(tmp)

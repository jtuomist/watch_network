library(jsonlite)
library(DiagrammeR)
library(OpasnetUtils)
library(RCy3)

objects.latest("Op_en3861", "makeGraph") # [[Insight network]] makeGraph

plan <- "hnh2035" # Identifier of the action plan

aplansGraph <- function(plan, ...) {
  require(OpasnetUtils)
  require(DiagrammeR)
  
  if(!exists("formatted")){
    objects.latest("Op_en3861", code_name="formatted") # [[Insight network]] formatted
  }
  if(!exists("chooseGr")) {
    objects.latest("Op_en3861", code_name="chooseGr") # [[Insight network]] chooseGr
  }
  
  dat <- fromJSON(paste0("https://aplans.api.hel.ninja/v1/insight/?plan=", plan))
  
  nodes <- dat$nodes[,-9] # remove the most recent values because the column is a data.frame
  edges <- dat$edges
  nodes$id <- as.numeric(gsub("a", "1", gsub("i","2",nodes$id))) # id must be numeric
  edges$from <- as.numeric(gsub("a", "1", gsub("i","2",edges$from)))
  edges$to <- as.numeric(gsub("a", "1", gsub("i","2",edges$to)))
  edges$id <- 1:nrow(edges)
  colnames(edges)[colnames(edges)=="effect_type"] <- "rel"
  nodes$label <- substr(nodes$name, 1, 30)
  nodes$type <- paste(nodes$indicator_level, "indicator")
  nodes$type[nodes$type=="NA indicator"] <- "action"
  
  nodes <- merge(nodes, formatted[!grepl("edge.", colnames(formatted))],
                 by.x="type", by.y="Resource", all.x=TRUE)
  colnames(nodes) <- gsub("node.","",colnames(nodes))

  edges <- merge(edges, formatted[!grepl("node.", colnames(formatted))],
                 by.x="rel", by.y="Resource", all.x=TRUE)
  colnames(edges) <- gsub("edge.","",colnames(edges))

  gr <- create_graph(
    nodes_df = nodes,
    edges_df = edges
  )
  return(gr)
}

gr <- aplansGraph("hnh2035")
render_graph(gr)

library(DiagrammeRsvg)

export_graph(gr, "grtest.svg")
save_graph(gr, "grtest")

write.csv(dat$nodes, file="hnh_nodes.csv")
write.csv(dat$edges, file="hnh_edges.csv")

library(RCy3)

plan <- "hnh2035"
require(jsonlite)

dat <- fromJSON(paste0("https://aplans.api.hel.ninja/v1/insight/?plan=", plan))

nodes <- dat$nodes[,-9] # remove the most recent values because the column is a data.frame

nodes$label <- substr(nodes$name, 1, 30)
nodes$group <- paste(nodes$indicator_level, "indicator")
nodes$group[nodes$group=="NA indicator"] <- "action"

nodes <- merge(nodes, formatted[!grepl("edge.", colnames(formatted))],
               by.x="type", by.y="Resource", all.x=TRUE)
colnames(nodes) <- gsub("node.","",colnames(nodes))

nodes <- nodes[c(5, 1:4, 6:ncol(nodes))] # Make id the first column

edges <- dat$edges
edges$directed <- TRUE

edges <- merge(edges, formatted[!grepl("node.", colnames(formatted))],
               by.x="effect_type", by.y="Resource", all.x=TRUE)
colnames(edges) <- gsub("edge.","",colnames(edges))

colnames(edges)[match(c("from","to","effect_type"), colnames(edges))] <- c("source","target","interaction")
edges <- edges[c(3:4, 1, 6, 5, 2, 7:ncol(edges))] # Make source, target and interaction the first columns

createNetworkFromDataFrames(
  nodes = nodes,
  edges = edges
)

############### Insight network of GPC protocols

df <- read.csv("~/watch insight network.csv", stringsAsFactors = FALSE)
df <- df[df$target!="",]

tmp <- df[grepl("gpc",df$id),]
tmp2 <- lapply(strsplit(tmp$gpc, split=","), trimws)
tmp2 <- lapply(tmp2, function(x) if(length(x)==0) "" else x)

out <- tibble()
for(i in 1:nrow(tmp)) {
  out <-bind_rows(out, tibble(tmp[i,colnames(tmp)!="gpc"], gpc=tmp2[[i]]))
}

out$source <- ifelse(out$gpc=="", out$source, out$gpc)

df <- out

nodes <- data.frame(name = unique(c(df$source,df$target)))
nodes <- merge(nodes, df, by.x="name", by.y = "source", all.x=TRUE)
nodes <- nodes[!duplicated(nodes$name),setdiff(colnames(nodes), c("target","interaction","id"))]
nodes <- cbind(id=nodes$name, nodes)

createNetworkFromDataFrames(
  nodes=nodes[!grepl("päästösektori ", nodes$name),],
  edges=df[!grepl("päästösektori ", df$target), 1:3])

############### Insight network of SYKE protocols

df <- read.csv("~/watch insight network.csv", stringsAsFactors = FALSE)
df <- df[df$target!="",]

tmp <- df[grepl("gpc_",df$id) | (grepl("syke", df$id) & df$alas) , ]
tmp2 <- lapply(strsplit(tmp$gpc, split=","), trimws)
tmp2 <- lapply(tmp2, function(x) if(length(x)==0) "" else x)

out <- tibble()
for(i in 1:nrow(tmp)) {
  out <-bind_rows(out, tibble(tmp[i,colnames(tmp)!="gpc"], gpc=tmp2[[i]]))
}

out$target <- ifelse(grepl("syke", out$id), out$gpc, out$target)
out$source <- ifelse(grepl("gpc_m",out$id), out$gpc, out$source)

df <- out

nodes <- data.frame(name = unique(c(df$source,df$target)))
nodes <- merge(nodes, df, by.x="name", by.y = "source", all.x=TRUE)
nodes <- nodes[!duplicated(nodes$name),setdiff(colnames(nodes), c("target","interaction","id"))]
nodes <- cbind(id=nodes$name, nodes)

createNetworkFromDataFrames(
  nodes=nodes[!grepl("päästösektori ", nodes$name),],
  edges=df[!grepl("päästösektori ", df$target), 1:3])

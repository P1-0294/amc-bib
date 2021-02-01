library(Rcpp)
sourceCpp("Rcpp/pathfinder.cpp")

# node_renumber0_map <- function(nodes, id="kid2") {
#   nodes %>% 
#     ungroup() %>%
#     mutate(
#       node_id = row_number()
#     ) %>%
#     select(orig=id, node_id)
# } 
# 
# remap_nodes <- function(idList, mapping) {
#   tibble(orig=idList) %>%
#     inner_join(mapping, by="orig") %>%
#     pull(node_id)
# }

# pathfinder_sparse <- function(nodes, links, r, q, from = "from", to = "to", weight="weight", id="kid2") {
#   mp <- nodes %>% node_renumber0_map(id=id)
#   from <- links %>% pull(from) %>% remap_nodes(mp)
#   to <- links %>% pull(to) %>% remap_nodes(mp)
#   weight <- links %>% pull(weight)
#   n <- nodes %>% nrow
#   # list(from=class(from), to=class(to), weight=class(weight), n=n, r=r, q=q)
#   df <- pathfinder_sparse_cpp(from, to, weight, n, r, q)
#   df %>%
#     left_join(mp, by=c("from"="node_id")) %>%
#     rename(
#       newFrom=orig
#     ) %>%
#     left_join(mp, by=c("to"="node_id")) %>%
#     rename(
#       newTo=orig
#     ) %>%
#     mutate(
#       from=ifelse(newFrom < newTo, newFrom, newTo),
#       to=ifelse(newFrom < newTo, newTo, newFrom)
#     ) %>%
#     select(from, to, weight) 
# }


pathfinder_sparse <- function(nodes, links, r, q, directed=FALSE, from = "from", to = "to", weight="weight", id="kid2") {
  from <- links %>% pull(from)
  to <- links %>% pull(to)
  weight <- links %>% pull(weight)
  nds <- nodes %>% pull(id)
  network <- make_network(nds, from, to, weight, directed)
  pathfinder_sparse_cpp(network, r, q) 
}

# nodes <- data.frame(kid2=c(30,40,50))
# links <- data.frame(
#   from=c(30,40,40),
#   to=c(50, 30, 50),
#   weight=c(0.1, 0.2, 0.4)
# )
# pathfinder_sparse(nodes, links, 1, 3, TRUE)

# nodes <- data.frame(kid2=c(30,40,50))
# links <- data.frame(
#   from=c(),
#   to=c(),
#   weight=c()
# )
# pathfinder_sparse(nodes, links, 1, 3, TRUE)

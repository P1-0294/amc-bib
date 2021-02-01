library(dplyr)
library(rvest)
library(stringr)
library(readr)
library(openxlsx)
library(tidyr)
library(data.table)
library(visNetwork)
library(igraph)

# visSave does not work well with subfolders hence this helper function.
visSaveToVisFolder <- function(graph, fname, FOLDER="visualization") {
  current <- getwd()
  setwd(FOLDER)
  graph %>%
    visSave(fname)
  setwd(current)
}

#### XML helpers
# Text on given path
valueForPath <- function(record, path) {
  record %>% 
    html_node(xpath=path) %>% 
    html_text()
}

# valueForPath mapped on serveral records
valuesForPath <- function(records, path) {
  records %>% sapply(function(record) {valueForPath(record, path)})
}

# Vrne s separatorjem ločene nize v značkah, ki jih dobimo če gremo po vseh možnih poteh iz 
# XML vozlišča (značke) record. Nize združi v en niz, kjer so ločeni s separatorjem.
listValueForPath <- function(record, path, sep=";") {
  record %>% 
    html_nodes(xpath=sprintf("%s/text()", path)) %>% 
    as.character() %>% 
    str_replace_all(" *, *", sep) %>%
    str_replace_all("\\.$", "") %>%
    paste0(collapse = sep)
}

# Izvede zgornjo funkcijo na seznamu (množici) XML vozlišč (znački)
listValuesForPath <- function(records, path, sep = ";") {
  records %>% sapply( . %>% listValueForPath(path, sep))
}


calculate.components <- function(links, nodes, nodeId="aid") {
  G <- links %>% 
    graph.data.frame(vertices = nodes %>% pull(nodeId))
  dec <- components(G)
  
  node.components <- dec$membership %>% 
    (function(vec) {
      tibble(
        id=names(vec) %>% as.integer(),
        component=vec
      )
    })
  
  node.components[nodeId] <- node.components$id
  node.components$id <- NULL
  
  components.count <- node.components %>%
    group_by(component) %>%
    summarise(cnt=n()) 
  
  nodes %>% 
    inner_join(node.components, by=nodeId) %>% 
    inner_join(components.count, by="component") %>% 
    arrange(desc(cnt, component))
}

induced.links <- function(links, nodes, nodeId="id") {
  ids <- nodes %>% pull(nodeId) %>% unique()
  links %>%
    filter(from %in% ids & to %in% ids)
}

addURLs <- function(data) {
  data %>% mutate(
    submissionURL = sprintf("https://amc-journal.eu/index.php/amc/editor/submission/%d", article_id),
    publicationURL = sprintf("https://amc-journal.eu/index.php/amc/article/view/%d", article_id),
    metadataURL = sprintf("https://amc-journal.eu/index.php/amc/editor/viewMetadata/%d", article_id)
  ) %>%  (function(dt) {
    class(dt$submissionURL) <- "hyperlink"
    class(dt$publicationURL) <- "hyperlink"
    class(dt$metadataURL) <- "hyperlink"
    dt
  })
}


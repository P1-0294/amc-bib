source("utils.r")

url <- "https://cran.r-project.org/web/classifications/MSC-2010.html"

items <- url %>% 
  read_html() %>%
  html_nodes(xpath = "body/ul//li")

# ids <- items %>% sapply(function(item) {
#   item %>% html_node(xpath="./@id[1]") %>% html_text()
# })
# texts <- items %>% sapply(function(item) {
#   item %>% html_node(xpath="./text()[1]") %>% html_text()
# })
# items %>% .[[5]] %>% html_node(xpath="./text()[1]") %>% html_text()
# 
# items %>% .[[5]] %>% html_node(xpath="./@id") %>% html_text()

ids <- url %>%
  read_html() %>%
  html_nodes(xpath = "body/ul//li/@id") %>% html_text() %>% 
  str_remove_all("^code:") %>% 
  str_trim()

descriptions <- url %>%
  read_html() %>%
  html_nodes(xpath = "body/ul//li/text()[1]") %>% html_text() %>%
  str_remove_all("\\[.*$")

tibble(
  msc=ids,
  # description0=descriptions,
  description=descriptions %>% str_remove("^.*:") %>% str_trim()
) %>% 
  write_csv("data/msc/msc.csv")

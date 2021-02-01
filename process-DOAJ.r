source("utils.r")
source("normalize.r")

## Load exported XMLs to this folder
DOAJ_EXPORT_FOLDER <- "data/DOAJ_exports"
## Normalized CSVs appear in this folder
DOAJ_FOLDER <- "data/DOAJ"

# Prebere XML datoteko, izloči seznam značk "record" in iz njih pridobiva tekstovne stolpce 
# glede na različne poti iz značke record.
# Vrne tabelo
processXML <- function(fname) {
  records <- fname %>% 
    read_xml() %>% html_nodes(xpath="/records/record")
  tibble(
    source_x = records %>% valuesForPath("publisher"),
    title = records %>% valuesForPath("title"),
    doi = records %>% valuesForPath("doi"),
    license = "CC BY 4.0",
    abstract = records %>% valuesForPath("abstract"),
    publish_time = records %>% valuesForPath("publicationDate"),
    journal = records %>% valuesForPath("journalTitle"),
    url = records %>% valuesForPath("fullTextUrl"),
    volume = records %>% valuesForPath("volume"),
    issue = records %>% valuesForPath("issue"),
    start_page = records %>% valuesForPath("startPage"),
    end_page = records %>% valuesForPath("endPage"),
    authors = records %>% listValuesForPath("authors/author/name"),
    keywords = records %>% listValuesForPath("keywords/keyword")
  )
}

data <- tibble()
for(file in list.files(DOAJ_EXPORT_FOLDER, "*.xml")) {
  fullPath <- file.path(DOAJ_EXPORT_FOLDER, file)
  print(sprintf("Processing '%s'", fullPath))
  data1 <- processXML(fullPath)
  data <- data %>% bind_rows(data1)
}

data <- data %>%
  mutate(
    id = row_number()
  ) 

# # Prebere obe datoteki v dve tabeli.
# data1 <- processXML("data/amc-1-1--8-1.xml")
# data2 <- processXML("data/amc-8-2--20-1.xml")
# 
# # Tabeli združimo in oštevilčimo vrstice (id članka)
# data <- data1 %>% 
#   bind_rows(data2) %>%
#   mutate(
#     id = row_number()
#   ) 

# Shranjevanje združene tabele v CSV
data %>% 
  write_csv("data/amc.csv")

# read_csv("data/amc.csv") %>% View

# Shranjevanje v Excel datoteko. Pri tem stolpec z URL povezavami naredimo aktivne
data %>% 
  (function(data) {
    class(data$url) <- "hyperlink"
    data
  }) %>%
  write.xlsx("data/amc.xlsx")

normalizeData(data, DOAJ_FOLDER)

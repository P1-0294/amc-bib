source("utils.r")

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

##################################
# Preoblikovanje v tidy data
##################################

# Čuden presledek
zw.space <- c(8203,32,8203) %>% intToUtf8()   # Med analizo se nam je pojavil čuden "presledek"

data.sep <- data %>%
  separate_rows("authors", sep=";") %>%    # Ločevanje stolpcev v vrstice
  separate_rows("keywords", sep=";") %>% 
  rename(
    author = authors,
    keyword = keywords
  ) %>% 
  drop_na(start_page) %>%
  filter(author != zw.space) %>%
  mutate(   # Prazni keywordi v NA
    keyword=ifelse(
      keyword == "",
      NA,
      keyword
    )
  ) 

# data.sep %>% View

###############################################################
### Normalizacija (pretvorba v tidy data) v zvezi z avtorji

# Izračun frekvence zastopanosti avtorjev (namen - pregled podatkov)
system.time({
  freq.author <- data.sep %>% 
    group_by(author) %>%
    summarise(cnt = n_distinct(id)) %>% 
    arrange(desc(cnt))
})

# freq.author %>% View

# Izračun frekvence zastopanosti avtorjev - še hitrejši način, primeren za delo 
# z večjimi podatkovji (hitrejši) - uporaba knjižnice data.table
data.sep.DT <- data.sep %>% as.data.table() 

system.time({
  # data.sep.DT[, .(cnt=length(unique(id))), by=author][order(-cnt)]
  freq.author <- data.sep.DT %>%
    .[, .(cnt=length(unique(id))), by=author] %>%
    .[order(-cnt)]
})

# freq.author %>% View

# avtorji na članku
author.paper <- data.sep %>%
  select(id, author) %>%
  distinct()

# enolični avtorji, ki jim dodelimo ključ "aid"
Author <- author.paper %>%
  select(author) %>%
  distinct() %>%
  arrange(author) %>%
  mutate(
    aid=row_number()
  )

# Tabela povezava avtorjev na članke preko "id" in "aid".
AuthorPaper <- author.paper %>%
  inner_join(Author, by="author") %>%
  select(id, aid)

###############################################################
### Normalizacija (pretvorba v tidy data) v zvezi z avtorji

# Pregled frekvenc ključnih besed in možnih anomalij
freq.keyword <- data.sep %>% 
  group_by(keyword) %>%
  summarise(cnt = n_distinct(id)) %>% 
  arrange(desc(cnt))

# freq.keyword %>% View

# Povezava ključnih besed na članke
keyword.paper <- data.sep %>%
  select(id, keyword) %>%
  distinct()

# Enolične ključne besede z dodelitvijo ključa "kid"
Keyword <- keyword.paper %>%
  select(keyword) %>%
  distinct() %>%
  arrange(keyword) %>%
  mutate(
    kid=row_number()
  ) 

# Povezava ključne besede na članek preko "id" in "kid"
KeywordPaper <- keyword.paper %>%
  inner_join(Keyword, by="keyword") %>%
  select(id, kid)

# Tabela člankov, vsak določen z "id", brez stolpcev "authors" in "keywords".
Paper <- data %>% 
  select(-authors, -keywords)


# Shranjevanje tabel v tidy data obliki oz. v "normalizirani" obliki
Author %>% write_csv(file.path(DOAJ_FOLDER,"author.csv"))
Keyword %>% write_csv(file.path(DOAJ_FOLDER,"keyword.csv"))
Paper %>% write_csv(file.path(DOAJ_FOLDER,"paper.csv"))
AuthorPaper %>% write_csv(file.path(DOAJ_FOLDER,"author-paper.csv"))
KeywordPaper %>% write_csv(file.path(DOAJ_FOLDER,"keyword-paper.csv"))

source("keywords-clean.r")
# Tako lahko iz normaliziranih tabel sestavimo tabelo "data.sep", ki je primerna za razne analize
# z uporabo filtriranj in agregacij (group_by, summarise)
# Paper %>%
#   inner_join(AuthorPaper, by="id") %>%
#   inner_join(Author, by="aid") %>% 
#   inner_join(KeywordPaper, by="id") %>%
#   inner_join(Keyword, by="kid")

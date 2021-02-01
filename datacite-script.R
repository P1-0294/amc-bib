source("utils.r")

# If it does not work do the following:
# - Login to OJS.
# - Journal Manger -> Import/Export Data -> DataCite Export/Registration Plugin -> Export articles
# - In browser enable Inspector, watch requests under Network Tab. Use Chrome
# - EXPORT one article and copy request (Copy -> Copy as cURL)
# - Copy OJSSID into string below - it is valid for limited time only
OJSSID <- "nmfjdjrsgjhvqq1vkrajtb6360"
DATA_EXPORT_FOLDER <- "data/datacite_exports"


articleIdTable <- read_delim(
  file.path(DATA_EXPORT_FOLDER, "out.csv"), 
  "\t", 
  col_types = cols(.default=col_character(),
                   issue_id=col_integer())
) %>% 
  filter(issue_id > 0) %>% 
  filter(date_published != "NULL") %>%
  filter(pages != "NULL")

# articleIdTable %>% nrow
# articleIdTable %>% sapply(class)

# articles <- c(69, 50, 88, 174)

reqGZIP <- "
curl 'https://amc-journal.eu/index.php/amc/manager/importexport/plugin/DataciteExportPlugin/process' \\
-H 'Connection: keep-alive' \\
-H 'Cache-Control: max-age=0' \\
-H 'Upgrade-Insecure-Requests: 1' \\
-H 'Origin: https://amc-journal.eu' \\
-H 'Content-Type: application/x-www-form-urlencoded' \\
-H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.96 Safari/537.36' \\
-H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \\
-H 'Sec-Fetch-Site: same-origin' \\
-H 'Sec-Fetch-Mode: navigate' \\
-H 'Sec-Fetch-User: ?1' \\
-H 'Sec-Fetch-Dest: document' \\
-H 'Referer: https://amc-journal.eu/index.php/amc/manager/importexport/plugin/DataciteExportPlugin/articles?articlesPage=2' \\
-H 'Accept-Language: en,it;q=0.9,en-US;q=0.8' \\
-H 'Cookie: OJSSID=%s' \\
--data-raw 'target=article&%s&export=Export' \\
--compressed --output %s
"

reqSingle <- "
curl 'https://amc-journal.eu/index.php/amc/manager/importexport/plugin/DataciteExportPlugin/process?articleId=%s&target=article&export=1' \\
  -H 'Connection: keep-alive' \\
  -H 'Upgrade-Insecure-Requests: 1' \\
  -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.96 Safari/537.36' \\
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9' \\
  -H 'Sec-Fetch-Site: same-origin' \\
  -H 'Sec-Fetch-Mode: navigate' \\
  -H 'Sec-Fetch-User: ?1' \\
  -H 'Sec-Fetch-Dest: document' \\
  -H 'Referer: https://amc-journal.eu/index.php/amc/manager/importexport/plugin/DataciteExportPlugin/articles' \\
  -H 'Accept-Language: en,it;q=0.9,en-US;q=0.8' \\
  -H 'Cookie: OJSSID=%s' \\
  --compressed > %s
"

N <- articleIdTable %>% nrow
# N <- 2
cnt <- 1
batchSize <- 1

WD <- getwd()
setwd(DATA_EXPORT_FOLDER)
repeat {
  if(cnt > N) break;
  # mx <- min(cnt + batchSize - 1, N)
  # articles <- articleIdTable %>% pull(article_id) %>% .[cnt:mx]
  # print(sprintf("%d %d", cnt, mx))
  # articles
  # articles %>% View
  # articlesQueryTemplate <- "articleId%%5B%%5D=%s"
  # tmpOutFile <- sprintf("tmp-%d-%d.tar.gz", cnt, mx)
  # articlesQuery <- articles %>% sprintf(articlesQueryTemplate, .) %>% paste0(collapse="&")
  # command <- sprintf(reqGZIP, OJSSID, articlesQuery, file.path(DATA_EXPORT_FOLDER, tmpOutFile))
  articleId <- articleIdTable[cnt, "article_id"]
  print(articleId)
  command <- sprintf(reqSingle, articleId, OJSSID, sprintf("%03d-article-%s.xml", cnt, articleId))
  
  system(command)
  Sys.sleep(0.1)
#   command2 <- sprintf("
# cd %s
# tar xzf %s
# cd -
# ", DATA_EXPORT_FOLDER, tmpOutFile)
#   system(command2)
#   
  cnt <- cnt + batchSize
}
setwd(WD)

fname <- "data/datacite_exports/001-article-10.xml"

fname %>% 
  read_xml() 

processXMLDataCite <- function(fname) {
  records <- fname %>% 
    read_html() %>% html_nodes(xpath="//resource")
  tibble(
    source_x = records %>% valuesForPath("publisher"),
    title = records %>% valuesForPath("titles/title"),
    doi = records %>% valuesForPath("identifier"),
    license = "CC BY 4.0",
    abstract = records %>% valuesForPath("descriptions/description[@descriptionType]"),
    publish_time = records %>% valuesForPath("publicationDate"),
    # journal = records %>% valuesForPath("journalTitle"),
    # url = records %>% valuesForPath("fullTextUrl"),
    # volume = records %>% valuesForPath("volume"),
    # issue = records %>% valuesForPath("issue"),
    # start_page = records %>% valuesForPath("startPage"),
    # end_page = records %>% valuesForPath("endPage"),
    # authors = records %>% listValuesForPath("authors/author/name"),
    # keywords = records %>% listValuesForPath("keywords/keyword")
  )
}

fname %>% processXMLDataCite


data <- tibble()
for(file in list.files(DATA_EXPORT_FOLDER, "*.xml")) {
  fullPath <- file.path(DATA_EXPORT_FOLDER, file)
  print(sprintf("Processing '%s'", fullPath))
  data1 <- processXML(fullPath)
  data <- data %>% bind_rows(data1)
}

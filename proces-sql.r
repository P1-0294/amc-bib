source("utils.r")
source("normalize.r")

DB_FOLDER <- "data/DB"
NORMALIZED_FOLDER <- "data/DB/normalized"
ANOMALIES_FOLDER <- "data/DB/anomalies"
published_articles <- read_delim(
  file.path(DB_FOLDER, "published_articles.txt"), 
  "\t",
  col_types = cols(
    .default = col_integer(),
    date_published = col_datetime(),
    seq=col_double()
  ),
  na=c("", "NULL")
)

articles <- read_delim(
  file.path(DB_FOLDER, "articles.txt"), 
  "\t",
  col_types = cols(
    .default = col_integer(),
    date_submitted = col_datetime(),
    last_modified = col_datetime(),
    date_status_modified = col_datetime(),
    language=col_character(),
    locale=col_character(),
    pages=col_character()
  ),
  na=c("", "NULL")
)  
  
authors <- read_delim(
  file.path(DB_FOLDER, "authors.txt"), 
  "\t",
  col_types = cols(
    .default = col_character(),
    author_id = col_integer(),
    submission_id = col_integer(),
    primary_contact = col_integer(),
    seq = col_number()
  ),
  na=c("", "NULL")
)

edit_decisions <- read_delim(
    file.path(DB_FOLDER, "edit_decisions.txt"), 
    "\t",
    col_types = cols(
      .default = col_integer(),
      date_decided = col_datetime()
    ),
    na=c("", "NULL")
)

article_settings <-  read_delim(
    file.path(DB_FOLDER, "article_settings.txt"), 
    "\t",
    col_types = cols(
      .default = col_character(),
      article_id = col_integer()
    ),
    na=c("", "NULL")
) %>% 
  select(article_id, setting_name, setting_value) %>%
  pivot_wider(names_from = setting_name, values_from = setting_value)
    
# article_settings %>% arrange(article_id) %>% View

issues <- read_delim(
    file.path(DB_FOLDER, "issues.txt"), 
    "\t",
    col_types = cols(
      # .default=col_character()
      .default = col_integer(),
      date_published = col_datetime(),
      date_notified = col_datetime(),
      open_access_date = col_datetime(),
      last_modified = col_datetime()
    ),
    na=c("", "NULL")
) 


authors_on_published_papers <- published_articles %>% 
  select(article_id) %>% 
  left_join(authors, by=c("article_id"="submission_id")) %>% 
  select(-user_group_id, -suffix) %>%
  arrange(article_id)

joined_authors <- authors_on_published_papers %>% 
  mutate(
    author_name_full = ifelse(
      is.na(middle_name), 
      sprintf("%s, %s", last_name, first_name), 
      sprintf("%s, %s %s", last_name, first_name, middle_name) 
    ) %>% str_replace_all(" +", " ") %>%
      str_trim()
  ) %>% 
  group_by(article_id) %>%
  arrange(seq) %>%
  summarise(
    authors=paste0(author_name_full, collapse=";"), 
    author_count = n()
  )
  
# define('SUBMISSION_EDITOR_DECISION_ACCEPT', 1);
# define('SUBMISSION_EDITOR_DECISION_PENDING_REVISIONS', 2);
# define('SUBMISSION_EDITOR_DECISION_RESUBMIT', 3);
# define('SUBMISSION_EDITOR_DECISION_DECLINE', 4);

acceptions_cnt <- edit_decisions %>%
  filter(decision==1) %>%
  group_by(article_id) %>%
  summarise(cnt=n()) %>%
  arrange(desc(cnt))

## Multiple acceptions
edit_decisions %>%
  filter(decision==1) %>% 
  left_join(acceptions_cnt, by="article_id") %>% 
  arrange(desc(cnt), article_id, desc(date_decided)) %>%
  select(article_id, date_decided, cnt) %>% 
  filter(cnt > 1) %>%
  addURLs() %>%
  write.xlsx(file.path(ANOMALIES_FOLDER, "multiple_accept_editor_decisions.xlsx"))

acceptions <- edit_decisions %>%
  filter(decision==1) %>% 
  left_join(acceptions_cnt, by="article_id") %>% 
  group_by(article_id) %>%
  summarise(
    first_accepted=min(date_decided),
    last_accepted=max(date_decided)
  )

published_papers_1 <- published_articles %>% 
  left_join(article_settings, by="article_id") %>% 
  left_join(
    articles %>% 
      select(article_id, section_id, pages, date_submitted),
    by="article_id"
  ) %>% 
  left_join(
    issues %>% select(issue_id, volume, number, year, date_issue_published=date_published),
    by="issue_id"
  ) %>%
  left_join(joined_authors, by="article_id") %>% 
  left_join(acceptions, by="article_id") %>% 
  addURLs() %>%
  select(article_id, title, authors, author_count, abstract, keywords=subject, subjectClass, doi=`pub-id::doi`, year, volume, number, pages, date_submitted, first_accepted, last_accepted, date_published, date_issue_published, submissionURL, publicationURL, metadataURL) 

published_papers_1 %>% 
  write.xlsx(file.path(ANOMALIES_FOLDER, "AMC.xlsx"))

special.space <- intToUtf8(8203)
authors_on_published_papers %>% 
  group_by(last_name) %>%
  mutate(
    author_name_full = ifelse(
      is.na(middle_name), 
      sprintf("%s, %s", last_name, first_name), 
      sprintf("%s, %s %s", last_name, first_name, middle_name) 
    ) %>% str_replace_all(" +", " ") %>%
      str_trim()
  ) %>%  
  group_by(last_name) %>%
  summarize(
    cnt=n_distinct(author_name_full),
    names=paste0(author_name_full %>% unique, collapse=' | '),
    articles = paste0(article_id %>% unique, collapse="|")
  ) %>%
  filter(cnt > 1) %>%
  filter(!is.na(last_name)) %>%
  filter(!str_detect(last_name, "^\\s*$")) %>%
  filter(last_name != special.space) %>%
  filter(!str_detect(last_name, "^\\d+$")) %>%
  arrange(desc(cnt)) %>% 
  separate_rows(articles) %>%
  mutate(article_id=articles %>% as.integer()) %>% 
  select(-articles) %>% 
  addURLs() %>%
  write.xlsx(file.path(ANOMALIES_FOLDER, "authors.xlsx"))
  
  
mscFix <- published_papers_1 %>% 
  select(article_id, subjectClass) %>% 
  mutate(
    sub1 = str_replace_all(subjectClass, "(\\d\\d[:upper:]\\d\\d) +(\\d\\d[:upper:]\\d\\d)", "\\1, \\2") %>%
      str_replace_all("(\\d\\d[:upper:]\\d\\d) +(\\d\\d[:upper:]\\d\\d)", "\\1, \\2") %>%
      str_replace_all("(\\d\\d[:upper:]\\d\\d) +(\\d\\d[:upper:]\\d\\d)", "\\1, \\2") %>%
      str_replace_all("(\\d\\d[:upper:]\\d\\d) +(\\d\\d[:upper:]\\d\\d)", "\\1, \\2") %>%
      str_replace_all("(\\d\\d[:upper:]\\d\\d) +(\\d\\d[:upper:]\\d\\d)", "\\1, \\2") %>%
      str_replace_all(" +", "") %>% 
      str_replace_all("-", "") %>%
      str_replace_all(";", ",") %>%
      str_replace_all(":", ",") %>%
      str_replace_all("\\\\[tT]", ",") %>%
      str_replace_all("\\(?[Pp]rimary\\)?", ",") %>%
      str_replace_all("\\(?[Ss]econdary\\)?", ",") %>%
      str_replace_all(",,", ",") %>%
      str_replace_all("topologicalgraphtheory,imbeddings", "") %>%
      str_replace_all("Grouptheoryandgeneralizations", "") %>%
      str_replace_all("Cayleygraphs,Pancakegraph,cycleembedding,smallcycles,", "") %>%
      str_replace_all("MSC", "") %>%
      str_replace_all("GraphColoring", "") %>%
      str_replace_all("\\.", "") %>%
      str_replace_all(" +", "") %>% 
      str_replace_all(intToUtf8(8206), "") %>%
      str_replace(",$", "") %>%
      str_replace("^,", "") %>%
      str_replace(",,+", ",") %>%
      toupper()
  ) %>% select(article_id, subjectClass, sub1) %>% 
  separate_rows(sub1, sep=",") %>%
  mutate(
    sub1 = str_trim(sub1)
  ) %>%
  mutate(
    ln=str_length(sub1)
  ) %>%
  # arrange(ln) %>%
  filter(str_detect(sub1, "\\d?\\d[:upper:][\\dX][\\dX]")) %>%
  mutate(
    sub1 = str_replace_all(sub1, "X", "x")
  ) %>%
  group_by(article_id) %>%
  summarise(
    newSubjectClass=paste0(sub1, collapse=",")
  ) 
# %>%
  # filter(article_id==24) %>%
  # .[1, 3] %>% pull(1) %>% utf8ToInt()
# View

fixed_papers <- published_papers_1 %>%
  left_join(mscFix, by="article_id") %>%
  select(article_id, title, authors, author_count, abstract, keywords, subjectClass, newSubjectClass, doi, year, volume, number, pages, date_submitted, first_accepted, last_accepted, date_published, date_issue_published, submissionURL, publicationURL, metadataURL) 

fixed_papers %>% 
  write.xlsx(file.path(ANOMALIES_FOLDER, "AMC-MSCfix.xlsx"))

data <- fixed_papers %>% 
  mutate(
    source_x="AMC",
    journal="AMC",
    license = "CC BY 4.0",
    publish_time = date_issue_published,
    url=submissionURL,
    newSubjectClass=str_replace_all(newSubjectClass, ",", ";")
  ) %>% separate(pages, c("start_page", "end_page"), sep="-") %>% 
  select(
    id=article_id, source_x, title, doi, license, abstract, publish_time, journal, url, volume, issue=number, start_page, end_page, authors, keywords, subject_class=newSubjectClass
  ) 

normalizeData(data, NORMALIZED_FOLDER)

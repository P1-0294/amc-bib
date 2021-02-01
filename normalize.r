source("keywords-clean.r")

##################################
# Preoblikovanje v tidy data
##################################

# id should be set properly
normalizeData <- function(data, OUT_FOLDER) {
  # Čuden presledek
  zw.space <- c(8203,32,8203) %>% intToUtf8()   # Med analizo se nam je pojavil čuden "presledek"
  
  data.sep <- data %>%
    separate_rows("authors", sep=";") %>%    # Ločevanje stolpcev v vrstice
    separate_rows("keywords", sep=";") %>% 
    (function(data){
      if("subject_class" %in% names(data)) {
        data %>% separate_rows("subject_class", sep=";")    
      } else {
        data
      }
    }) %>%
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
    ) %>%
    (function(data){
      if("subject_class" %in% names(data)) {
        data %>% 
          mutate(   # Prazni keywordi v NA
            subject_class=ifelse(
              subject_class == "",
              NA,
              subject_class
            )
          ) 
      } else {
        data
      }
    })
  
  
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
  
  
  ###############################################################
  ### Normalizacija (pretvorba v tidy data) v zvezi z MSX
  
  if("subject_class" %in% names(data)) {
    # Pregled frekvenc ključnih besed in možnih anomalij
    freq.subjectClass <- data.sep %>% 
      group_by(subject_class) %>%
      summarise(cnt = n_distinct(id)) %>% 
      arrange(desc(cnt))
    
    # freq.keyword %>% View
    
    # Povezava ključnih besed na članke
    subjectClass.paper <- data.sep %>%
      select(id, subject_class) %>%
      distinct()
    
    # Enolične ključne besede z dodelitvijo ključa "kid"
    SubjectClass <- subjectClass.paper %>%
      select(subject_class) %>%
      distinct() %>%
      arrange(subject_class) %>%
      mutate(
        scid=row_number()
      ) 
    
    # Povezava ključne besede na članek preko "id" in "kid"
    SubjectClassPaper <- subjectClass.paper %>%
      inner_join(SubjectClass, by="subject_class") %>%
      select(id, scid)
  }
  
  # Tabela člankov, vsak določen z "id", brez stolpcev "authors" in "keywords".
  Paper <- data %>% 
    select(-authors, -keywords)
  
  
  # Shranjevanje tabel v tidy data obliki oz. v "normalizirani" obliki
  Author %>% write_csv(file.path(OUT_FOLDER,"author.csv"))
  Keyword %>% write_csv(file.path(OUT_FOLDER,"keyword.csv"))
  Paper %>% write_csv(file.path(OUT_FOLDER,"paper.csv"))
  AuthorPaper %>% write_csv(file.path(OUT_FOLDER,"author-paper.csv"))
  KeywordPaper %>% write_csv(file.path(OUT_FOLDER,"keyword-paper.csv"))
  if("subject_class" %in% names(data)) {
    SubjectClass %>% write_csv(file.path(OUT_FOLDER,"subject-class.csv"))
    SubjectClassPaper %>% write_csv(file.path(OUT_FOLDER,"subject-class-paper.csv"))
  }
  cleanKeywords(Keyword, Paper, KeywordPaper, OUT_FOLDER)
  # Tako lahko iz normaliziranih tabel sestavimo tabelo "data.sep", ki je primerna za razne analize
  # z uporabo filtriranj in agregacij (group_by, summarise)
  # Paper %>%
  #   inner_join(AuthorPaper, by="id") %>%
  #   inner_join(Author, by="aid") %>% 
  #   inner_join(KeywordPaper, by="id") %>%
  #   inner_join(Keyword, by="kid")
}
source("utils.r")

DOAJ_FOLDER <- "data/DOAJ"

Keyword <- read_csv(file.path(DOAJ_FOLDER, "keyword.csv"))
Paper <- read_csv(file.path(DOAJ_FOLDER, "paper.csv"))
KeywordPaper <- read_csv(file.path(DOAJ_FOLDER, "keyword-paper.csv"))

#################################################
# Čiščenje ključnih besed
#################################################

# Frekvence ključnih besed
Paper %>%
  inner_join(KeywordPaper, by="id") %>%
  inner_join(Keyword, by="kid") %>% 
  group_by(keyword) %>%
  summarise(cnt =n()) %>% 
  arrange(desc(cnt)) %>% 
  write.xlsx("data/freq-keyword.xlsx")

replacements <- c(
  "(^.*configuration)(s)$"="\\1",
  "(^.*graph)(s)$"="\\1",
  "(^.*factorisation)(s)$"="\\1",
  "(^.*)(factorization)(s)$"="\\1factorisation",
  "(^.*group)(s)$"="\\1",
  "(^.*topolog)(ies)$"="\\1y",
  "(^.*polytop)(es)$"="\\1e",
  "(^.*space)(s)$"="\\1",
  "(^.*curve)(s)$"="\\1",
  "(^.*aggregate)(s)$"="\\1",
  "(^.*famil)(ies)$"="\\1y",
  "(^.*patch)(es)$"="\\1",
  "(^.*line)(s)$"="\\1",
  "(^.*scheme)(s)$"="\\1",
  "  "=" ",
  "(^.*quotient)(s)$"="\\1",
  "(^.*plane)(s)$"="\\1",
  "(^.*lattice)(s)$"="\\1",
  "(^.*function)(s)$"="\\1",
  "(^.*automorphism)(s)$"="\\1",
  "(^.*grap)(s)$"="\\1h",
  "(^.*network)(s)$"="\\1",
  "(^.*measure)(s)$"="\\1",
  "(^.*cycle)(s)$"="\\1",
  "(^.*design)(s)$"="\\1",
  "(^.*surface)(s)$"="\\1",
  "(^.*tree)(s)$"="\\1",
  "(^.*system)(s)$"="\\1",
  "(^.*knot)(s)$"="\\1",
  "(^.*coloring)(s)$"="\\1",
  "(^.*colouring)(s)$"="\\1",
  "(^.*dessin)(s)$"="\\1",
  "(^.*permutation)(s)$"="\\1",
  "(^.*number)(s)$"="\\1",
  "(^.*problem)(s)$"="\\1",
  "(^.*class)(es)$"="\\1",
  "(^.*bound)(s)$"="\\1",
  "(^.*product)(s)$"="\\1",
  "(^.*cover)(s)$"="\\1",
  "(^.*voltage)(s)$"="\\1",
  "(^.*vertice)(s)$"="\\1",
  "(^.*code)(s)$"="\\1",
  "(^.*ind)(ices)$"="\\1ex",
  "(^.*cage)(s)$"="\\1",
  "(^.*triangulation)(s)$"="\\1",
  "(^.*transversal)(s)$"="\\1",
  "(^.*strateg)(ies)$"="\\1y",
  "(^.*isomorphism)(s)$"="\\1",
  "(^.*forest)(s)$"="\\1",
  "(^.*leave)(s)$"="\\1",
  "(^.*variet)(ies)$"="\\1y",
  "(^.*triple)(s)$"="\\1",
  "(^.*matroid)(s)$"="\\1",
  "(^.*base)(s)$"="\\1",
  "(^.*cycle)(s)$"="\\1",
  "(^.*bas)(is)$"="\\1e",
  "(^.*map)(s)$"="\\1",
  "(^.*field)(s)$"="\\1",
  "(^.*algorithm)(s)$"="\\1",
  "(^.*identit)(ies)$"="\\1y",
  "(^.*propert)(ies)$"="\\1y",
  "(^.*commutator)(s)$"="\\1",
  "(^.*geometr)(ies)$"="\\1y",
  "(^.*homomorphism)(s)$"="\\1",
  "(^.*determinant)(s)$"="\\1",
  "(^.*symmetr)(ies)$"="\\1y",
  "(^.*polynomial)(s)$"="\\1",
  "(^.*determinant)(s)$"="\\1",
  "(^.*equalit)(ies)$"="\\1y",
  "(^.*snark)(s)$"="\\1",
  "(^.*angle)(s)$"="\\1",
  "(^.*surger)(ies)$"="\\1y",
  "(^.*tiling)(s)$"="\\1",
  "(^.*tiling)(s)$"="\\1",
  "(^.*assignment)(s)$"="\\1",
  "(^.*convexit)(ies)$"="\\1y",
  "(^.*eigenvalue)(s)$"="\\1",
  "(^.*eigenvector)(s)$"="\\1",
  "(^.*integer)(s)$"="\\1",
  "(^.*tour)(s)$"="\\1",
  "(^.*edge)(s)$"="\\1",
  "(^.*polygon)(s)$"="\\1",
  "(^.*decomposition)(s)$"="\\1",
  "(^.*embedding)(s)$"="\\1",
  "(^.*path)(s)$"="\\1",
  "(^.*algebra)(s)$"="\\1",
  "(^.*trilateral)(s)$"="\\1",
  "(^.*tensegrit)(ies)$"="\\1y",
  "(^.*spread)(s)$"="\\1",
  "(^.*game)(s)$"="\\1",
  "(^.*match)(s)$"="\\1",
  "(^.*solid)(s)$"="\\1",
  "(^.*degree)(s)$"="\\1",
  "(^.*crossing)(s)$"="\\1",
  "(^.*enfant)(s)$"="\\1",
  "(^.*cut)(s)$"="\\1",
  "(^.*set)(s)$"="\\1",
  "(^.*covering)(s)$"="\\1",
  "(^.*arrangement)(s)$"="\\1",
  "(^.*value)(s)$"="\\1",
  "(^.*cube)(s)$"="\\1",
  "(^.*word)(s)$"="\\1",
  "(^.*fullerene)(s)$"="\\1",
  "(^.*generator)(s)$"="\\1",
  "(^.*product)(s)$"="\\1",
  "(^.*array)(s)$"="\\1",
  "(^.*hierarch)(ies)$"="\\1y",
  "(^.*packing)(s)$"="\\1",
  "(^.*matri)(ces)$"="\\1x",
  "(^.*theorem)(s)$"="\\1",
  "(^.*involute)(s)$"="\\1",
  "(^.*representation)(s)$"="\\1",
  "(^.*ring)(s)$"="\\1",
  "(^.*relation)(s)$"="\\1",
  "(^.*unital)(s)$"="\\1",
  "(^.*hexagon)(s)$"="\\1",
  "(^.*otope)(s)$"="\\1",
  "(^.*parameterization)(s)$"="\\1",
  "(^.*order)(s)$"="\\1",
  "(^.*partition)(s)$"="\\1",
  "(^.*permanent)(s)$"="\\1",
  "(^.*recurrence)(s)$"="\\1",
  "(^.*coefficient)(s)$"="\\1",
  "(^.*point)(s)$"="\\1",
  "(^.*contribution)(s)$"="\\1",
  "(^.*equidistant)(s)$"="\\1",
  "(^.*produc)(s)$"="\\1t",
  "(^.*distance)(s)$"="\\1",
  "(^.*operation)(s)$"="\\1",
  "(^.*extension)(s)$"="\\1",
  "(^.*disc)(s)$"="\\1",
  "(^.*hypertope)(s)$"="\\1",
  "(^.*nomial)(s)$"="\\1",
  "(^.*multiplicit)(ies)$"="\\1y",
  "(^.*operator)(s)$"="\\1",
  "(^.*model)(s)$"="\\1",
  "(^.*conguration)(s)$"="\\1", 
  "(^.*cone)(s)$"="\\1",
  "(^.*evolute)(s)$"="\\1",
  "(^.*latin square)(s)$"="\\1",
  "(^.*level)(s)$"="\\1",
  "(^.*discriminant)(s)$"="\\1",
  "(^.*chain)(s)$"="\\1",
  "(^.*matching)(s)$"="\\1",
  "(^.*monoid)(s)$"="\\1",
  "- "="-"
)

keyword.clean <- Keyword %>% 
  mutate(
    lower=tolower(keyword)
  ) %>%
  arrange(lower) %>%
  (function(data) {
    keyword <- data$lower
    from <- replacements %>% names()
    to <- replacements
    for(i in 1:length(replacements)) {
      keyword <- str_replace(keyword, from[i], to[i])  
    }
    data$lower2 <- keyword
    data
  }) %>% (function(data){
    enum2 <- data %>% 
      arrange(lower2) %>%
      pull(lower2) %>% 
      unique();
    # print(enum2)
    data %>% 
      left_join(
        tibble(lower2=enum2) %>% mutate(kid2=row_number()), 
        by="lower2"
      )
  }) 

Keyword2 <- Keyword %>% 
  left_join(keyword.clean, by=c("keyword"="keyword", "kid"="kid")) %>%
  select(kid, kid2, keyword, normalized=lower2) %>%
  mutate(
    normalized=ifelse(is.na(normalized), "NO KEYWORDS", normalized)
  )

Keyword2 %>% write_csv(file.path(DOAJ_FOLDER, "keyword-2.csv"))
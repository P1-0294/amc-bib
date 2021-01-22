source("utils.r")

DOAJ_FOLDER <- "data/DOAJ"
VISUALIZATIONS_FOLDER <- "visualizations/keywords"

# Keyword <- read_csv("data/keyword.csv")
Keyword2 <- read_csv(
  file.path(DOAJ_FOLDER, "keyword-2.csv"), 
  col_types = cols(
    kid = col_integer(),
    kid2 = col_integer()
  )
)
Paper <- read_csv(
  file.path(DOAJ_FOLDER, "paper.csv"),
  col_types = cols(
    volume = col_integer(),
    issue = col_integer(),
    start_page = col_integer(),
    end_page = col_integer(),
    id = col_integer()
  )
)

KeywordPaper <- read_csv(
  file.path(DOAJ_FOLDER, "keyword-paper.csv"),
  col_types = cols(
    id = col_integer(),
    kid = col_integer()
  )
)

############################
## Analiza Keywordov
############################

keyword.paper.weight <- KeywordPaper %>%
  group_by(id) %>%
  summarise(weight=1/n()) %>%
  # summarise(weight=1) %>% 
  inner_join(KeywordPaper, by="id")  %>% 
  inner_join(Keyword2, by="kid")

# Pregled
# keyword.paper.weight %>%
#   group_by(kid2, normalized) %>%
#   summarise(total.contribution = sum(weight)) %>%
#   arrange(desc(total.contribution)) %>% View


# Izračun povezav med keywordi. Če keyworda sodelujeta na kakem članku, sta povezana.
keyword.keyword.links <- keyword.paper.weight %>% 
  inner_join(keyword.paper.weight, by="id") %>% # Ustvarjanje povezav preko skupnih člankov 
  mutate(                                      # t.i. "množenje omrežij"
    from=kid2.x,
    to=kid2.y,
    weight=weight.x
  ) %>%
  select(from, to, weight) %>%     # Izvleček povezav za sodelovanje na posameznem članku
  group_by(from, to) %>%      # združevanje vseh povezav pri sodelovanju
  summarise(
    n = sum(weight),
    ndeg = n()
  ) %>%   
  filter(from < to)   # ohranimo samo povezave v eno smer (tudi brez zank). 
# zanke bi določale celoten prispevek avtorjev kot zgoraj

# Seznam avtorjev z njihovimi prispevki
keyword.all <- keyword.paper.weight %>%
  group_by(kid2, normalized) %>%
  summarise(
    total.contribution = sum(weight),
    total.degree=n()
  ) %>%
  arrange(desc(total.contribution))

keywords.connected.kid2 <- c(
  keyword.keyword.links %>% pull(from) %>% unique,
  keyword.keyword.links %>% pull(to) %>% unique
) %>% unique

###################################################################
## Keyword co-existence (fractional) 
###################################################################

nodes <- keyword.all
links <- keyword.keyword.links 
nodes <- calculate.components(links, nodes, nodeId="kid2") 
visNetwork(
  nodes %>% 
    # filter(kid2 %in% keywords.connected.kid2) %>%
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution,
      group=component
    ),
  links %>% mutate(
    width=n*10,
    # label=sprintf("%.2f/%d", n, ndeg),
    title=sprintf("%.2f/%d", n, ndeg)
  ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
    ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-all.html", VISUALIZATIONS_FOLDER)


###################################################################
## Keyword co-existence (fractional, more than 1 paper)
###################################################################

keyword.important <- keyword.all %>% 
  arrange(desc(total.degree)) %>%
  filter(total.degree > 1)


nodes <- keyword.important
links <- keyword.keyword.links %>% 
  induced.links(keyword.important, nodeId="kid2")
nodes <- calculate.components(links, nodes, nodeId="kid2") 
# keyword.all %>% View
visNetwork(
  nodes %>% 
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution,
      group=component
    ),
  links %>%
  mutate(
    width=n*10,
    # label=sprintf("%.2f/%d", n, ndeg),
    title=sprintf("%.2f/%d", n, ndeg)
  ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional, more than 1 paper)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-important.html", VISUALIZATIONS_FOLDER)


###################################################################
## Keyword co-existence (fractional, more than 2 papers, without top 3)
###################################################################

cutoff.degree <- 30
keyword.important.but.top3 <- keyword.important %>%
  filter(total.degree < cutoff.degree)

visNetwork(
  keyword.important.but.top3 %>% 
    mutate(
      id=kid2,
      label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
      value=total.contribution
    ),
  keyword.keyword.links %>% 
    induced.links(keyword.important.but.top3, nodeId="kid2") %>%
    mutate(
      width=n*10,
      # label=sprintf("%.2f/%d", n, ndeg),
      title=sprintf("%.2f/%d", n, ndeg)
    ),
  width="100%",
  height="800px",
  main="Keyword co-existence (fractional, more than 2 papers, without top 3)"
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("keywords-important-but-top-3.html", VISUALIZATIONS_FOLDER)


###################################################################
## Keyword co-existence (fractional, more than 1 paper, pathfinder)
###################################################################

nodes <- keyword.important
links0 <- keyword.keyword.links %>% 
  induced.links(keyword.important, nodeId="kid2") %>%
  mutate(
    weight=1/n
  )

# links %>% View
# node_renumber0_map(nodes, id="kid2") %>% View
source("pathfinder.r")

visPathfinder <- function(nodes, links, r=1, q="n - 1", id="kid2", description="fractional, more than 1 paper", shortDesc="frac-important") {
  if(q == "n - 1") {
    q = nodes %>% nrow() - 1
  }
  links <- pathfinder_sparse(nodes, links0, r, q, id=id) %>% 
    # mutate(       # debug
    #   pathfinder=1
    # ) %>%
    # right_join(links0 %>%    
    left_join(links0 %>%
                select(from, to, ndeg, n), by=c("from"="from", "to"="to")) %>%
    select(-weight)
  # select(-weight) %>% # debug
  # mutate(
  #   color=ifelse(is.na(pathfinder), "#ff0000aa", "blue"),
  #   weight=1/n
  # )
  
  # links %>% View  
  visNetwork(
    nodes %>%
      mutate(
        id=kid2,
        label=sprintf("%s (%.2f/%d)", normalized, total.contribution, total.degree),
        title=sprintf("%d:%s (%.2f/%d)", id, normalized, total.contribution, total.degree),
        value=total.contribution,
        # group=component
      ),
    links %>%
      mutate(
        width=n*10,
        # width=weight,
        # label=sprintf("%.2f(w:%.2f)/%d", n, weight, ndeg),
        title=sprintf("w: %.2f | 1/w: %.2f | deg: %d", n, 1/n, ndeg),
        # title=sprintf("%.2f(w:%.2f)/%d", n, weight, ndeg),
      ),
    width="100%",
    height="800px",
    main=sprintf("Keyword co-existence (N: %d, E: %d, %s, pathfinder, r = %s, q = %s)", nodes %>% nrow, links %>% nrow, description, r, q)
  ) %>%
    visIgraphLayout(
      physics=TRUE,
      type="full"
    ) %>%
    visPhysics(stabilization = FALSE) %>%
    visEdges(smooth = TRUE, physics = TRUE) %>%
    # visEdges(smooth = TRUE) %>%
    visNodes(physics = TRUE) %>%
    visSaveToVisFolder(sprintf("keywords-%s-pathfinder-rq-%s-%d.html", shortDesc, r, q), VISUALIZATIONS_FOLDER)
}

visPathfinder(nodes, links0)
visPathfinder(nodes, links0, r=Inf)
visPathfinder(nodes, links0, r=Inf, q=3)

# testlinks <- tibble(
#   from=      c(1,2,3),
#   to=        c(2,3,1),
#   weight =   c(1/3, 1/5, 1/5)
# )
# 
# testnodes = tibble(
#   id=c(1,2,3)
# )
# 
# source("pathfinder.r")
# pathfinder_sparse(testnodes, testlinks, 1, 3, id="id") %>% View

source("utils.r")

DOAJ_FOLDER <- "data/DOAJ"
VISUALIZATIONS_FOLDER <- "visualizations/authors"
Author <- read_csv(file.path(DOAJ_FOLDER, "author.csv"))
Paper <- read_csv(file.path(DOAJ_FOLDER, "paper.csv"))
AuthorPaper <- read_csv(file.path(DOAJ_FOLDER, "author-paper.csv"))

#######################
### Analiza prispevkov avtorjev v reviji in medsebojnega sodelovanja preko avtorstev
### Uporaba metod analize omrežij

# Tabela uteži prispevkov enega avtorja za posamezen članek
author.paper.weight <- AuthorPaper %>%
  group_by(id) %>%
  summarise(weight=1/n()) %>% 
  inner_join(AuthorPaper, by="id") 

# Pregled
# author.paper.weight %>%
#   inner_join(Author, by="aid") %>% 
#   group_by(aid, author) %>%
#   summarise(total.contribution = sum(weight)) %>%
#   arrange(desc(total.contribution)) %>% View


# Izračun povezav med avtorji. Če avtorja sodelujeta na kakem članku, sta povezana.
# Primer: če sta na nekem članku to 2 izmed 5 avtorjev, vsak od avtorjev pridobi utež 1/5.
# Če avtorja sodelujeta (sta soavtorja) na več člankih, se prispevki seštejejo v celotno utež na
# povezavi med avtorjema. 
author.author.links <- author.paper.weight %>% 
  inner_join(author.paper.weight, by="id") %>% # Ustvarjanje povezav preko skupnih člankov 
  mutate(                                      # t.i. "množenje omrežij"
    from=aid.x,
    to=aid.y,
    weight=weight.x
  ) %>%
  select(from, to, weight) %>%     # Izvleček povezav za sodelovanje na posameznem članku
  group_by(from, to) %>%      # združevanje vseh povezav pri sodelovanju
  summarise(n = sum(weight)) %>%   
  filter(from < to)   # ohranimo samo povezave v eno smer (tudi brez zank). 
     # zanke bi določale celoten prispevek avtorjev kot zgoraj

# Seznam avtorjev z njihovimi prispevki
author.all <- author.paper.weight %>%
  inner_join(Author, by="aid") %>% 
  group_by(aid, author) %>%
  summarise(total.contribution = sum(weight)) %>%
  arrange(desc(total.contribution)) 

# Dobimo omrežje
# VOZLIŠĆA: author.all, identifikator avtorja je "aid"
# POVEZAVE: author.author.links, imajo tudi utež "n", kot opisano

# Če je veliko točk in povezav so vizualizacije lahko nepregledne in/ali počasne
# Poglejmo si podomrežje na 100 najproduktivnejših avtorjih (glede na revijo)
author.top100 <- author.all %>% 
  head(100)

# "aid"-ji teh avtorjev
author.top100.ids <- author.top100 %>% pull(aid)

# Filtriranje povezav, ki so v podomrežju
author.author.links.top100 <- author.author.links %>%
  filter(from %in% author.top100.ids & to %in% author.top100.ids)

# Primer uporabe vizualizacije s knjižnico visNetwork
# nodes <- data.frame(id = 1:3)
# edges <- data.frame(from = c(1,2), to = c(1,3))
# visNetwork(nodes, edges, width = "100%")

# author.author.links.top100 %>% View

# Vizualizacija omrežja sodelovanj na 100 najproduktivnejših avtorjih
visNetwork(
  author.top100 %>% mutate(
    id=aid,   # identifikator mora biti v stolpcu "id"
    label=sprintf("%s (%.2f)", author, total.contribution),  # labele vozlišč
    title=sprintf("%s (%.2f)", author, total.contribution),  # labele "na klik"
    value=total.contribution  # Velikost vozlišča
  ),
  author.author.links.top100 %>% mutate(
    width=n*10,   # Debelina povezave - pomnožimo jo glede na "občutek", da lepše izgleda
    # label=sprintf("%.2f", n), # Napis na povezavi (ga ne želimo)
    title=sprintf("%.2f", n)  # Napis na povezavi (ob kliku/oz. potovanju miške nad njo)
  ),
  width="100%",   # Velikost na spletni strani
  height="800px",
  main="Author collaboration (top 100 authors by contribution)"
) %>% 
  visIgraphLayout(physics=TRUE) %>%
  visPhysics(stabilization = FALSE, solver="barnesHut", barnesHut=list(
    gravitationalConstant=-2000,
    centralGravity=0.3,
    springLength=150,
    springConstant=0.03,
    damping=0.9,
    avoidOverlap=1
  )) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("authors-100.html", VISUALIZATIONS_FOLDER) # Shranjevanje v HTML datoteko (stran), ki jo lahko odpremo v brskalniku

###################################################
## Vizualizacija sodelovanj vseh avtorjev
###################################################

visNetwork(
  author.all %>% mutate(
    id=aid,
    label=sprintf("%s (%.2f)", author, total.contribution),
    title=sprintf("%s (%.2f)", author, total.contribution),
    value=total.contribution
  ),
  author.author.links %>% mutate(
    width=n*10,
    # label=sprintf("%.2f", n),
    title=sprintf("%.2f", n)
  ),
  width="100%",
  height="800px",
  main="Author collaboration"
) %>% 
  visIgraphLayout(physics=TRUE) %>%
  visPhysics(stabilization = FALSE, solver="barnesHut", barnesHut=list(
    gravitationalConstant=-2000,
    centralGravity=0.3,
    springLength=150,
    springConstant=0.03,
    damping=0.9,
    avoidOverlap=1
  )) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("authors-all.html", VISUALIZATIONS_FOLDER)

###################################################
## Glavna komponenta sodelovanj
###################################################

author.all.components <- calculate.components(author.author.links, author.all) 

min.component.size <- 6
author.all.main.components <- author.all.components %>% 
  filter(cnt > min.component.size)

# author.component.cnt <- author.all.components %>%
#   group_by(component) %>%
#   summarise(n=n()) %>%
#   arrange(desc(n)) 

# author.all.main.components %>% View
# author.author.links %>% 
#   induced.links(author.all.main.components, nodeId="aid") %>% nrow

visNetwork(
  author.all.main.components %>% mutate(
    id=aid,
    label=sprintf("%s (%.2f)", author, total.contribution),
    title=sprintf("%s (%.2f)", author, total.contribution),
    value=total.contribution,
    group=component
  ),
  author.author.links %>% 
    induced.links(author.all.main.components, nodeId="aid") %>%
    mutate(
      width=n*10,
      # label=sprintf("%.2f", n),
      title=sprintf("%.2f", n)
    ),
  width="100%",
  height="800px",
  main=sprintf("Author collaboration (main components, cutoff=%d)", min.component.size)
) %>% 
  visIgraphLayout(physics=TRUE) %>%
  visPhysics(stabilization = FALSE, solver="barnesHut", barnesHut=list(
    gravitationalConstant=-2000,
    centralGravity=0.3,
    springLength=200,
    springConstant=0.03,
    damping=0.9,
    avoidOverlap=1
  )) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("authors-main-components.html", VISUALIZATIONS_FOLDER)




source("utils.r")

DATA_FOLDER <- "data/DB/normalized"
VISUALIZATIONS_FOLDER <- "visualizations/subject-class"


MSC <- read_csv(
  "data/msc/msc.csv",
  col_types = cols(
    .default = col_character()
  )
)

SubjectClass <- read_csv(
  file.path(DATA_FOLDER, "subject-class.csv"),
  col_types = cols(
    scid = col_integer()
  )
) %>% left_join(MSC, by=c("subject_class"="msc")) %>%
  rename(msc_description = description)

Paper <- read_csv(
  file.path(DATA_FOLDER, "paper.csv"),
  col_types = cols(
    volume = col_integer(),
    issue = col_integer(),
    start_page = col_integer(),
    end_page = col_integer(),
    id = col_integer()
  )
)

SubjectClassPaper <- read_csv(
  file.path(DATA_FOLDER, "subject-class-paper.csv"),
  col_types = cols(
    id = col_integer(),
    scid = col_integer()
  )
)

sc.paper.weight <- SubjectClassPaper %>%
  group_by(id) %>%
  summarise(weight=1/n()) %>% 
  inner_join(SubjectClassPaper, by="id") %>%
  inner_join(SubjectClass, by="scid") 

sc.sc.links <- sc.paper.weight %>% 
  inner_join(sc.paper.weight, by="id") %>% # Ustvarjanje povezav preko skupnih člankov 
  mutate(                                      # t.i. "množenje omrežij"
    from=scid.x,
    to=scid.y,
    weight=weight.x
  ) %>% 
  select(from, to, weight) %>%     
  group_by(from, to) %>%      
  summarise(
    n = sum(weight),
    ndeg = n()
  ) %>%   
  filter(from < to) 

sc.all <- sc.paper.weight %>% 
  group_by(scid, subject_class, msc_description) %>%
  summarise(
    total.contribution = sum(weight),
    total.degree=n()
  ) %>%
  arrange(desc(total.contribution))

###################################################################
## Subject class co-existence (fractional) 
###################################################################

nodes <- sc.all
links <- sc.sc.links 
nodes <- calculate.components(links, nodes, nodeId="scid") 

visNetwork(
  nodes %>% 
    mutate(
      id=scid,
      label=sprintf("%s (%.2f/%d)", subject_class, total.contribution, total.degree),
      title=sprintf("%s (%.2f/%d)", subject_class, total.contribution, total.degree),
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
  main=sprintf("Subject class co-existence (N: %d, E: %d, fractional)", nodes %>% nrow, links %>% nrow)
) %>% 
  visIgraphLayout(
    physics=TRUE,
    type="full"
  ) %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = TRUE, physics = TRUE) %>%
  # visEdges(smooth = TRUE) %>%
  visNodes(physics = TRUE) %>%
  visSaveToVisFolder("subject-class-all.html", VISUALIZATIONS_FOLDER)


###################################################################
## Subject class co-existence (fractional, pathfinder) 
###################################################################
source("pathfinder.r")
visPathfinder <- function(nodes, links, r=1, q="n - 1", id="scid", description="fractional", shortDesc="frac") {
  if(q == "n - 1") {
    q = nodes %>% nrow() - 1
  }
  links <- pathfinder_sparse(nodes, links, r, q, id=id) %>% 
    # mutate(       # debug
    #   pathfinder=1
    # ) %>%
    # right_join(links %>%    
    left_join(links %>%
                select(from, to, ndeg, n), by=c("from"="from", "to"="to")) %>%
    select(-weight)
  # select(-weight) %>% # debug
  # mutate(
  #   color=ifelse(is.na(pathfinder), "#ff0000aa", "blue"),
  #   weight=1/n
  # )
  
  visNetwork(
    nodes %>%
      mutate(
        id=scid,
        label=sprintf("%s (%.2f/%d)", subject_class, total.contribution, total.degree),
        title=sprintf("%d:%s: %s (%.2f/%d)", id, subject_class, msc_description, total.contribution, total.degree),
        value=total.contribution,
        # group=component
      ),
    links %>%
      mutate(
        width=n*3,
        # width=weight,
        # label=sprintf("%.2f(w:%.2f)/%d", n, weight, ndeg),
        title=sprintf("w: %.2f | 1/w: %.2f | deg: %d", n, 1/n, ndeg),
        # title=sprintf("%.2f(w:%.2f)/%d", n, weight, ndeg),
      ),
    width="100%",
    height="800px",
    main=sprintf("Subject class co-existence (N: %d, E: %d, %s, pathfinder, r = %s, q = %s)", nodes %>% nrow, links %>% nrow, description, r, q)
  ) %>%
    visIgraphLayout(
      physics=TRUE,
      type="full"
    ) %>%
    visPhysics(stabilization = FALSE) %>%
    visEdges(smooth = TRUE, physics = TRUE) %>%
    # visEdges(smooth = TRUE) %>%
    visNodes(physics = TRUE) %>%
    visSaveToVisFolder(sprintf("subject-class-%s-pathfinder-rq-%s-%d.html", shortDesc, r, q), VISUALIZATIONS_FOLDER)
}

nodes <- sc.all
links <- sc.sc.links %>% 
  mutate(
    weight=1/n
  )

visPathfinder(nodes, links)
# visPathfinder(nodes, links, r=Inf)

threshold <- 0.5
desc <- sprintf("fractional, weight treshold: %.2f", threshold)
shortDesc <- sprintf("frac-thld: %.2f", threshold)
links2 <- links %>% filter(n >= 0.5)

visPathfinder(nodes, links2, description=desc, shortDesc=shortDesc)

sourceCpp("Rcpp/tarjan.cpp")


from <- links %>% pull(from)
to <- links %>% pull(to)
weight <- links %>% pull(weight)
nds <- nodes %>% pull(scid)

make_network(nds, from, to, weight, TRUE) %>% strongly_connected_components()

nds <- c(1,2,3,4,5,6,7,8);
from <- c(1,2,2,2,3,3,4,4,5,5,6,7,8,8);
to   <- c(2,5,6,3,7,4,3,8,1,6,7,6,7,4);
weight <- 1:14;

make_network(nds, from, to, weight, TRUE) %>% strongly_connected_components()



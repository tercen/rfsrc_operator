library(tercen)
library(dplyr)
library(randomForestSRC)
library(reshape2)

ctx <- tercenCtx()

find_interactions <- ctx$op.value("find_interactions", as.logical, FALSE)
seed <- ctx$op.value("seed", as.integer, 42)
if(seed > 0) {
  seed <- -seed # rfsrc requires negative integer
} else {
  seed <- NULL
}
n_tree <- ctx$op.value("seed", as.double, 10000)
importance <- ctx$op.value("importance", as.character, "anti")

if (length(ctx$labels) < 1) stop("One or more label factors are required.")

pred.table <- ctx$select(unlist(list(ctx$labels, '.ci'))) %>%
  unique()

if(any(duplicated((pred.table)[[".ci"]]))) stop('One label is required')

table <- as.data.frame(
  ctx %>% select(.ci, .ri, .y) %>%
    reshape2::acast(.ci ~ .ri, value.var='.y', fun.aggregate = mean)
) %>%
  rename_all(.funs = function(cname) paste0('c', cname)) %>%
  bind_cols(pred.table %>% select(unlist(ctx$labels)) %>% as_tibble) %>%
  mutate_if(is.character, as.factor)

npred <- length(unlist(ctx$labels))
if(npred == 1) form <- paste0(unlist(ctx$labels), " ~ .")
if(npred > 1) form <- paste0("Multivar(", paste0(unlist(ctx$labels), collapse = ","), ") ~ .")

rf <- rfsrc(
  formula(form),
  data = table,
  seed = seed,
  ntree = n_tree,
  importance = importance
)

imp.table <- data.frame(
  .ri = seq(from = 0L, to = nrow(rf$importance) - 1L), 
  importance = rf$importance
) %>%
  mutate(.ri = as.integer(.ri)) %>%
  ctx$addNamespace()

if(find_interactions) {
  
  inter <- find.interaction(
    rf,
    method = "vimp",
    nrep = 3,
    seed = seed,
    importance = importance,
    verbose = FALSE
  ) %>%
    as.data.frame() %>%
    mutate(interaction = rownames(.)) %>%
    mutate(.ri = as.integer(gsub("c|:c.*", "", inter$interaction)))
  
  nm1 <- as.numeric(gsub("c|:c.*", "", inter$interaction)) + 1
  nm2 <- as.numeric(gsub("c.*:|c", "", inter$interaction)) + 1
  nms <- ctx$rselect(ctx$rnames)[[1]]
  
  inter.table <- inter %>% 
    mutate(interaction = paste0(nms[nm1], " - ", nms[nm2])) %>%
    ctx$addNamespace()
  
  ctx$save(list(imp.table, inter.table))

} else {
  
  ctx$save(imp.table)
  
}



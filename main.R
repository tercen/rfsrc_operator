library(tercen)
library(dplyr)
library(randomForestSRC)
library(reshape2)

set.seed(42)
ctx <- tercenCtx()

find_interactions <- FALSE
if(!is.null(ctx$op.value('find_interactions'))) find_interactions <- as.logical(ctx$op.value('find_interactions'))

if (length(ctx$labels) < 1) stop("One or more label factors are required.")

do.unique = function(df){
  result = unique(df)
  if (dim(result)[1] > 1) stop('One label is required')
  return (result %>% select_(.dots = ("-.ci")))
}
pred.table <- ctx$select(unlist(list(ctx$labels, '.ci'))) %>%
  group_by(.ci) %>%
  do(do.unique(.))

table <- as.data.frame(
  ctx %>% select(.ci, .ri, .y) %>%
    reshape2::acast(.ci ~ .ri, value.var='.y', fun.aggregate=mean)
) %>%
  rename_all(.funs = function(cname) paste0('c', cname)) %>%
  bind_cols(pred.table[unlist(ctx$labels)])

npred <- length(unlist(ctx$labels))
if(npred == 1) form <- paste0(unlist(ctx$labels), " ~ .")
if(npred > 1) form <- paste0("Multivar(", paste0(unlist(ctx$labels), collapse = ","), ") ~ .")
rf <- rfsrc(formula(form), data = table, importance = TRUE)

imp.table <- data.frame(
  .ri = seq(from = 0, to = length(rf$importance) - 1), 
  importance = -rf$importance
) %>% 
  ctx$addNamespace()

if(find_interactions) {
  inter <- find.interaction(rf, method = "vimp", nrep = 3)
  inter <- data.frame(inter)
  inter$interaction <- rownames(inter)
  
  inter$.ri <- as.numeric(gsub("c|:c.*", "", inter$interaction))
  
  nm1 <- as.numeric(gsub("c|:c.*", "", inter$interaction)) + 1
  nm2 <- as.numeric(gsub("c.*:|c", "", inter$interaction)) + 1
  nms <- ctx$rselect(ctx$rnames)[[1]]
  inter$interaction <- paste0(nms[nm1], " - ", nms[nm2])
  inter.table <- inter %>% ctx$addNamespace()
  
  list(imp.table, inter.table) %>%
    ctx$save()
} else {
  imp.table %>%
    ctx$save()
}



# ---------------------------------------------------------------------------- #
#
# channel.freq
#
# ---------------------------------------------------------------------------- #

# librairies ----

library(DT)
library(magrittr)
library(tidyverse)


# function ----

channel.freq <- function(table, return = 'DT', pix = '30%', se = F, mod.var = c(''), see.var = F){
  # table: data.frame with Groupin variable in 1st column and weighting one in 2nd.
  # table = df %>% select(CHANNEL, W, GENDER, AGE, LANGUE, STATUT, CHILD, WORK, STUDY, INCOME, INVESTS)
  
  table <- table %>% set_colnames(c('GROUP', colnames(.)[-1]))
  K <- nlevels(as.factor(table$GROUP))
  
  for(j in 3:ncol(table)){
    if(class(table[,j]) == 'factor'){
      temp <- sapply(levels(table[,j]), function(x) 100 * as.numeric(table[,j] == x)) %>% 
        as.data.frame() %>% set_colnames(paste0(levels(table[,j]), " - ", colnames(table)[j]))
      table <- table %>% cbind(temp)
    }}
  for(j in ncol(table):3){
    if(class(table[,j]) == 'factor') table <- table %>% select(-one_of(colnames(table)[j]))
  }
  
  dt <- table %>% filter(!is.na(GROUP)) %>% 
    group_by(GROUP) %>% 
    summarise_at(vars(-W, -GROUP), funs(round(wtd.mean(., weights = W, na.rm = T), 4))) %>% 
    select(-GROUP) %>% t() %>% 
    cbind(Kruskal = apply(table[,-c(1:2)], 2, function(x) kruskal.test(x = x, g = table$GROUP)$p.value)) %>% 
    as.data.frame() %>% 
    mutate(Variable = rownames(.)) %>%
    arrange(Kruskal) %>%  
    left_join(metadata, by = 'Variable') %>% 
    mutate(Kruskal     = ifelse(Kruskal < 0.001, '***', ifelse(Kruskal < 0.01, '**', as.character(round(Kruskal, 3)))),
           Variable    = sapply(strsplit(Variable, "- "), tail, 1),
           Description = as.character(Description)
    ) %>% 
    select(Description, one_of(paste0('V', 1:K)), Kruskal, Variable) %>%
    set_colnames(c('Description', levels(table$GROUP), '*', 'Var'))
  if(return == 'dt') return(dt)
  
  #colfunc <- colorRampPalette(c("#44B015", "white")); colfunc(K)
  xvs <- metadata$Description[sub(".*-", "", metadata$Variable) %in% mod.var] %>% unique()
  if(length(xvs) == 0) xvs <- ''
  
  javascript <- JS(
    'function(row, data) {
    var num_data = data.slice(1, data.length - 2)
    num_data.sort(function(a, b){ return a - b;});
    for(i = 1; i < data.length - 1; i++) {
      if(data[i]      == num_data[num_data.length - 1]) { $("td:eq("+i+")", row).css("background-color", "#44B015")
    } else if(data[i] == num_data[num_data.length - 2]) { $("td:eq("+i+")", row).css("background-color", "#72C34F")
    } else if(data[i] == num_data[num_data.length - 3]) { $("td:eq("+i+")", row).css("background-color", "#A1D78A")
    } else if(data[i] == num_data[num_data.length - 4]) { $("td:eq("+i+")", row).css("background-color", "#D0EBC4")
    } else if(data[i] == num_data[0]) { $("td:eq("+i+")", row).css("background-color", "#FFFFFF")
  }}}')
  
  if(see.var == F){ dt <- dt[,-ncol(dt)] }
  dt %>% 
    datatable(rownames = F, escape = F, filter = 'top', 
              options = list(dom = 'tp', autoWidth = T, columnDefs = list(list(width = pix, targets = 0)), rowCallback = javascript)) %>% 
    formatStyle(c(1), backgroundColor = '#E6E6E6') %>% 
    formatRound(levels(table$GROUP), digits = 2) %>% 
    formatStyle(c(0,1,K+3), fontSize = '14px') %>% 
    formatStyle(2:7, fontSize = '14px') %>% 
    formatStyle(column = 'Description', fontWeight = styleEqual(xvs, rep('bold', length(xvs))))
}
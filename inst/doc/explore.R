## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----setup, message=FALSE-----------------------------------------------------
library(treeheatr)

## ----fig.height=3, fig.width=7------------------------------------------------
dat_raw <- na.omit(penguins)
heat_tree(dat_raw, target_lab = 'species')

## ----fig.height=3, fig.width=7------------------------------------------------
heat_tree(
  dat_raw, target_lab = 'species',
  # target_cols = c('#E69F00', '#56B4E9', '#009E73'),
  target_cols = c('darkorange','purple','cyan4'),
  custom_layout = data.frame(id = 1, x = 0.1, y = 1), 
  show_all_feats = TRUE,
  panel_space = 0.05, target_space = 0.2, tree_space_bottom = 0.1, heat_rel_height = 0.4)

## ----fig.height=4, fig.width=7------------------------------------------------
heat_tree(
  dat_raw, target_lab = 'species',
  par_node_vars = list(
    label.size = 0.2,
    label.padding = ggplot2::unit(0.1, 'lines'),
    line_list = list(
      ggplot2::aes(label = paste('Node', id)),
      ggplot2::aes(label = splitvar),
      ggplot2::aes(label = paste('p =', formatC(p.value, format = 'e', digits = 2)))),
    line_gpar = list(
      list(size = 8),
      list(size = 8),
      list(size = 6)),
    id = 'inner'),
  terminal_vars = list(size = 0),
  cont_legend = TRUE, cate_legend = TRUE,
  edge_vars = list(size = 1, color = 'grey'))

## ----fig.width=9, fig.height=3.5, fig.show='hold', warning=FALSE, message=FALSE----
heat_tree(wine_quality_red, target_lab = 'target', lev_fac = 1, title = 'lev_fac = 1')
heat_tree(wine_quality_red, target_lab = 'target', title = 'lev_fac = 1.3')

## ----fig.height=4, fig.width=7, warning=F, message=F--------------------------
heat_tree(data = galaxy,
          target_lab = 'target',
          task = 'regression',
          terminal_vars = NULL,
          tree_space_bottom = 0)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(dplyr)
library(partykit)

selected_train <- train_covid %>%
  select(
    LDH = 'Lactate dehydrogenase',
    hs_CRP = 'High sensitivity C-reactive protein',
    Lymphocyte = '(%)lymphocyte',
    outcome = Type2
  ) %>%
  na.omit()

selected_test <- test_covid %>%
  select(
    LDH = 'Lactate dehydrogenase',
    hs_CRP = 'High sensitivity C-reactive protein',
    Lymphocyte = '(%)lymphocyte',
    'outcome'
  )

## ----fig.height=4.5, fig.width=7----------------------------------------------
# first argument indicates the index of the feature used for splitting

split_ldh <- partysplit(1L, breaks = 365)
split_crp <- partysplit(2L, breaks = 41.2)
split_lymp <- partysplit(3L, breaks = 14.7)

custom_tree <- partynode(1L, split = split_ldh , kids = list(
  partynode(2L, split = split_crp, kids = list(
    partynode(3L, info = 'Survival'),
    partynode(4L, split = split_lymp, kids = list(
      partynode(5L, info = 'Death'),
      partynode(6L, info = 'Survival'))))),
  partynode(7L, info = 'Death')))

heat_tree(
  selected_train,
  target_lab = 'outcome',
  label_map = c(`1` = 'Death', `0` = 'Survival'),
  custom_tree = custom_tree)

## ----eval = FALSE-------------------------------------------------------------
#  metrics = yardstick::metric_set(yardstick::f_meas)

## ----fig.height=4.5, fig.width=7----------------------------------------------
heat_tree(
  selected_train,
  data_test = selected_test,
  target_lab = 'outcome',
  label_map = c(`1` = 'Death', `0` = 'Survival'),
  print_eval = TRUE,
  custom_tree = custom_tree,
  lev_fac = 3
)


## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----setup, message=FALSE-----------------------------------------------------
library(treeheatr)

## ----fig.height=3, fig.width=7------------------------------------------------
# To avoid warnings, for now, apply na.omit():
penguins <- na.omit(penguins)
heat_tree(penguins, target_lab = 'species')

## ----fig.height=3, fig.width=7------------------------------------------------
heat_tree(
  penguins, target_lab = 'species',
  target_cols = c('#E69F00', '#56B4E9', '#009E73'),
  # moving node 3 a bit to the left:
  custom_layout = data.frame(id = 3, x = 0.1, y = 0.5), 
  show_all_feats = TRUE,
  panel_space = 0.05, target_space = 0.2, tree_space_bottom = 0.1, heat_rel_height = 0.4)

## ----fig.height=4, fig.width=7------------------------------------------------
heat_tree(
  penguins, target_lab = 'species',
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

## ----fig.height=3-------------------------------------------------------------
heat_tree(penguins, target_lab = 'species', show = 'tree-only')

## ----fig.height=4-------------------------------------------------------------
heat_tree(penguins, target_lab = 'species', feats = c('body_mass_g', 'sex'))

## ----fig.height=3-------------------------------------------------------------
heat_tree(penguins, target_lab = 'species', feats = NA, heat_rel_height = 0.1)

## -----------------------------------------------------------------------------
# # same tree as letting `x = train_covid`:
# x <- partykit::ctree(Outcome ~ ., data = train_covid)

# build tree using rpart:
x <- partykit::as.party(rpart::rpart(Outcome ~ ., data = train_covid))

heat_tree(x = x, label_map = c(`1` = 'Deceased', `0` = 'Survived'))

## ----fig.height=4.5, fig.width=7, warning=FALSE, message=FALSE----------------
library(partykit)

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
  x = custom_tree,
  data_test = train_covid,
  print_eval = FALSE,
  target_lab = 'Outcome',
  label_map = c(`1` = 'Deceased', `0` = 'Survived'))

## ----eval = FALSE-------------------------------------------------------------
#  metrics = yardstick::metric_set(yardstick::f_meas)

## ----fig.height=4.5, fig.width=7----------------------------------------------
heat_tree(
  x = custom_tree,
  data_test = test_covid,
  target_lab = 'Outcome',
  label_map = c(`1` = 'Death', `0` = 'Survival'),
  lev_fac = 3)

## ----fig.height=4, fig.width=7, warning=F, message=F--------------------------
heat_tree(x = galaxy,
          target_lab = 'target',
          task = 'regression',
          terminal_vars = NULL,
          tree_space_bottom = 0)

## ----fig.width=9, fig.height=3.5, fig.show='hold', warning=FALSE, message=FALSE----
heat_tree(wine_quality_red, target_lab = 'target', lev_fac = 1, title = 'lev_fac = 1')
heat_tree(wine_quality_red, target_lab = 'target', title = 'lev_fac = 1.3')


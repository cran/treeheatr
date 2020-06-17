# library(testthat)
# source('R/def_testing_data.R')
context('heat_tree')

testing_data <- iris %>%
  group_by(Species) %>%
  slice(1:5) %>%
  ungroup()

test_that('error when supplied incorrect target label', {
  expect_error(heat_tree(testing_data, target_lab = 'species'))
})


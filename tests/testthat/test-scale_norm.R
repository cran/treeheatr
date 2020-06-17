context('scale_norm')

test_that('percentizing works', {
  expect_equal(scale_norm(c(1,2,3,5), 'percentize'), c(0.25, 0.5, 0.75, 1))
})

test_that('normalizing works', {
  expect_equal(scale_norm(c(1,2,3,5), 'normalize'), c(0, 0.25, 0.5, 1))
})

test_that('scaling works', {
  expect_equal(scale_norm(0:1, 'scale'), 1/sqrt(2)*c(-1, 1))
})


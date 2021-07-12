context("Tests for function distr.btest")

test_that(desc = "Print call", {
  data("sinoForest")
  res <- digitTests::distr.test(x = sinoForest$value, check = 'first', reference = 'benford')
  invisible({capture.output({ print(res) }) })
  expect_equal(length(res$digits), 9)
})

test_that(desc = "Validate Derks et al. (2020)", {
  data("sinoForest")
  res <- digitTests::distr.btest(x = sinoForest$value, check = 'first', reference = 'benford', BF10 = FALSE)
  expect_equal(as.numeric(res$bf), 6899678.1488)
})

test_that(desc = "Validate uniform distribution", {
  res <- digitTests::distr.btest(x = 1:9, check = 'first', reference = 'uniform', BF10 = FALSE)
  expect_equal(as.numeric(res$bf), 22.77012458)
})

test_that(desc = "Validate benford.analysis package", {
  ba <- benford.analysis::benford(data = sinoForest$value, number.of.digits = 1)
  dt <- digitTests::distr.test(x = sinoForest$value, check = "first")
  expect_equal(as.numeric(ba$bfd$data.dist.freq), as.numeric(dt$observed))
})
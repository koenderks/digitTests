context("Tests for function distr.test")

test_that(desc = "Validate Derks et al. (2020)", {
  data("sinoForest")
  res <- digitTests::distr.test(x = sinoForest$value, check = 'first', reference = 'benford')
  expect_equal(as.numeric(res$n), 772)
  expect_equal(as.numeric(res$statistic), 7.6517426989499855)
  expect_equal(as.numeric(res$parameter), 8)
  expect_equal(as.numeric(res$p.value), 0.46820638130036729)
})

test_that(desc = "Validate uniform distribution", {
  set.seed(1)
  res <- digitTests::distr.test(x = 1:9, check = 'first', reference = 'uniform')
  expect_equal(as.numeric(res$n), 9)
  expect_equal(as.numeric(res$statistic), 0)
  expect_equal(as.numeric(res$parameter), 8)
  expect_equal(as.numeric(res$p.value), 1)
})

test_that(desc = "Validate benford.analysis package first digits", {
  ba <- benford.analysis::benford(data = sinoForest$value, number.of.digits = 1)
  dt <- digitTests::distr.test(x = sinoForest$value, check = "first")
  expect_equal(as.numeric(ba$bfd$data.dist.freq), as.numeric(dt$observed))
  expect_equal(as.numeric(ba[["stats"]]$chisq$statistic), as.numeric(dt$statistic))
  expect_equal(as.numeric(ba[["stats"]]$chisq$parameter), as.numeric(dt$parameter))
  expect_equal(as.numeric(ba[["stats"]]$chisq$p.value), as.numeric(dt$p.value))
})

test_that(desc = "Validate benford.analysis package first and second digits", {
  ba <- benford.analysis::benford(data = sinoForest$value, number.of.digits = 2)
  dt <- digitTests::distr.test(x = sinoForest$value, check = "firsttwo")
  expect_equal(as.numeric(ba$bfd$data.dist.freq), as.numeric(dt$observed))
  expect_equal(as.numeric(ba[["stats"]]$chisq$statistic), as.numeric(dt$statistic))
  expect_equal(as.numeric(ba[["stats"]]$chisq$parameter), as.numeric(dt$parameter))
  expect_equal(as.numeric(ba[["stats"]]$chisq$p.value), as.numeric(dt$p.value))
})
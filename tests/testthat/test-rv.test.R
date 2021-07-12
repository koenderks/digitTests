context("Tests for function rv.test")

test_that(desc = "Validate Datacolada[77]", {
  data("sanitizer")
  res <- digitTests::rv.test(x = sanitizer$value, check = 'last', method = 'af')
  expect_equal(as.numeric(res$statistic), 1.5225)
  res <- digitTests::rv.test(x = sanitizer$value, check = 'last', method = 'entropy')
  expect_equal(as.numeric(res$statistic), 7.065769174)
})
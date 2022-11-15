testthat::test_that("Explicitly set legacy RNG", {
  options(jaspLegacyRngKind = TRUE)
  options <- jaspTools::analysisOptions("rngKind")
  set.seed(1)
  results <- jaspTools::runAnalysis("rngKind", NULL, options)
  table <- results[["results"]][["result"]][["data"]]
  jaspTools::expect_equal_tables(table, list(266))
})

testthat::test_that("Latest R default RNG", {
  options(jaspLegacyRngKind = FALSE)
  on.exit(options(jaspLegacyRngKind = NULL))
  options <- jaspTools::analysisOptions("rngKind")
  set.seed(1)
  results <- jaspTools::runAnalysis("rngKind", NULL, options)
  table <- results[["results"]][["result"]][["data"]]
  jaspTools::expect_equal_tables(table, list(836))
})

testthat::test_that("Default begavior - legacy RNG", {
  options <- jaspTools::analysisOptions("rngKind")
  set.seed(1)
  results <- jaspTools::runAnalysis("rngKind", NULL, options)
  table <- results[["results"]][["result"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(266))
})

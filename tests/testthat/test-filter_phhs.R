library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

testthat::test_that("PHH population filtering returns input if population not enabled", {

  phh_inregion <- dplyr::tibble(id = 1:100)
  use_pops <- FALSE
  region_pop <- 10
  min_phh_pop <- 3

  filtered_phhs <- filter_phhs(use_pops, region_pop, phh_inregion, min_phh_pop)

  testthat::expect_equal(filtered_phhs, phh_inregion)
})

testthat::test_that("PHH population filtering returns input if no PHHs need to be removed", {

  # each PHH will get 100/10= 10 population, min is 3, no filtering required
  phh_inregion <- dplyr::tibble(id = 1:10)
  use_pops <- TRUE
  region_pop <- 100
  min_phh_pop <- 3

  filtered_phhs <- filter_phhs(use_pops, region_pop, phh_inregion, min_phh_pop)

  testthat::expect_equal(filtered_phhs, phh_inregion)
})


testthat::test_that("PHH population filtering returns only first PHH if min pop cannot be achieved", {

  # each PHH will get 100/10= 10 population, min is 3, no filtering required
  phh_inregion <- dplyr::tibble(id = 1:10)
  use_pops <- TRUE
  region_pop <- 10
  min_phh_pop <- 30

  filtered_phhs <- filter_phhs(use_pops, region_pop, phh_inregion, min_phh_pop)

  testthat::expect_equal(filtered_phhs, phh_inregion[1,])
})


testthat::test_that("PHH population filtering respects minimum population request", {

  # test set: 200 PHHs, 1000 total population
  phh_inregion <- dplyr::tibble(id = 1:200)
  use_pops <- TRUE
  min_phh_pops <- 1:1000
  region_pop <- 1000

  # test for each min pop between 1 and the region pop
  results <- purrr::map_lgl(min_phh_pops, function(min_phh_pop) {

    filtered_phhs <- filter_phhs(use_pops, region_pop, phh_inregion, min_phh_pop)
    phh_pop <- region_pop / nrow(filtered_phhs)

    return(phh_pop >= min_phh_pop)
  })

  testthat::expect_true(all(results))
}) # end test "PHH population filtering respects minimum population request"

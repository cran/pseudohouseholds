library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# test that single test data case passes validation
testthat::test_that("validate_phhs() works for single region", {

  phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
  regions <- region_shp
  region_idcol <- "region_id"
  region_popcol <- "population"

  validation <- validate_phhs(phhs, regions, region_idcol, region_popcol)
  result <- unique(validation$result)

  testthat::expect_equal(result, "Passed")

})


# test that single test data case passes validation
testthat::test_that("validate_phhs() finds wrong PHH populations", {

  phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
  regions <- region_shp
  region_idcol <- "region_id"
  region_popcol <- "population"

  phhs$population[[5]] <- 0

  validation <- validate_phhs(phhs, regions, region_idcol, region_popcol)
  result <- validation$result[[1]]

  testthat::expect_equal(result, "Failed")

})

# test that complex test data case passes validation
testthat::test_that("validate_phhs() works with complex test data", {
  regions <- ottawa_db_shp[1:5,]
  roads <- ottawa_roads_shp
  region_idcol <- "DBUID"
  region_popcol <- "dbpop2021"

  phhs <- get_phhs_parallel(region = regions, region_idcol = region_idcol, region_popcol = region_popcol, roads = roads, roads_idcol = "NGD_UID")


  # test that it works properly with expected inputs and outputs
  validation <- validate_phhs(phhs, regions, region_idcol, region_popcol)

  result <- unique(validation$result)

  testthat::expect_equal(result, "Passed")

  # test that it finds incorrect populations
  phhs_fortest <- phhs
  phhs_fortest$dbpop2021[[5]] <- 1000

  validation <- validate_phhs(phhs_fortest, regions, region_idcol, region_popcol)

  result <- validation$result[[1]]

  testthat::expect_equal(result, "Failed")
  testthat::expect_equal(validation$failing_regions[[1]], phhs$DBUID[[5]])

  # test that it finds missing regions

  phhs_fortest <- phhs |>
    dplyr::filter(DBUID != regions$DBUID[[1]])

  validation <- validate_phhs(phhs_fortest, regions, region_idcol, region_popcol)

  result <- validation$result[[2]]

  testthat::expect_equal(result, "Failed")
  testthat::expect_equal(validation$failing_regions[[2]], regions$DBUID[[1]])

})


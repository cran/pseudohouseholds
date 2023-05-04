library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)


testthat::test_that("Sequential processing words", {
  # dplyr::bind_row() works interactively but fails in testing fails in testing during R CMD CHECK
  #regions <- dplyr::bind_rows(region_shp, region_shp, region_shp)
  regions <- region_shp
  regions[2,] <- region_shp
  regions[3,] <- region_shp
  regions$region_id <- 1:nrow(regions)
  regions <- sf::st_as_sf(regions)

  phhs <- get_phhs_parallel(regions = regions, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")

  #Result has correct classes"
  testthat::expect_equal(class(phhs), c("sf", "tbl_df", "tbl", "data.frame"))

  #Result has correct number of rows
  testthat::expect_equal(nrow(phhs), 105)

  # PHH populations sum to original region population
  testthat::expect_equal(sum(phhs$population), sum(regions$population))
})

library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# Unit tests for get_phhs_single(), function that finds PHH for a single region

testthat::test_that(
  "Full function works: correct classes, number of rows, and population",{
    phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")

    #Result has correct classes"
    testthat::expect_equal(class(phhs), c("sf", "tbl_df", "tbl", "data.frame"))

    #Result has correct number of rows
    testthat::expect_equal(nrow(phhs), 35)

    # PHH populations sum to original region population
    testthat::expect_equal(sum(phhs$population), region_shp$population)
  }
)

testthat::test_that(
  "Full function works and warns with no population column",{

    phhs <- suppressWarnings(get_phhs_single(region = region_shp, region_idcol = "region_id",  roads = road_shp, roads_idcol = "road_id"))

    #Function gives warning
    testthat::expect_warning(get_phhs_single(region = region_shp, region_idcol = "region_id",  roads = road_shp, roads_idcol = "road_id"))

    # Result has correct classes
    testthat::expect_equal(class(phhs), c("sf", "tbl_df", "tbl", "data.frame"))

    # Result has correct number of rows
    testthat::expect_equal(nrow(phhs), 35)

    # Result does not have a population column
    testthat::expect_true(!"population" %in% colnames(phhs))
  }
)


testthat::test_that(
  "Full function works and warns with no road_id column",{

    phhs <- suppressWarnings(get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp))

    # Function gives warning
    testthat::expect_warning(get_phhs_single(region = region_shp, region_popcol = "population", region_idcol = "region_id",  roads = road_shp))

    #Result has correct classes
    testthat::expect_equal(class(phhs), c("sf", "tbl_df", "tbl", "data.frame"))

    #Result has correct number of rows
    testthat::expect_equal(nrow(phhs), 35)

    # PHH populations sum to original region population
    testthat::expect_equal(sum(phhs$population), region_shp$population)
  }
)


testthat::test_that("Error if input CRSs are different or are WGS84", {
  region_shp_wgs84 <- sf::st_transform(region_shp, crs="WGS84")
  road_shp_wgs84 <- sf::st_transform(road_shp, crs="WGS84")

  testthat::expect_error(get_phhs_single(region = region_shp_wgs84, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp_wgs84, roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp_wgs84, region_idcol = "region_id", region_popcol = "population", roads = road_shp_wgs84, roads_idcol = "road_id"))
})

testthat::test_that("Error if region or road inputs are not sf objects", {
  testthat::expect_error(get_phhs_single(region = dplyr::tibble(), region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = dplyr::tibble(), roads_idcol = "road_id"))
})

testthat::test_that("Error if no roads or regions supplied (0-row sf objects) or more than one region supplied", {
  regions_shp <- dplyr::bind_rows(region_shp, region_shp)

  testthat::expect_error(get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp[0,], roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp[0,], roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp[0,], region_idcol = "region_id", region_popcol = "population", roads = road_shp[0,], roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = regions_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp[0,], roads_idcol = "road_id"))
})

testthat::test_that("Error if bad column IDs supplied", {
  testthat::expect_error(get_phhs_single(region = region_shp[1,], region_idcol = "asdf", region_popcol = "population", roads = road_shp, roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp[1,], region_idcol = "region_id", region_popcol = "asdf", roads = road_shp, roads_idcol = "road_id"))
  testthat::expect_error(get_phhs_single(region = region_shp[1,], region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "asdsf"))
})





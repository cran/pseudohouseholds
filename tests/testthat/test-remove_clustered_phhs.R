library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# test function remove_clustered_phhs()


# ensure that for a range of values, the distances between points are always
# above the desired minimum
testthat::test_that("PHH cluster meets minimum distance criteria", {
  phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
  buffer_sizes <- c(0, 10, 100, 250, 500, 1000, 5000)

  # for each buffer_size, run the buffering function and ensure distances
  # between remaining points are greater than the buffer size
  results <- purrr::map_lgl(buffer_sizes, function(buffer_size) {

    filtered <- remove_clustered_phhs(phh_inregion_filtered = phhs, min_phh_distance = buffer_size )
    distances <- sf::st_distance(filtered)
    all(as.numeric(distances[lower.tri(distances)]) > buffer_size)

  }) # end purrr::map_lgl

  testthat::expect_true(all(results))

}) # end test "PHH cluster meets minimum distance criteria"


testthat::test_that("No changes to CRS or class", {
  phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
  filtered <- remove_clustered_phhs(phh_inregion_filtered = phhs, min_phh_distance = 500 )

  testthat::expect_equal(class(phhs), class(filtered))
  testthat::expect_equal(sf::st_crs(phhs), sf::st_crs(filtered))
}) # end test "No changes to CRS or class"


testthat::test_that("Filtered points are in original points (no changes to geometry)", {

  phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id", region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
  buffer_sizes <- c(0, 10, 100, 250, 500, 1000, 5000)

  # for each buffer_size, run the buffering function and ensure returned points
  # exactly overlap input points: no movement or addition
  results <- purrr::map_lgl(buffer_sizes, function(buffer_size) {

    filtered <- remove_clustered_phhs(phh_inregion_filtered = phhs, min_phh_distance = buffer_size )

    # each filtered point intersects an original point
    sum(sf::st_intersects(filtered, phhs, sparse = FALSE)) == nrow(filtered)
  })

  testthat::expect_true(all(results))

}) # end test "Filtered points are in original points (no changes to geometry)", {

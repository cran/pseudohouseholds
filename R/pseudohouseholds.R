PHH_TYPE_REGULAR <- 0
PHH_TYPE_UNPOPULATED <- 1
PHH_TYPE_NO_VALID_PHHS_FOUND <- 2

#' Get Pseudo-Households (PHH) for many regions, with optional parallel processing
#'
#' Calculate PHHs for a set of regions using a given road network.
#'
#' Regions will be processed sequentially by default, but parallel processing
#' is supported if users call future::plan() before calling this function.
#'
#' This function is a wrapper around get_phhs_single(), and parameters are
#' passed on to it.
#'
#' @param regions simple feature object, sf tibble where each row is a region
#' @param region_idcol character, name of column with unique region id
#' @param region_popcol character, name of column with region population
#' @param roads simple feature object, lines or polylines with road network
#' @param roads_idcol character, name of column containing road unique identifiers
#' @param phh_density numeric, parameter given to sf::st_line_sample()
#' @param min_phh_pop numeric, minimum population per phh
#' @param min_phhs_per_region numeric, minimum phhs per region (it will try its best)
#' @param min_phh_distance numeric, minimum distance between phhs in meters
#' @param road_buffer_m numeric, buffer in meters for intersections
#' @param delta_distance_m numeric, buffer in meters for intersections
#' @param skip_unpopulated_regions boolean, should we skip regions with no population?
#'
#' @return a simple feature object with one row per phh in the region
#'
#' @export
#'
#' @examples
#'  # Create PHHs for the first 2 dissemination blocks in Ottawa, Ontario, without
#'  # using any parallel processing
#'  library(sf)
#'  library(pseudohouseholds)
#'  phhs <- get_phhs_parallel(region = ottawa_db_shp[1:2,], region_idcol = "DBUID",
#'  region_popcol = "dbpop2021", roads = ottawa_roads_shp, roads_idcol = "NGD_UID")
#'
#'
#'  # Create PHHs for the first 20 dissemination blocks in Ottawa, Ontario, using
#'  # parallel processing (consult documentation for the package future for details
#'  # about parallel processing).
#'  \donttest{
#'  library(future)
#'  future::plan(future::multisession)
#'  phhs <- get_phhs_parallel(region = ottawa_db_shp[1:20,], region_idcol = "DBUID",
#'   region_popcol = "dbpop2021", roads = ottawa_roads_shp, roads_idcol = "NGD_UID")
#'
#'  # Shut down parallel workers
#'  future::plan(future::sequential)
#' }
#'
get_phhs_parallel <- function(regions, region_idcol, roads, region_popcol = NA, roads_idcol = NA, phh_density = 0.005, min_phh_pop = 5, min_phhs_per_region = 1, min_phh_distance = 25, road_buffer_m = 5, delta_distance_m = 5, skip_unpopulated_regions = TRUE ){

  # regions must each have a unique id
  region_ids <- unique(regions[, region_idcol, drop = TRUE])
  if (length(region_ids) < nrow(regions)) stop("Regions must each have a unique id.")

  # split input regions into a list for mapping over
  regions_list <- split(regions, region_ids)

  # iterate in parallel over regions
  phh_candidates <- furrr::future_map(regions_list, ~{
    get_phhs_single(region = .x, region_idcol = region_idcol, region_popcol = region_popcol,
                    roads = roads, roads_idcol = roads_idcol,
                    phh_density = phh_density, min_phh_pop = min_phh_pop,
                    min_phhs_per_region = min_phhs_per_region,
                    min_phh_distance = min_phh_distance,
                    road_buffer_m = road_buffer_m,
                    delta_distance_m = delta_distance_m,
                    skip_unpopulated_regions = skip_unpopulated_regions,
                    track_warnings = TRUE)
  } , .options=furrr::furrr_options(seed=NULL)
  ,.progress = TRUE
  )

  # extract the valid phhs
  phh_valid <- phh_candidates[vapply(phh_candidates, length, FUN.VALUE = 1) > 0]

  # tidy them up
  phhs <- dplyr::bind_rows(phh_valid)

  # remove any temporary options used to suppress repeated warnings
  warning_cleanup()

  return(phhs)
}

#' Get Pseudo-Households (PHH) for a single region
#'
#' @param region simple feature object, one-row sf tibble
#' @param region_idcol character, name of column with unique region id
#' @param region_popcol character, name of column with region population
#' @param roads simple feature object, lines or polylines with road network
#' @param roads_idcol character, name of column containing road unique identifiers
#' @param phh_density numeric, parameter given to sf::st_line_sample()
#' @param min_phh_pop numeric, minimum population per phh
#' @param min_phhs_per_region numeric, minimum phhs per region (it will try its best)
#' @param min_phh_distance numeric, minimum distance between phhs in meters
#' @param road_buffer_m numeric, buffer in meters for intersections
#' @param delta_distance_m numeric, buffer in meters for intersections
#' @param skip_unpopulated_regions boolean, should we skip regions with no population?
#' @param track_warnings boolean, internal parameter used when this function is
#'        called by get_phhs_parallel() to ensure warnings are only shown once.
#'
#' @return a simple feature object with one row per phh in the region
#' @export
#'
#' @examples
#' phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id",
#' region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
#'
get_phhs_single <- function(region, region_idcol, roads, region_popcol = NA, roads_idcol = NA, phh_density = 0.005, min_phh_pop = 5, min_phhs_per_region = 1, min_phh_distance = 25, road_buffer_m = 5, delta_distance_m = 5, skip_unpopulated_regions = TRUE, track_warnings = FALSE ){

  ## INPUT VALIDATION

  # check region and roads are both sf objects
  if ((!"sf" %in% class(region)) | (!"sf" %in% class(roads))) stop("Region and roads must both be simple feature (sf) objects.")

  # check region_idcol is a valid column name
  if ((!region_idcol %in% colnames(region))) stop("Parameter region_idcol must name a valid column in region.")

  # check region_popcol parameter
  if (is.na(region_popcol)) {
    use_pops <- FALSE
    warn_once ("No region population column provided. PHHs will not be assigned populations.", track_warnings)
  } else {
    use_pops <- TRUE
    if (!region_popcol %in% colnames(region))  stop("Parameter region_popcol must name a valid column in region.")
  }

  # check road idcol
  if (is.na(roads_idcol)) {
    warn_once ("No roads id column provided. PHHs will not be traceable back to road segments.", track_warnings)
  } else {
    if (!roads_idcol %in% colnames(roads)) stop("Parameter roads_idcol must name a valid column in region.")
  }

  # check same CRS between region and roads
  region_crs <- sf::st_crs(region)
  roads_crs <- sf::st_crs(roads)
  if (region_crs != roads_crs) stop("Region and roads must have same CRS (coordinate reference system).")

  # if no road idcol is given, we give each road a simple numeric identifier
  # and a silly column ID name so we can remove it later
  if (is.na(roads_idcol)) {
    roads_idcol <- "road_id_NA"
    region$road_id_NA <- 1:nrow(region)
  }

  # check region has only one row
  if (nrow(region) != 1) stop ("One and only one region must be provided at a time in a one-row sf tibble. Use lapply or purrr::map for many regions.")

  # check roads have at least one row
  if (nrow(roads) == 0) stop ("No road data detected in roads input. Must have at least one row.")

  # WGS84 won't work
  if (grepl(region_crs$input, "WGS84")) stop("Input CRS is WGS84, which is not supported. Did you mean to use a projected CRS instead?")


  ## PRELIMINARY INPUT PROCESSING
  # attributes are assumed to be constant across all simple feature inputs
  sf::st_agr(region) = "constant"
  sf::st_agr(roads) = "constant"

  # remove unnecessary road columns
  roads <- dplyr::select(roads, dplyr::any_of(roads_idcol))

  # if we are using populations, set that up here, and also
  if (use_pops){
    region_pop <- region[, region_popcol, drop=TRUE];

    # if no population and we're skipping such regions, return empty result
    # if no population and we're including such regions, return default response
    if (region_pop == 0) {
      if (skip_unpopulated_regions){
        result <- dplyr::tibble()
      }  else {
        result <- create_default_response(region = region, region_idcol = region_idcol, roads_idcol = roads_idcol, type = PHH_TYPE_UNPOPULATED)
      }

      return(result);
    }
  }



  # get the roads that intersect the region plus a buffer
  # we cast to multilinestring and then back to linestring to deal with disconnected multilinestrings
  roads_touching_region <- sf::st_intersection(roads, sf::st_buffer(region, road_buffer_m)) |>
    sf::st_cast("MULTILINESTRING", warn = FALSE) |>
    sf::st_cast("LINESTRING", warn = FALSE)

  # ggplot() + geom_sf(data=region) + geom_sf(data=roads_touching_region)

  # if it doesn't intersect any roads, return default response
  if (nrow(roads_touching_region) == 0) {
    result <- create_default_response(region = region, region_idcol = region_idcol, roads_idcol = roads_idcol, type = PHH_TYPE_NO_VALID_PHHS_FOUND)
    return(result)
  }

  ## set candidate PHHs by sampling road segments at the density set by the user
  # cast to points
  phh_onstreet <- sf::st_line_sample(roads_touching_region, density = phh_density) |>
    sf::st_cast("POINT")

  # If line sampling returns no points (e.g. because each segment is too short for
  # the sampling density), sample 1 point per region-intersecting line segment
  if (length(phh_onstreet) == 0) {
    phh_onstreet <- sf::st_line_sample(roads_touching_region,n=1) |>
      sf::st_cast("POINT")
  }

  # if we don't get enough candidate points because of density being too low, take the longest road segments
  # and sample from them manually
  if (length(phh_onstreet) < min_phhs_per_region) {
    roads_touching_region$lengths <- sf::st_length(roads_touching_region)
    roads_for_sample <- roads_touching_region |> dplyr::arrange(dplyr::desc(lengths)) |> dplyr::slice_head(n=min_phhs_per_region)
    num_to_sample <- ceiling(min_phhs_per_region/nrow(roads_for_sample))

    phh_onstreet <- sf::st_line_sample(roads_for_sample, n=num_to_sample) |>
      sf::st_cast("POINT")
  }

  # add back road information for traceability
  # this is convoluted because points don't intersect lines reliably
  # need to buffer the road, get point intersections
  # for future optimization, explore buffering the points instead, may be faster
  sf::st_agr(roads_touching_region) = "constant"
  phh_onstreet <- sf::st_intersection(sf::st_buffer(roads_touching_region, 1), phh_onstreet)

  # ggplot() + geom_sf(data=region) + geom_sf(data=phh_onstreet)

  # get phh points slightly off the street based on the on-street points
  # for all points, we draw a line from it to the centroid, then create two
  # candidate points: one closer and one farther along the line. Then we keep
  # any (probably one) that are inside the region. This seems to work well with
  # weird convexities and strange crescent geometries, giving a good number of
  # points. If there are too many too close together we thin them out later.
  phh_inregion <- get_phh_points (region, phh_onstreet, region_idcol, roads_idcol, delta_distance = delta_distance_m)

  #ggplot() + geom_sf(data=region) + geom_sf(data=roads_touching_region) + geom_sf(data=phh_inregion)

  ## if we're using populations, make sure we get the right number of points
  # to respect the minimum population requirements
  phh_inregion_filtered <- filter_phhs(use_pops, region_pop, phh_inregion, min_phh_pop)

  ## make sure PHHs aren't too close together
  # create a buffer around them of radius 0.5 the min separation distance
  phh_keepers <- remove_clustered_phhs (phh_inregion_filtered, min_phh_distance)

  #ggplot() + geom_sf(data=region) + geom_sf(data=roads_touching_region) + geom_sf(data=phh_keepers)

  result <- sf::st_as_sf(dplyr::as_tibble(phh_keepers))

  # set the region idcolumn. note all phhs will belong to the same one region!
  region_id <- as.character(region[, region_idcol, drop = TRUE])
  result[, region_idcol] <- region_id

  # set unique phh id: region id, then period, then sequential numbers
  result$phh_id <- paste0(region_id, ".", 1:nrow(result))

  # set the population by distributing it evenly across all phhs
  if (use_pops){
    #result$pop <- as.numeric(region_pop/nrow(result))
    result[, region_popcol] <- as.numeric(region_pop/nrow(result))
  }

  if ("x" %in% colnames(result)) {
    result$geometry <- result$x
    result$x <- NULL
  }

  if (roads_idcol == "road_id_NA") result$road_id_NA <- NULL

  # make it a real sf object
  result <- sf::st_as_sf(result, crs = region_crs)

  return(result)
}


# Internal function to create candidate off-road PHHs from points samples on
# road networks. For each point on the road, the algorithm creates two candidate
# PHHs: one moved directly towards the region centroid, and one moved away from
# it. The distance to move is given by the paramter delta_distance.
# Creating these two candidates helps ensure we have valid PHHs even for regions
# with weird shapes--e.g. a banana or a donut, where the centroid is outside of
# the region, and so simply pulling shapes from the edges will often move them
# outside the region. In cases where both candidates are valid, unreasonably
# close PHHs will be cleaned up later.
get_phh_points <- function(db, phh_onstreet, region_idcol, roads_idcol, delta_distance = 5){

  # for clean R CMD CHECK
  PHH_X_pull <- PHH_X_push <- PHH_Y_pull <- PHH_Y_push <- geometry <- .tempid <- NULL

  # get db centroid coordinates
  db_centroid <- sf::st_centroid(db)
  db_centroid_coords <- dplyr::as_tibble(sf::st_coordinates(db_centroid))
  colnames(db_centroid_coords) <- c("DB_X", "DB_Y")

  # get phh coordinates
  phh_onstreet_coords <- dplyr::as_tibble(sf::st_coordinates(phh_onstreet))
  colnames(phh_onstreet_coords) <- c("PHH_X", "PHH_Y")
  phh_onstreet_coords[, roads_idcol] <- phh_onstreet[, roads_idcol, drop=TRUE]

  # create data table for vectorized anaylsis
  phh_foranalysis <-   dplyr::bind_cols(phh_onstreet_coords, db_centroid_coords)

  # For each point on the road network, create two potential PHHs: one "pulled"
  # towards the region centroid, and one pushed away. This code block just
  # gets a unit vector from the point to the centroid and then adds/subtracts
  # it from each point on the road.
  # Note: In benchmarking, using base R was ~100x faster than dplyr
  phh_foranalysis$deltaY = phh_foranalysis$DB_Y - phh_foranalysis$PHH_Y
  phh_foranalysis$deltaX = phh_foranalysis$DB_X - phh_foranalysis$PHH_X
  phh_foranalysis$magnitude = sqrt(phh_foranalysis$deltaY^2 + phh_foranalysis$deltaX^2)
  phh_foranalysis$unit_vecY = phh_foranalysis$deltaY/phh_foranalysis$magnitude
  phh_foranalysis$unit_vecX = phh_foranalysis$deltaX/phh_foranalysis$magnitude
  phh_foranalysis$PHH_X_pull = phh_foranalysis$PHH_X + phh_foranalysis$unit_vecX * delta_distance
  phh_foranalysis$PHH_Y_pull = phh_foranalysis$PHH_Y + phh_foranalysis$unit_vecY * delta_distance
  phh_foranalysis$PHH_X_push = phh_foranalysis$PHH_X - phh_foranalysis$unit_vecX * delta_distance
  phh_foranalysis$PHH_Y_push = phh_foranalysis$PHH_Y - phh_foranalysis$unit_vecY * delta_distance

  # extract input CRS so we can re-assign it later
  original_crs <- sf::st_crs(phh_onstreet)

  # create separate sf objects for the pushed and pulled PHH candidates
  phh_push <- sf::st_as_sf(dplyr::select(phh_foranalysis, PHH_X_push, PHH_Y_push),
                           coords = c("PHH_X_push","PHH_Y_push"),
                           crs=original_crs)

  phh_pull <- sf::st_as_sf(dplyr::select(phh_foranalysis, PHH_X_pull, PHH_Y_pull),
                           coords = c("PHH_X_pull","PHH_Y_pull"),
                           crs=original_crs)

  # consider both at once
  phh_pushpull <- dplyr::bind_rows(phh_push, phh_pull)
  phh_pushpull[, roads_idcol] <- rep(x = phh_onstreet[, roads_idcol, drop=TRUE], times=2)

  # ggplot() + geom_sf(data=db) +  geom_sf(data=roads_touching_region) + geom_sf(data=phh_push, colour="red")+ geom_sf(data=phh_pull, colour="blue") +  geom_sf(data=db_centroid)

  # remove candidates that aren't inside the region
  # if we proceed, these are regular PHHs and we type them as such
  phh_indb <- sf::st_filter(phh_pushpull, db)
  phh_indb$phh_type <- PHH_TYPE_REGULAR

  # strange geometries with only a few border roads can lead to no points inside
  # in this case, we sample 16 points distributed radially around the candidate point
  #  and keep the first one we find that's inside the region
  if (nrow(phh_indb) == 0) {
    phh_onstreet_foranalysis <- phh_onstreet
    phh_onstreet_foranalysis$.tempid <- 1:nrow(phh_onstreet)
    sf::st_agr(phh_onstreet_foranalysis) = "constant"
    pts_spread <- sf::st_buffer(phh_onstreet_foranalysis, delta_distance, nQuadSegs = 4) |>
      sf::st_cast("POINT")

    # ggplot(pts_spread) + geom_sf()

    pts_inshape <- sf::st_filter(pts_spread, db) |>
      dplyr::group_by(.tempid) |>
      dplyr::slice_head(n=1)

    phh_indb <- pts_inshape |>
      dplyr::select(-.tempid) |>
      sf::st_as_sf()

    phh_indb$phh_type <- PHH_TYPE_REGULAR

    # if we STILL don't get at least one valid PHH for some reason, return the
    # default point -- either centroid or approx geometric center
    if (nrow(phh_indb) == 0) {
      phh_indb <- create_default_response(region = db, region_idcol = region_idcol, roads_idcol = roads_idcol, type = PHH_TYPE_NO_VALID_PHHS_FOUND)
    }

  } # end if no valid points returned by push/pull algorithm

  # rename the geometry column for compatibility with the sf package
  if (!"geometry" %in% colnames(phh_indb)) phh_indb <- dplyr::rename(phh_indb, x = geometry)

  # ggplot() + geom_sf(data=db) +  geom_sf(data=roads_touching_region) + geom_sf(data=phh_indb)

  return (phh_indb)
}


# internal function for only giving warnings once when calling get_phhs_single()
# many times using purrr/furrr. This implementation works by setting a global
# option for each warning, and requires warning_cleanup() to be called afterwards
# to clear the options.
# For future optimization: look into doing this with environments instead
# Reference for setting options procedurally:
# https://stackoverflow.com/questions/67756783/how-do-you-add-an-environment-option-with-an-evaluated-name-in-r
# Parameter `warning_message` is a string with the warning to display.
# Parameter `track_warnings` is a boolean for whether to set the options or not.
warn_once <- function(warning_message, track_warnings = FALSE){

  if (track_warnings){
    # create a unique id using the warning message text to use as an option name
    warning_id <- gsub(x =  paste0(".phhs.",warning_message), pattern=" ", replacement = ".")

    # if this option does not exist, create it and set its value as the warning message
    if (is.null(getOption(warning_id))){
      do.call(options, as.list(stats::setNames(warning_message, warning_id)))

      # also issue a warning!
      warning(warning_message)
    }
  } else {
    # if we're not tracking warnings, just issue the warning
    warning(warning_message)
  }
}

# internal function to remove global options corresponding to warnings given
# https://stackoverflow.com/questions/53988871/how-can-i-completely-remove-a-global-option-from-r-with-options
warning_cleanup <- function() {
  # identify all package options which contain the unique string ".phhs."
  phh_options <-  grep(x = names(options()), pattern = ".phhs.", value = TRUE)

  # set them all to NULL
  option_names <- stats::setNames(rep(x = list(NULL), times = length(phh_options)), phh_options)

}


# Internal function to "thin out" the set of PHHs so that we have the minimum
# desired population per PHH. We take the number of input PHHS Ni, find the
# number oh PHHs we need Nn, and then take every ceiling(Ni/Nn) PHHs.
# Eg if minimum desired PHH pop is 5, the region pop is 10, and we have 10
# potential PHHs, reduce it so we only have 2 PHHs with pop 5 each by taking
# the 1st and 6th.
filter_phhs <- function(use_pops, region_pop, phh_inregion, min_phh_pop) {

  # if we're not using populations, return the phhs we have without change
  if (!use_pops) return (phh_inregion)

  # if the candidate PHHs would already get more than the desired minimum pop,
  # return them without change
  if (region_pop/nrow(phh_inregion) >= min_phh_pop) return(phh_inregion)

  # and if the minimum is impossible to meet, just take the first PHH
  num_needed <- floor(region_pop/min_phh_pop)
  if (num_needed < 1) return (phh_inregion[1,])

  # number of PHHs we start with
  num_orig <- nrow(phh_inregion)

  num_per_rep <- ceiling(num_orig/num_needed)

  num_reps <- ceiling(num_orig/num_per_rep) + 1

  filter_seq <- c(TRUE, rep(FALSE, times = (num_per_rep-1)))

  filter_index <- rep(filter_seq, times=(num_reps))[1:num_orig]

  phh_inregion_filtered <- phh_inregion[filter_index,]

  return (phh_inregion_filtered)
}

remove_clustered_phhs <- function(phh_inregion_filtered, min_phh_distance) {

  phh_testbuffer <- sf::st_buffer(phh_inregion_filtered, dist = min_phh_distance/2)

  # ggplot() + geom_sf(data=region) + geom_sf(data=phh_testbuffer)

  phh_intersections <- sf::st_intersects(phh_testbuffer) |>
    lapply(unlist)

  num_intersections <- unlist(lapply(phh_intersections, length))
  phhs_to_investigate <- which(num_intersections>1)

  phh_keepers_index <- rep(TRUE, times=length(num_intersections))

  # if more than one candidate phh, look through all candidate phhs
  if (length(phhs_to_investigate) > 1){
    for (i in phhs_to_investigate){
      # if this one has already been eliminated, skip it
      if (!phh_keepers_index[[i]])   next
      # keep this phh, eliminate any other phhs its the buffer zone
      phh_keepers_index[setdiff(phh_intersections[[i]], i)] <- FALSE
    } # end for
  } # end if

  # use our index to keep the keepers
  phh_keepers <- phh_inregion_filtered[phh_keepers_index,]

  return (phh_keepers)
}


#' Validate Pseudohouseholds (PHHs)
#'
#' This function runs two tests to ensure that PHHs meet minimal criteria for
#' validity: it checks to see whether PHH populations sum accurately to region
#' populations, and whether each populated region has at least one PHH. Results
#' are returned in a data frame, and any failing regions are returned in a list-
#' column that can be used for filtering and further analysis. Note that these
#' tests may fail if PHHs were generated without using population data.
#'
#' @param phhs A data frame containing a set of PHHs.
#' @param regions A simple feature object, sf tibble where each row is a region,
#'          used to generate the PHHs.
#' @param region_idcol Character, the name of the column in both `phhs` and `regions`
#'          containing regional identifiers.
#' @param region_popcol Character, the name of the column in both `phhs` and `regions`
#'          containing population data.
#'
#' @return A data frame containing test outputs.
#' @export
#'
#' @examples
#' phhs <- get_phhs_single(region = region_shp, region_idcol = "region_id",
#' region_popcol = "population", roads = road_shp, roads_idcol = "road_id")
#' validate_phhs(phhs = phhs, regions = region_shp, region_idcol = "region_id",
#' region_popcol = "population")
validate_phhs <- function(phhs, regions, region_idcol, region_popcol){

  # for clean R CMD CHECK with dplyr masking
  pop_diff <- pop_diff_raw <- phh_pop_sum <- NULL

  # rename columns for easy testing
  # PHHs
  phhs_fortest <- dplyr::select(phhs, dplyr::all_of(c(region_idcol, region_popcol))) |>
    sf::st_drop_geometry()

  phhs_fortest$region_idcol <- phhs_fortest[,region_idcol, drop=TRUE]
  phhs_fortest$region_popcol <- phhs_fortest[,region_popcol, drop=TRUE]
  phhs_fortest[, region_idcol] <- NULL
  phhs_fortest[, region_popcol] <- NULL

  # regions
  regions_fortest <- dplyr::select(regions, dplyr::all_of(c(region_idcol, region_popcol))) |>
    sf::st_drop_geometry()

  regions_fortest$region_idcol <- regions_fortest[,region_idcol, drop=TRUE]
  regions_fortest$region_popcol <- regions_fortest[,region_popcol, drop=TRUE]
  regions_fortest[, region_idcol] <- NULL
  regions_fortest[, region_popcol] <- NULL

  ## test population: do PHHs associated with regions have the same population as the full region?

  pop_test_result <- dplyr::tibble(test = "Population",
                                   description = "Do PHH populations sum to regional populations?")
  pop_test <- phhs_fortest |>
    dplyr::group_by(region_idcol) |>
    dplyr::summarise(phh_pop_sum = sum(region_popcol)) |>
    dplyr::inner_join(regions_fortest, by = "region_idcol") |>
    dplyr::mutate(pop_diff_raw = phh_pop_sum - region_popcol,
                  pop_diff = abs(pop_diff_raw) > .1)

  if (any(pop_test$pop_diff, na.rm = TRUE)) {
    bad_regions <- dplyr::filter(pop_test, pop_diff) |>
      dplyr::pull(region_idcol) |>
      unique()
    pop_test_result$result <- "Failed"
    pop_test_result$failing_regions <- list(bad_regions)
  } else {
    pop_test_result$result <- "Passed"
    pop_test_result$failing_regions <- list(NA)
  }


  ## test all populated input regions in output

  region_test_result <- dplyr::tibble(test = "Regions",
                                      description = "Do all populated regions have at least one PHH?")

  region_test <- regions_fortest |>
    dplyr::filter(!region_idcol %in% phhs_fortest$region_idcol ) |>
    dplyr::filter(region_popcol > 0)

  if (nrow(region_test) > 0) {
    bad_regions <- unique(region_test$region_idcol) #paste0(unique(region_test$region_idcol), collapse = ",")

    region_test_result$result <- "Failed"
    region_test_result$failing_regions <- list(bad_regions)
  } else {
    region_test_result$result <- "Passed"
    region_test_result$failing_regions <- list(NA)
  }

  # return result

  result <- dplyr::bind_rows(
    pop_test_result,
    region_test_result
  )

  return (result)
}



# internal function to return either the centroid (for a convex shape) or else
# the point inside the shape farthest from its boundaries. used for unpopulated
# regions, or regions with no roads, to still provide a single representative pt
get_center <- function(shape) {

  # calculate the centroid
  centroid <- sf::st_centroid(shape)

  # if the centroid is inside the shape, we are done: return the centroid
  if (sf::st_contains(shape, centroid, sparse = FALSE)) return (centroid)

  #ggplot() + geom_sf(data = centroid) + geom_sf(data = crescent)

  # create a hollow version of the shape for measuring distances to the borders
  shape_hollow <- sf::st_cast(shape, "MULTILINESTRING")

  #ggplot(crescent_hollow) + geom_sf()

  # sample points within the shape
  pts <- sf::st_sample(shape, 100)
  pts <- sf::st_sf(geometry = pts)

  #ggplot(pts) + geom_sf()

  # get distances from the sampled points to the borders
  distances <- sf::st_distance(shape_hollow, pts)[1,]

  # choose the shape with the biggest distance to the border
  biggest_dist_index <- which(distances == max(distances))
  pt <- pts[biggest_dist_index,]

  # add back the input shape's information
  pt <- dplyr::bind_cols(pt, sf::st_drop_geometry(shape))
  #ggplot() + geom_sf(data = crescent) + geom_sf(data = pts) + geom_sf(data = pt, colour="red")

  return (pt)
}



create_default_response <- function(region, region_idcol, roads_idcol, type) {

  result <- get_center(region)

  result$phh_id <- paste0(result[,region_idcol, drop=TRUE], ".1")

  if (!is.na(roads_idcol)) {
    result[,roads_idcol] <- NA
  }

  result$phh_type <- type

  return (result)
}

#create_default_response(region = region, region_idcol = "DBUID", roads_idcol = "NGD_UID", type = PHH_TYPE_NO_VALID_PHHS_FOUND)

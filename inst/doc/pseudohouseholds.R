## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

noaxislabels <- ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank())

## ----set----------------------------------------------------------------------
library(sf)
library(ggplot2)
library(ggspatial)
library(pseudohouseholds)

db <- dplyr::filter(pseudohouseholds::ottawa_db_shp, DBUID == "35061699003")

print(db)


## ----plot_db_map, echo = FALSE, message=FALSE, warning=FALSE, out.width="50%"----
ggplot2::ggplot(db)  + 
 # ggspatial::annotation_map_tile() + 
  ggplot2::geom_sf() + 
  ggspatial::annotation_scale() + noaxislabels


## ----plot_db_roads, echo = FALSE, message=FALSE, warning=FALSE----------------

 roads_touching_region <- sf::st_intersection(pseudohouseholds::ottawa_roads_shp, sf::st_buffer(db, 5)) |>
    sf::st_cast("MULTILINESTRING", warn = FALSE) |>
    sf::st_cast("LINESTRING", warn = FALSE) 


  ggplot() + geom_sf(data=db) + geom_sf(data=roads_touching_region)  + noaxislabels


## ----plot_db_pointsonroads, echo=FALSE, message=FALSE, warning=FALSE----------
  ## set candidate PHHs by sampling road segments at the density set by the user
  # cast to points
  phh_onstreet <- sf::st_line_sample(roads_touching_region, density = 0.005) |>
    sf::st_cast("POINT")

  sf::st_agr(roads_touching_region) = "constant"

    phh_onstreet <- sf::st_intersection(sf::st_buffer(roads_touching_region, 1), phh_onstreet)
    
    ggplot() + geom_sf(data=db) + geom_sf(data=phh_onstreet)  + noaxislabels


## ----plot_db_pointsbesideroads, echo=FALSE,message=FALSE,warning=FALSE--------
delta_distance <- 5
roads_idcol <- "NGD_UID"

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

  ggplot() + 
    geom_sf(data=db) +  
    geom_sf(data=roads_touching_region) + 
    geom_sf(data=phh_push, colour="red")+ 
    geom_sf(data=phh_pull, colour="blue") +
    geom_sf(data=db_centroid)  + noaxislabels



## ----plot_db_pointsinregion, echo=FALSE, message=FALSE, warning=FALSE---------
  # remove candidates that aren't inside the region
  phh_indb <- sf::st_filter(phh_pushpull, db)

  # rename the geometry column for compatibility with the sf package
  phh_indb <- dplyr::rename(phh_indb, x = geometry)

  ggplot() + geom_sf(data=db) +  geom_sf(data=roads_touching_region) + geom_sf(data=phh_indb)  + noaxislabels



#' Synthetic region shapefile for testing
#'
#' A small shapefile for testing.
#'
"region_shp"

#' Synthetic road shapefile for testing
#'
#' A small shapefile for testing.
#'
"road_shp"


#' 2021 Statistics Canada Road Network for Ottawa, Ontario
#'
#' Spatial dataset for road networks in Ottawa, Ontario, provided by Statistics
#' Canada, \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/rnf-frr/index2021-eng.cfm?year=21}.
#'
#' @format A data frame with class sf in CRS NAD/MTM zone 9 (32189) and 33,983 rows and 5 variables:
#' \describe{
#' \item{NGD_UID}{Unique road segment identifier}
#' \item{NAME}{Road segment name}
#' \item{RANK}{Road rank, lower numbers generally mean bigger/faster roads,
#'    \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=RANK}}
#' \item{CLASS}{Road class, lower numbers generally but do not always mean bigger/faster roads,
#'    \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/ref/domain-domaine/index2021-eng.cfm?lang=e&id=CLASS}}
#' \item{geometry}{LINESTRING defining road segment geometry}
#' }
#'
#' This data is licensed under the Statistics Canada Open Data License
#' (\url{https://www.statcan.gc.ca/en/reference/licence}).
#' Adapted from Statistics Canada, 2021 Census Road Network File, 2022-09-28.
#' This does not constitute an endorsement by Statistics Canada of this product.
"ottawa_roads_shp"


#' 2021 Statistics Canada Dissemination Block Boundaries and Populations for Ottawa, Ontario
#'
#' Spatial dataset for dissemination blocks (DBs) in Ottawa, Ontario, provided by
#' Statistics Canada, \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21}.
#'
#' Dissemination blocks are the smallest unit of geography at which Statistics
#' Canada publishes population data. DBs are generally bounded by road segments
#' or natural features like waterways. In urban areas DBs are generally the size
#' of a city block, but in rural areas they can be much larger.
#'
#' @format A data frame with class sf in CRS NAD/MTM zone 9 (32189) and 8,559 rows and 3 variables:
#' \describe{
#'    \item{DBUID}{Unique dissemination block identifier}
#'    \item{dbpop2021}{Dissemination block population as given in the 2021
#'        Statistics Canada geographic attribute file,
#'      \url{https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/index2021-eng.cfm}}
#'    \item{geometry}{MULTIPOLYGON defining DB geometry}
#' }
#'
#' This data is licensed under the Statistics Canada Open Data License
#' (\url{https://www.statcan.gc.ca/en/reference/licence}).
#' Adapted from Statistics Canada, 2021 Dissemination Block Boundary File,
#' 2022-09-19, and Statistics Canada, 2021 Census – Geographic Attribute File,
#' 2022-02-10.
#' This does not constitute an endorsement by Statistics Canada of this product.
"ottawa_db_shp"

library(sf)

make_lsoa_boundary_file <- function(radius = 500) {

  # read in 2011 lsoa pop weighted centroids
  lsoa_centroids <- st_read("data/lsoa/LSOA_(Dec_2011)_Population_Weighted_Centroids_in_England_and_Wales/LSOA_Dec_2011_PWC_in_England_and_Wales.shp")
  # buffer centroids to 500m radius
  lsoa_centroids_500m <- st_buffer(x = lsoa_centroids,
                                   dist = radius,
                                   endCapStyle = "ROUND")

  # keep only key id and geom fields, and calculate area of 500m buffer
  lsoa_centroids_500m <- lsoa_centroids_500m %>%
    select(lsoa11cd,
           lsoa11nm,
           geometry) %>%
    mutate(lsoa_500m_area = st_area(geometry))

  # check by mapping these...
  # qtm(lsoa_centroids_500m)

  # remove geom (i.e. convert into non-sf object/data.frame only)
  lsoa_centroid_areas <- st_drop_geometry(lsoa_centroids_500m)

  # read in 2011 lsoa boundary shape file
  lsoa_boundaries <- st_read("../gis-data/boundaries/lsoa/LSOAs_Dec_2011_BFC_EW_V3/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Full_Clipped_(BFC)_EW_V3.shp")
  # sort out col names to align with centroid data
  colnames(lsoa_boundaries) <- tolower(names(lsoa_boundaries))
  # keep only id and geom fields, and calc full area of lsoa
  lsoa_boundaries <- lsoa_boundaries %>%
    select(lsoa11cd,
           lsoa11nm,
           geometry) %>%
    mutate(lsoa_full_area = st_area(geometry))

  # remove geom from lsoa boundaries (i.e. convert into non-sf object/data.frame only)
  lsoa_boundary_areas <- st_drop_geometry(lsoa_boundaries)

  # join centroid 500m buffer area and full lsoa area dfs together
  lsoa_areas <- inner_join(lsoa_centroid_areas, lsoa_boundary_areas, by = c("lsoa11cd", "lsoa11nm"))
  # determine which is the largest of the two.
  lsoa_areas <- lsoa_areas %>%
    mutate(largest_area = ifelse(lsoa_500m_area > lsoa_full_area, "centroid_area", "full_area"))

  # split the table based on this field
  lsoa_split <- split(lsoa_areas, lsoa_areas$largest_area)

  # list lsoa codes in each file
  centroid_lsoacodes <- lsoa_split$centroid_area$lsoa11cd
  fullarea_lsoacodes <- lsoa_split$full_area$lsoa11cd

  # then filter the sf objects to keep only those we want to use from each file
  lsoa_boundaries_keep <- lsoa_boundaries %>%
    filter(lsoa11cd %in% fullarea_lsoacodes)
  lsoa_centroids_500m_keep <- lsoa_centroids_500m %>%
    filter(lsoa11cd %in% centroid_lsoacodes)

  # bind together to get a complete set of lsoas
  lsoa_transport_area_boundaries <- bind_rows(lsoa_boundaries_keep,
                                              lsoa_centroids_500m_keep)


}

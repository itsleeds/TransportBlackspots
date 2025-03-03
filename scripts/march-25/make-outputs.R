make_tph_quintiles <- function(bus_lsoa) {

  bus_lsoa_long <- make_bustrips_long(bus_lsoa)

  bus_lsoa_long <- bus_lsoa_long %>%
    group_by(rurality) %>%
    mutate(tph_qt = ntile(desc(tph), n = 5))

  bus_lsoa_long$tph_qt_id <- LETTERS[bus_lsoa_long$tph_qt]

  return(bus_lsoa_long)

}

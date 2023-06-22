bespoke.la.combination <- function(data, old.la.codes, new.la.code, new.la.name) {

  data <- data %>%
    mutate(oslaua = case_when(oslaua %in% old.la.codes ~ new.la.code,
                              TRUE ~ oslaua)) %>%
    mutate(local_authority_name = case_when(oslaua == new.la.code ~ new.la.name,
                                            TRUE ~ local_authority_name))

  # data <- data %>%
  #   group_by(oslaua,
  #            local_authority_name) %>%
  #   summarise(across(where(is.numeric), sum)) %>%
  #   ungroup()

}


#' ----------------------------------------------------------------------------

# April 2023 changes:
# https://blog.planningportal.co.uk/2023/03/24/local-authority-changes-from-1-april-2023/
#   In Cumbria:
#       Cumberland – from: Allerdale, Carlisle and Copeland
#       Westmorland and Furness – from: Barrow-in-Furness, Eden and South Lakeland
#
#   In North Yorkshire:
#       North Yorkshire - from: Ryedale, Scarborough, Selby, Craven, Hambleton, Harrogate and Richmondshire
#
#   In somerset:
#       [One] Somerset - from: Mendip, Sedgemoor, Somerset West and Taunton and South Somerset

cumberland.consolidation <- function(data) {

  #cumberland_old <- c("Allerdale", "Carlisle", "Copeland")
  #filter(oslaua.list, local_authority_name %in% cumberland_old)
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000028", "E07000026", "E07000029"),
                                 new.la.code = "E06000063",
                                 new.la.name = "Cumberland")

}

westmorland.consolidation <- function(data) {

  #westmorland_old <- c("Barrow-in-Furness", "Eden", "South Lakeland")
  #filter(oslaua.list, local_authority_name %in% westmorland_old)
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000030", "E07000031", "E07000027"),
                                 new.la.code = "E06000064",
                                 new.la.name = "Westmorland and Furness")

}


northyorks.consolidation <- function(data) {
  #ny_old <- c("Ryedale", "Scarborough", "Selby", "Craven", "Hambleton", "Harrogate", "Richmondshire", "North Yorkshire")
  #filter(oslaua.list, local_authority_name %in% ny_old)
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000163", "E07000165", "E07000166", "E07000164", "E07000167", "E07000169", "E07000168"),
                                 new.la.code = "E06000065",
                                 new.la.name = "North Yorkshire")

}

somerset.consolidation <- function(data) {
  #somerset_old <- c("Mendip", "Sedgemoor", "Taunton Deane", "West Somerset", "South Somerset")
  #filter(oslaua.list, local_authority_name %in% somerset_old)
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000189", "E07000187",  "E07000188", "E07000190", "E07000191"),
                                 new.la.code = "E06000066",
                                 new.la.name = "Somerset")
}

# Bournemouth, Christchurch and Poole
bournemouth.consolidation <- function(data) {
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E06000028", "E07000048", "E06000029"),
                                 new.la.code = "E06000058",
                                 new.la.name = "Bournemouth, Christchurch and Poole")
}

# Dorset
# E07000049 - East Dorset
# E07000050 - North Dorset
# E07000051 - Purbeck
# E07000052 - West Dorset
# E07000053 - Weymouth and Portland

dorset.consolidation <- function(data) {
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000049", "E07000050", "E07000051", "E07000052", "E07000053"),
                                 new.la.code = "E06000059",
                                 new.la.name = "Dorset")

}

# West Suffolk
# E07000201 - Forest Heath
# E07000204 - St Edmundsbury
westsuffolk.consolidation <- function(data) {
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000201", "E07000204"),
                                 new.la.code = "E07000245",
                                 new.la.name = "West Suffolk")
}

# East Suffolk
# E07000205 - Suffolk Coastal
# E07000206 - Waveney
eastsuffolk.consolidation <- function(data) {
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000205", "E07000206"),
                                 new.la.code = "E07000244",
                                 new.la.name = "East Suffolk")
}

buckinghamshire.combination <- function(data) {

  # combined from: "Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"
  # E07000004 - Aylesbury Vale
  # E07000005 - Chiltern
  # E07000006 - South Bucks
  # E07000007 - Wycombe

  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000004", "E07000005", "E07000006", "E07000007"),
                                 new.la.code = "E06000060",
                                 new.la.name = "Buckinghamshire")

}

northnorthamptonshire.combination <- function(data) {

  # combined from: Corby, East Northamptonshire, Kettering, and Wellingborough
  #old.nnorthants.names <- c("Corby", "East Northamptonshire", "Kettering", "Wellingborough")
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000150", "E07000152", "E07000153", "E07000156"),
                                 new.la.code = "E06000061",
                                 new.la.name = "North Northamptonshire")

}

westnorthamptonshire.combination <- function(data) {

  # combined from: Daventry, Northampton, South Northamptonshire
  # E07000151 Daventry
  # E07000154 Northampton
  # E07000155 South Northamptonshire
  data <- bespoke.la.combination(data,
                                 old.la.codes = c("E07000151", "E07000154", "E07000155"),
                                 new.la.code = "E06000062",
                                 new.la.name = "West Northamptonshire")

}


# combine all april 2023 changes in one function to use across the data.
# it will need to be added before percentages are recalculated...
la_consolidation_april_2023 <- function(data) {

  data <- cumberland.consolidation(data)
  data <- westmorland.consolidation(data)
  data <- northyorks.consolidation(data)
  data <- somerset.consolidation(data)
  data <- bournemouth.consolidation(data)
  data <- dorset.consolidation(data)
  data <- westsuffolk.consolidation(data)
  data <- eastsuffolk.consolidation(data)
  data <- buckinghamshire.combination(data)
  data <- northnorthamptonshire.combination(data)
  data <- westnorthamptonshire.combination(data)

  data <- data %>%
    mutate(local_authority_name = ifelse(local_authority_name == "Shepway",
                                         "Folkestone and Hythe",
                                         local_authority_name))

  data <- data %>%
    mutate(local_authority_name = ifelse(local_authority_name == "Cornwall/Isles of Scilly",
                                         "Cornwall",
                                         local_authority_name))
  data <- data %>%
    mutate(oslaua = ifelse(oslaua == "E06000052/E06000053",
                           "E06000052",
                           oslaua))

}

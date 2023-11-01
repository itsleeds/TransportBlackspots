get_pcon23_implied_results <- function() {

  # read in data from Electoral Calculus website
  headline_results_2023 <- read.csv("../pcon-profiles/data/political/pcon23-implied-results.csv")
  implied_results_2023_old <- read.csv("../pcon-profiles/data/political/ElectoralCalculus_Implied2019.csv")

  headline_results_2023 <- left_join(headline_results_2023, implied_results_2023_old, by = c("constituency23_name" = "Seat"))

    # make processing functions
  comma_format_to_num <- function(x) as.numeric(gsub(",", "", x))
  percent_format_to_pct <- function(x) as.numeric(gsub("[%]", "", x))/ 100

  # process and clean data, and calculate implied swing needed
  headline_results_2023 <- headline_results_2023 %>%
    mutate(implied_turnout = percent_format_to_pct(Turnout),
           electorate = comma_format_to_num(Electorate),
           implied_majority_2019 = comma_format_to_num(Implied.Majority.2019)) %>%
    mutate(implied_total_votes = round(electorate * implied_turnout)) %>%
    mutate(implied_swing_needed = implied_majority_2019 / implied_total_votes / 2)

  # select final results
  headline_results_2023 <- headline_results_2023 %>%
    transmute(constituency23_name,
              implied_mp_name = mp_name,
              #implied_mp_name = Implied.MP.at.2019,
              implied_first_party = first_party,
              implied_majority_2019,
              implied_total_votes,
              implied_swing_needed)

  # need to update names for join
  headline_results_2023 <- headline_results_2023 %>%
    mutate(constituency23_name = case_when(constituency23_name == "Ashton under Lyne" ~ "Ashton-under-Lyne",
                                           constituency23_name == "Basildon South and East Thurrock" ~ "South Basildon and East Thurrock",
                                           constituency23_name == "Bedfordshire Mid" ~ "Mid Bedfordshire",
                                           constituency23_name == "Bedfordshire North" ~ "North Bedfordshire",
                                           constituency23_name == "Berkshire Mid" ~ "Mid Berkshire",
                                           constituency23_name == "Bridlington and the Wolds" ~ "Bridlington and The Wolds",
                                           constituency23_name == "Cambridgeshire East" ~ "East Cambridgeshire",
                                           constituency23_name == "Cambridgeshire North East" ~ "North East Cambridgeshire",
                                           constituency23_name == "Cambridgeshire North West" ~ "North West Cambridgeshire",
                                           constituency23_name == "Cambridgeshire South" ~ "South Cambridgeshire",
                                           constituency23_name == "Cornwall North" ~ "North Cornwall",
                                           constituency23_name == "Cornwall South East" ~ "South East Cornwall",
                                           constituency23_name == "Cotswolds South" ~ "South Cotswolds",
                                           constituency23_name == "Derbyshire Mid" ~ "Mid Derbyshire",
                                           constituency23_name == "Derbyshire North East" ~ "North East Derbyshire",
                                           constituency23_name == "Derbyshire South" ~ "South Derbyshire",
                                           constituency23_name == "Devon Central" ~ "Central Devon",
                                           constituency23_name == "Devon North" ~ "North Devon",
                                           constituency23_name == "Devon South" ~ "South Devon",
                                           constituency23_name == "Devon South West" ~ "South West Devon",
                                           constituency23_name == "Dorset Mid and Poole North" ~ "Mid Dorset and North Poole",
                                           constituency23_name == "Dorset North" ~ "North Dorset",
                                           constituency23_name == "Dorset South" ~ "South Dorset",
                                           constituency23_name == "Dorset West" ~ "West Dorset",
                                           constituency23_name == "Durham North" ~ "North Durham",
                                           constituency23_name == "Durham, City of" ~ "City of Durham",
                                           constituency23_name == "Faversham and Kent Mid" ~ "Faversham and Mid Kent",
                                           constituency23_name == "Hampshire East" ~ "East Hampshire",
                                           constituency23_name == "Hampshire North East" ~ "North East Hampshire",
                                           constituency23_name == "Hampshire North West" ~ "North West Hampshire",
                                           constituency23_name == "Herefordshire North" ~ "North Herefordshire",
                                           constituency23_name == "Hertfordshire North East" ~ "North East Hertfordshire",
                                           constituency23_name == "Hertfordshire South West" ~ "South West Hertfordshire",
                                           constituency23_name == "Hull East" ~ "Kingston upon Hull East",
                                           constituency23_name == "Hull North" ~ "Kingston upon Hull North",
                                           constituency23_name == "Hull West and Hessle" ~ "Kingston upon Hull West and Hessle",
                                           constituency23_name == "Lancashire West" ~ "West Lancashire",
                                           constituency23_name == "Leicestershire Mid" ~ "Mid Leicestershire",
                                           constituency23_name == "Leicestershire North West" ~ "North West Leicestershire",
                                           constituency23_name == "Leicestershire South" ~ "South Leicestershire",
                                           constituency23_name == "Middlesbrough South and Cleveland East" ~ "Middlesbrough South and East Cleveland",
                                           constituency23_name == "Norfolk Mid" ~ "Mid Norfolk",
                                           constituency23_name == "Norfolk North" ~ "North Norfolk",
                                           constituency23_name == "Norfolk North West" ~ "North West Norfolk",
                                           constituency23_name == "Norfolk South" ~ "South Norfolk",
                                           constituency23_name == "Norfolk South West" ~ "South West Norfolk",
                                           constituency23_name == "Northamptonshire South" ~ "South Northamptonshire",
                                           constituency23_name == "Ribble South" ~ "South Ribble",
                                           constituency23_name == "Shropshire North" ~ "North Shropshire",
                                           constituency23_name == "Shropshire South" ~ "South Shropshire",
                                           constituency23_name == "Somerset North" ~ "North Somerset",
                                           constituency23_name == "Somerset North East and Hanham" ~ "North East Somerset and Hanham",
                                           constituency23_name == "Suffolk Central and Ipswich North" ~ "Central Suffolk and North Ipswich",
                                           constituency23_name == "Suffolk South" ~ "South Suffolk",
                                           constituency23_name == "Suffolk West" ~ "West Suffolk",
                                           constituency23_name == "Surrey East" ~ "East Surrey",
                                           constituency23_name == "Sussex Mid" ~ "Mid Sussex",
                                           constituency23_name == "Thanet East" ~ "East Thanet",
                                           constituency23_name == "Wiltshire East" ~ "East Wiltshire",
                                           constituency23_name == "Wiltshire South West" ~ "South West Wiltshire",
                                           constituency23_name == "Worcestershire West" ~ "West Worcestershire",
                                           constituency23_name == "Worthing East and Shoreham" ~ "East Worthing and Shoreham",
                                           constituency23_name == "Wrekin, The" ~ "The Wrekin",
                                           constituency23_name == "Pembrokeshire Mid and South" ~ "Mid and South Pembrokeshire",
                                           constituency23_name == "Carmarthen" ~ "Caerfyrddin",

                                           #constituency23_name == "" ~ "",
                                           constituency23_name == "Buckinghamshire Mid" ~ "Mid Buckinghamshire",
                                           constituency23_name == "Cheshire Mid" ~ "Mid Cheshire",
                                           constituency23_name == "Cotswolds North" ~ "North Cotswolds",
                                           constituency23_name == "Northumberland North" ~ "North Northumberland",
                                           constituency23_name == "Warwickshire North and Bedworth" ~ "North Warwickshire and Bedworth",
                                           constituency23_name == "Essex North West" ~ "North West Essex",
                                           constituency23_name == "Ynys Mon (Anglesey)" ~ "Ynys Mon",

                                           TRUE ~ constituency23_name))

  # get new boundary details
  pcon_boundary_change <- get_boundary_change_details_2023()

  headline_results_2023 %>%
    filter(grepl("Yn", constituency23_name, ignore.case = TRUE))%>%
    select(constituency23_name)

  # join implied results and pcon boundary changes data
  pcon_implied_changes <- left_join(headline_results_2023, pcon_boundary_change, by = c("constituency23_name" = "constituency_name_successor"))

  # clean to keep main old constituency
  pcon_implied_changes <- pcon_implied_changes %>%
    group_by(constituency23_name) %>%
    mutate(pop_rank = rank(desc(successor_proportion_pop))) %>%
    ungroup() %>%
    filter(pop_rank == 1)

  # take account of by-election changes...
  byelections <- get_byelection_results()
  pcon_implied_changes <- left_join(pcon_implied_changes, byelections, by = c("pcon", "constituency_name_current"))

  pcon_implied_changes <- pcon_implied_changes %>%
    mutate(byelection = ifelse(is.na(byelection), FALSE, byelection)) %>%
    mutate(implied_first_party = ifelse(byelection, byelection_first_party, implied_first_party),
           implied_mp_name = ifelse(byelection, byelection_mp_name, implied_mp_name),
           implied_majority_2019 = ifelse(byelection, byelection_majority, implied_majority_2019))

  pcon_implied_changes <- pcon_implied_changes %>%
    select(constituency23_name,
           implied_mp_name,
           implied_first_party,
           implied_majority_2019,
           implied_swing_needed)

}



get_boundary_change_details_2023 <- function() {

  getSheetNames("../pcon-profiles/data/political/HoC-Revised-proposals-analysis.xlsx")

  pcon_changes_2023_all <- read.xlsx("../pcon-profiles/data/political/HoC-Revised-proposals-analysis.xlsx",
                                     sheet = "All segments",
                                     startRow = 4)

  pcon_changes_2023 <- read.xlsx("../pcon-profiles/data/political/HoC-Revised-proposals-analysis.xlsx",
                                 sheet = "Closest successor only",
                                 startRow = 4)

  pcon_changes_2023 <- pcon_changes_2023 %>%
    select(pcon = Current.constituency.code,
           constituency_name_current = Current.constituency.name,
           constituency_name_successor = Closest.successor.constituency.name,
           similarity_current_new = Similarity.between.current.constituency.and.successor,
           successor_proportion_pop = POP.How.much.of.successor.is.made.up.of.current.constituency)

  # get estimated changes for wales (FoE estimate based on postcode distribution)
  pcon_changes_wales_2023 <- get_wales_boundary_changes()

  # join to England
  pcon_changes_2023 <- bind_rows(pcon_changes_2023, pcon_changes_wales_2023)

}


get_wales_boundary_changes <- function() {

  pcon_changes_wales_2023 <- read.csv("../pcon-profiles/data/political/pcon-successor-lookup-wales.csv",
                                      stringsAsFactors = FALSE)

  pcon_changes_wales_2023 <- pcon_changes_wales_2023 %>%
    select(pcon = PCON21CD,
           constituency_name_current = PCON21NM,
           constituency_name_successor = constituency_successor_name_pc_main,
           successor_proportion_pop = pc_pct_similarity) %>%
    mutate(similarity_current_new = case_when(successor_proportion_pop <= 0.40 ~ "Under 40%",
                                              successor_proportion_pop <= 0.50 ~ "40% - 50%",
                                              successor_proportion_pop <= 0.60 ~ "50% - 60%",
                                              successor_proportion_pop <= 0.70 ~ "60% - 70%",
                                              successor_proportion_pop <= 0.80 ~ "70% - 80%",
                                              successor_proportion_pop <= 0.90 ~ "80% - 90%",
                                              successor_proportion_pop <= 0.95 ~ "90% - 95%",
                                              successor_proportion_pop <= 0.99 ~ "95% - 99%",
                                              successor_proportion_pop <= 0.999999 ~ "Over 99% of population",
                                              successor_proportion_pop == 1 ~ "Unchanged"))

}


get_byelection_results <- function() {

  # by elections data-file from here: "https://commonslibrary.parliament.uk/research-briefings/cbp-9225/"
  by_elections_datafile_path = "../pcon-profiles/data/political/By-elections-2019-Parliament-Aug2023.xlsx"

  byelections_summary <- read.xlsx(by_elections_datafile_path,
                                   sheet = "Summary",
                                   startRow = 2,
                                   detectDates = TRUE)

  byelections_summary <- byelections_summary %>%
    transmute(pcon = ONS_id,
              #ons_region_id,
              constituency_name = Constituency.name,
              #county_name,
              #region_name,
              #country_name,
              #constituency_type,
              #byelection_date = `Date.of.By-election`,
              #mp_firstname,
              #mp_surname,
              #mp_gender,
              #result = paste(Winning.Party, `Gain/Hold`, sep = " "),
              first_party = Winning.Party,
              #second_party,
              #electorate = Electorate,
              #valid_votes = Total.valid.votes,
              #invalid_votes = Invalid.Votes,
              majority = `Majority.(votes)`)

  byelections_candidates <- read.xlsx(by_elections_datafile_path,
                                      sheet = "Candidates",
                                      startRow = 2,
                                      detectDates = TRUE)

  # correct lower case incident in names
  byelections_candidates <- byelections_candidates %>%
    mutate(Constituency = ifelse(Constituency == "Old bexley and Sidcup", "Old Bexley and Sidcup", Constituency))

  # simplify main party names
  byelections_candidates <- byelections_candidates %>%
    mutate(party_simple = case_when(Party == "Conservative" ~ "CON",
                                    Party == "Green" ~ "Green",
                                    Party == "SNP" ~ "NAT",
                                    Party == "Liberal Democrats" ~ "LIB",
                                    Party == "Liberal Democrat" ~ "LIB",
                                    Party == "Labour" ~ "LAB",
                                    TRUE ~ "Other"))

  # spread the party vote results ready for a wide table join
  byelections_allparty_results <- byelections_candidates %>%
    group_by(Constituency,
             party_simple) %>%
    summarise(votes = sum(Votes_by, na.rm = TRUE)) %>%
    ungroup() %>%
    spread(key = party_simple,
           value = votes,
           fill = 0) %>%
    rename(constituency_name = Constituency)

  # add a rank to pull out winners and second place party
  byelections_candidates <- byelections_candidates %>%
    group_by(Constituency) %>%
    mutate(rank = rank(desc(Votes_by))) %>%
    ungroup()

  # get winner names
  byelections_winners <- byelections_candidates %>%
    filter(rank == 1) %>%
    separate(col = Full_name, into = c("mp_firstname", "mp_surname"), sep = " ") %>%
    select(constituency_name = Constituency,
           mp_firstname,
           mp_surname)

  # # get second place party
  # byelections_second <- byelections_candidates %>%
  #   filter(rank == 2) %>%
  #   transmute(constituency_name = Constituency,
  #             second_party = party_simple)

  # join all data sets
  byelections <- Reduce(function(x, y) left_join(x, y, by = "constituency_name"),
                        list(byelections_summary,
                             byelections_winners))

  byelections <- byelections %>%
    mutate(first_party = case_when(first_party == "Labour" ~ "LAB",
                                   first_party == "Liberal Democrat" ~ "LIB",
                                   first_party == "Conservative" ~ "CON",
                                   TRUE ~ first_party))

  byelections <- byelections %>%
    mutate(mp_firstname = case_when(constituency_name == "Uxbridge and South Ruislip" ~ "Steve",
                                    constituency_name == "Selby and Ainsty" ~ "Kier",
                                    constituency_name == "Somerton and Frome" ~ "Sarah",
                                    TRUE ~ mp_firstname),
           mp_surname = case_when(constituency_name == "Uxbridge and South Ruislip" ~ "Tuckwell",
                                  constituency_name == "Selby and Ainsty" ~ "Mather",
                                  constituency_name == "Somerton and Frome" ~ "Dyke",
                                  TRUE ~ mp_surname)) %>%
    unite(byelection_mp_name, mp_firstname, mp_surname, sep = " ") %>%
    rename(constituency_name_current = constituency_name,
           byelection_first_party = first_party,
           byelection_majority = majority) %>%
    mutate(byelection = TRUE)

}

pcon23_impacts <- get_pcon23_implied_results()
write.csv(pcon23_impacts,
          "data/pcon23-implied-results.csv",
          row.names = FALSE,
          na="")

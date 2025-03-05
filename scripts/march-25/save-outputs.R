source("../environmental-data-for-change/scripts/save-foe-workbook.R")

# save main article tables in one file
save_article_main_tables <- function(t1, t2, t3a, t3b, t4, t5, g1, h1) {

  save_as_spreadsheet_multiformat(number_of_tabs = 8,
                                  tab1_data = t1,
                                  tab2_data = t2,
                                  tab3_data = t3a,
                                  tab4_data = t3b,
                                  tab5_data = t4,
                                  tab6_data = t5,
                                  tab7_data = g1,
                                  tab8_data = h1,
                                  tab1_name = "Table 1",
                                  tab2_name = "Table 2",
                                  tab3_name = "Table 3 v1",
                                  tab4_name = "Table 3 v2",
                                  tab5_name = "Table 4",
                                  tab6_name = "Table 5",
                                  tab7_name = "Graph 1",
                                  tab8_name = "Summary trends",
                                  number_cols_1 = 0,
                                  percent_cols_1 = c(3:8),
                                  number_cols_2 = c(3:5),
                                  percent_cols_2 = 6,
                                  number_cols_3 = 0,
                                  percent_cols_3 = 0,
                                  number_cols_4 = c(2:6),
                                  percent_cols_4 = 0,
                                  number_cols_5 = c(2,4,6,8,10,12),
                                  percent_cols_5 = c(3,5,7,9,11,13),
                                  number_cols_6 = c(4,7),
                                  percent_cols_6 = c(3,6),
                                  number_cols_7 = c(4:5),
                                  percent_cols_7 = 0,
                                  number_cols_8 = c(5:7),
                                  percent_cols_8 = 8,
                                  xlsx_path = "outputs/march-25/bus-article-tables.xlsx",
                                  #alternative_xlsx_path = "",
                                  percent_decimal = TRUE,
                                  number_decimal = FALSE)

  }




# save geographical summarise of no service lsoas
save_no_service_geogs <- function(t4_region, t4_las, t4_pcon24) {

  t4_nums <- c(3,5,7,9,11,13)
  t4_pcts <- c(4,6,8,10,12,14)

  save_as_spreadsheet_multiformat(number_of_tabs = 3,
                                  tab1_data = t4_region,
                                  tab2_data = t4_las,
                                  tab3_data = t4_pcon24,
                                  tab1_name = "Table 4 - Region",
                                  tab2_name = "Table 4 - LA",
                                  tab3_name = "Table 4 - Const",
                                  number_cols_1 = t4_nums,
                                  percent_cols_1 = t4_pcts,
                                  number_cols_2 = t4_nums,
                                  percent_cols_2 = t4_pcts,
                                  number_cols_3 = t4_nums,
                                  percent_cols_3 = t4_pcts,
                                  xlsx_path = "outputs/march-25/bus-no-service-by-geog.xlsx",
                                  alternative_xlsx_path = "../../OneDrive - Friends of the Earth/Documents - Environmental Data for Change/Data/FoE Analysis/transport/march-25/buses-no-service-lsoas-by-geog.xlsx",
                                  percent_decimal = TRUE,
                                  number_decimal = FALSE)

}


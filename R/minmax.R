library(magrittr)

test_df <- AWQMSdata::AWQMS_Data(station = c("14046778","14034470" ), stat_base = c('Minimum', 'Maximum'), char = "Temperature, water",
                                 startdate = '2012-6-1', enddate = '2012-10-1', crit_codes = TRUE)



test_df_crit <- test_df %>%
  dplyr::left_join(AWQMSdata::Temp_crit, by = c('FishCode' = 'FishUse_code')) %>%
  dplyr::left_join(AWQMSdata::LU_spawn, by = c('SpawnCode' = 'SpawnCode'))

upstream = '14034470'
temp_crit = dplyr::first(test_df_crit$Temp_Criteria)

minmax <- test_df %>%
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::mutate(name = ifelse(MLocID == upstream, "Upstream", "Downstream" )) %>%
  dplyr::select(OrganizationID, Org_Name,  HUC8, HUC8_Name, AU_ID, MLocID, name,
                Lat_DD, Long_DD, SampleStartDate,Result_Operator,
                Result_Numeric, Result_Unit, Statistical_Base
                ) %>%
  tidyr::spread(key = Statistical_Base, value = Result_Numeric)


spawnstart = lubridate::ymd("2012-07-01")
spawnend = lubridate::ymd("2012-09-01")


plot_minmax_compare(df = minmax, date_col = "SampleStartDate" , min_col = "Minimum", max_col = "Maximum",
                    station_name_col = "name",  grey_station = "Downstream", red_station = "Upstream",
                    spawnstart = spawnstart, spawnend = spawnend, crit = temp_crit, spawn_crit = 11)







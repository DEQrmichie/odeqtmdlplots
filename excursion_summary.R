library(AWQMSdata)
library(lubridate)
library(dplyr)
library(odeqtmdlplots)
library(odeqmloctools)
library(writexl)

df.raw <- AWQMSdata::AWQMS_Data(HUC8 = "17080001",
                                startdate = "2012-01-01", enddate = "2021-12-31",
                                char = "Temperature, water",
                                stat_base = "7DADM",
                                filterQC = TRUE,
                                crit_codes = TRUE)

df.mloc <- AWQMSdata::query_stations(mlocs = unique(df.raw$MLocID))

save.image("data_dev/excursion_summary.RData")
# load("data_dev/excursion_summary.RData")

# There are a couple of stations without AUs listed so need to get them.
# Get them via join with the nhd table from odeqmloctools using Permanent_Identifier/
# unfortunately there are two stations without the Permanent_Identifier.
# Get those using odeqmloctools::launch_map.

# convert column names into template format so they can be used in launch map.
df.mloc1 <- df.mloc %>%
  dplyr::select(MLocID, StationDes, MonLocType, Lat_DD, Long_DD, Datum, ELEV_Ft,
                Reachcode, Measure, Permanent_Identifier, TribalLand, AltLocID)

mloc.names <- names(df.mloc1) %>%
   lapply(odeqmloctools::col_lookup, from = "stations", to = "template") %>%
   unlist()

mloc.names <- ifelse(is.na(mloc.names), names(df.mloc1), mloc.names)

names(df.mloc1) <- mloc.names

# Look at the stations in the map and get the NHD Permanent_Identifier
df.mloc2 <- odeqmloctools::launch_map(mloc = df.mloc1)

# Isolate the needed columns for the join
df.mloc3 <- df.mloc2 %>%
  dplyr::select(MLocID = Monitoring.Location.ID,
                Permanent_Identifier = Permanent.Identifier,
                Datum = Horizontal.Datum, ELEV_Ft) %>%
  dplyr::left_join(odeqmloctools::ornhd) %>%
  dplyr::select(MLocID, Datum, ELEV_Ft, AU_ID, AU_Name)

# criteria joins
df2 <- df.raw %>%
  mutate(FishCode_chr = as.character(FishCode)) %>%
  dplyr::left_join(AWQMSdata::Temp_crit) %>%
  dplyr::left_join(AWQMSdata::LU_FishUse, by = c("FishCode_chr"="FishCode")) %>%
  dplyr::left_join(AWQMSdata::LU_spawn)

# Add in some missing data needed for data cleaning
df3 <- df2 %>%
  dplyr::mutate(Result_Text = as.character(Result_Numeric),
                DQL = dplyr::if_else(is.na(DQL), "E", DQL))


df4 <- odeqtmdlplots::clean_data(df3)

# mloc join and excursion summary
df5 <- df4 %>%
  dplyr::filter(Statistical_Base == "7DADM") %>%
  dplyr::select(-AU_ID) %>%
  dplyr::left_join(df.mloc3) %>%
  dplyr::mutate(excursion = dplyr::if_else(Result_Numeric > Temp_Criteria, 1, 0 )) %>%
  dplyr::group_by(AU_ID, AU_Name, Temp_Criteria) %>%
  dplyr::summarise(monitoring_locations = paste(sort(unique(MLocID)), collapse = ", "),
                   stat = first(Statistical_Base),
                   num_excursions = sum(excursion),
                   num_results = dplyr::n(),
                   max_temp = max(Result_Numeric)) %>%
  dplyr::mutate(Temp_Criteria = Temp_Criteria + 0.3,
                excess_temp = max_temp - Temp_Criteria,
                excess_temp = dplyr::if_else(excess_temp > 0, excess_temp, 0),
                percent_reduction = excess_temp / max_temp * 100)


writexl::write_xlsx(df5, path = "Sandy_Subbsin_temperature_excursion_summary.xlsx")



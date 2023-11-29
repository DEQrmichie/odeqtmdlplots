#' plot_seasonality_box
#'
#' This function produces a Tukey style box plot of seven day average daily maximum river
#' temperatures grouped by the first and second half of each month for one or more monitoring locations.
#' The month is split on the 15th with the first group including all results measured on the
#' 1st through the 14th day and the second group including all results measured on the 15th
#' through the end of the month.
#' These plots are intended to be placed in the seasonal variation and critical period section of
#' temperature TMDLs. The temperature criteria line is drawn based on the
#' fish use designation for each station.
#' A yellow box can be included to represent times of impairment.
#'
#' @param df Data frame of data to be graphed
#' @param station_col Name of column containing station ID, name ,or grouping label. This is the value used for facet. Default is "MLocID".
#' @param date_col Name of the datetime column. Format as character in "yyyy-mm-dd". Defaults is "SampleStartDate".
#' @param result_col Name of column containing result information. Default is 'Result_Numeric'.
#' @param temp_criteria_col Name of column containing the numeric non spawning temperature criteria. Defaults is "Temp_Criteria".
#' @param spawn_start_col Name of column containing the spawning start date. Format as character in "mm-dd". Defaults is "SpawnStart".
#' @param spawn_end_col Name of column containing the spawning end date. Format as character in "mm-dd". Defaults is "SpawnEnd".
#' @param critical_plot Logical if the plot should highlight the critical period. The critical period shown on the plot is the same for all data in df. If different periods are desired the data will need to be separated and sent to this function individually.
#' @param critical_start Start of the critical period. Format as character in "mm-dd" or "guess". Guess rounds the date down by half month periods based on the earliest date temperatures exceed the temperature criteria in any given year. Default is "guess".
#' @param critical_end End of the critical period. Format as character in "mm-dd" or "guess. Guess rounds the date up by half month periods based on the latest date temperatures exceed the temperature criteria in a given year. Default is "guess".
#' @param facet Logical if the plot should be a facet wrap using the station_col name to facet by. Default is FALSE.
#' @param facet_cols Number of columns in the facet wrap. Ignored if facet = FALSE. Default is 2.
#' @param y_axis_max Maximum value for the y-axis. Default is 28.
#' @param y_axis_int Interval between y-axis labels. Default is 5.
#' @param y_label Label for the y-axis. Default is '7DADM Temperature (deg-C)'.
#' @examples
#'  \dontrun{
#'
#'  library(AWQMSdata)
#'  library(odeqtmdlplots)
#'
#'  df.raw <- AWQMS_Data(station = '14034470', char = "temperature, water",
#'                     stat_base = "7DADM", crit_codes = TRUE)
#'  df <- df.raw %>%
#'  left_join(AWQMSdata::Temp_crit, by = "FishCode") %>%
#'  left_join(AWQMSdata::LU_spawn, by = "SpawnCode") %>%
#'  mutate(Result_Numeric = if_else(Result_Unit == "deg F",
#'                                   (Result_Numeric - 32) * 5/9,
#'                                   Result_Numeric)) %>%
#'  select(MLocID, StationDes, Temp_Criteria, SpawnStart, SpawnEnd, Result_Numeric)
#'
#'  plot_seasonality_box(df = df)
#'
#' }
#'
#'
#' @return plot of all 7DADM data, with criteria and critical period
#'  \if{html}{\figure{seasonality.png}{Plot}}
#'  \if{latex}{\figure{seasonality.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export
#'


plot_seasonality_box <- function(df,
                                 station_col = 'MLocID',
                                 date_col = 'SampleStartDate',
                                 result_col = 'Result_Numeric',
                                 temp_criteria_col = 'Temp_Criteria',
                                 spawn_start_col = 'SpawnStart',
                                 spawn_end_col = 'SpawnEnd',
                                 critical_plot = TRUE,
                                 critical_start = "guess",
                                 critical_end = "guess",
                                 facet = FALSE,
                                 facet_col = 2,
                                 y_axis_max = 28,
                                 y_axis_int = 5,
                                 y_label = "7DADM Temperature (deg-C)") {

  # Testing
  # df <- filter(df.p, MLocID %in% bullrun)
  # df <- filter(df.p, MLocID == "PWB_BR_BWMN_BR")
  # station_col = 'MLocID'
  # date_col = 'SampleStartDate'
  # result_col = 'Result_Numeric'
  # temp_criteria_col = 'Temp_Criteria'
  # spawn_start_col = 'SpawnStart'
  # spawn_end_col = 'SpawnEnd'
  # critical_plot = TRUE
  # critical_start = "guess"
  # critical_end = "guess"
  # facet = FALSE
  # facet_col = 2
  # y_axis_max = 28
  # y_axis_int = 5
  # y_label = "7DADM Temperature (deg-C)"

  # Stop Testing

  df1 <-  df[,c(station_col, date_col, result_col, temp_criteria_col, spawn_start_col, spawn_end_col)]

  colnames(df1) <- c("MLocID", "SampleStartDate", "Result_Numeric", "Temp_Criteria", "SpawnStart", "SpawnEnd")

  df2 <- df1 %>%
    dplyr::mutate(datetime = lubridate::ymd(SampleStartDate),
                  Year = lubridate::year(datetime),
                  Month = lubridate::month(datetime),
                  Day = lubridate::day(datetime),
                  day_boxplot = ifelse(Day < 15, 8, 23),
                  Spawn = ifelse(is.na(SpawnStart) | is.na(SpawnEnd), FALSE, TRUE),
                  SpawnStart = if_else(Spawn,
                                       lubridate::mdy(paste0(SpawnStart, "/", Year)),
                                       as.Date(NA)),
                  SpawnEnd = if_else(Spawn,
                                     lubridate::mdy(paste0(SpawnEnd, "/", Year)),
                                     as.Date(NA)),
                  SpawnStartMo = if_else(Spawn,
                                         lubridate::month(SpawnStart),
                                         NA_real_),
                  SpawnEndMo = if_else(Spawn,
                                       lubridate::month(SpawnEnd),
                                       NA_real_),
                  SpawnWinter = case_when(Spawn & (SpawnStartMo > SpawnEndMo) ~ TRUE,
                                          TRUE ~ FALSE),
                  BBNC = dplyr::case_when(Spawn & SpawnWinter & datetime >= SpawnStart & datetime >= SpawnEnd ~ 13,
                                          Spawn & SpawnWinter & datetime <= SpawnStart & datetime <= SpawnEnd ~ 13,
                                          Spawn & datetime >= SpawnStart & datetime <= SpawnEnd ~ 13,
                                          TRUE ~ Temp_Criteria),
                  date_boxplot = lubridate::ymd(paste0("2020-", Month, "-", day_boxplot)),
                  jday = lubridate::ymd(paste0("2020-", Month, "-", Day)),
                  legend = "Results") %>%
    dplyr::arrange(MLocID, datetime) %>%
    dplyr::select(MLocID, jday, date_boxplot, Year, Month, Result_Numeric, BBNC, legend)

  y_axis_breaks <- seq(0, y_axis_max, by = y_axis_int)

  if (critical_start == "guess") {

    critical_start <- df2 %>%
      dplyr::filter(Result_Numeric - BBNC > 0) %>%
      dplyr::slice(which.min(jday)) %>%
      dplyr::mutate(end = dplyr::case_when(lubridate::day(jday) >= 15 ~ lubridate::floor_date(jday, unit = "month") + lubridate::ddays(14),
                                           TRUE ~ lubridate::floor_date(jday, unit = "month")),
                    end = format(end, format = "%m-%d")) %>%
      dplyr::pull(end)
  }

  if (critical_end == "guess") {

    critical_end <- df2 %>%
      dplyr::filter(Result_Numeric - BBNC > 0) %>%
      dplyr::slice(which.max(jday)) %>%
      dplyr::mutate(end = dplyr::case_when(lubridate::day(jday) < 15 ~ lubridate::floor_date(jday, unit = "month") + lubridate::ddays(14),
                                           TRUE ~ lubridate::ceiling_date(jday, unit = "month")),
                    end = format(end, format = "%m-%d")) %>%
      dplyr::pull(end)
  }


  df.dates <- df1 %>%
    dplyr::select(MLocID, Temp_Criteria, SpawnStart, SpawnEnd) %>%
    dplyr::distinct()

  df.bbnc <- expand.grid(MLocID = unique(df1$MLocID),
                         datetime = seq(lubridate::ymd("2020-01-01"),
                                        lubridate::ymd("2020-12-31"), by = "1 day")) %>%
    dplyr::left_join(df.dates, by = "MLocID") %>%
    dplyr::mutate(Year = lubridate::year(datetime),
                  Month = lubridate::month(datetime),
                  Spawn = ifelse(is.na(SpawnStart) | is.na(SpawnEnd), FALSE, TRUE),
                  SpawnStart = if_else(Spawn,
                                       lubridate::mdy(paste0(SpawnStart, "/2020")),
                                       as.Date(NA)),
                  SpawnEnd = if_else(Spawn,
                                     lubridate::mdy(paste0(SpawnEnd, "/2020")),
                                     as.Date(NA)),
                  SpawnStartMo = if_else(Spawn,
                                         lubridate::month(SpawnStart),
                                         NA_real_),
                  SpawnEndMo = if_else(Spawn,
                                       lubridate::month(SpawnEnd),
                                       NA_real_),
                  SpawnWinter = case_when(Spawn & (SpawnStartMo > SpawnEndMo) ~ TRUE,
                                          TRUE ~ FALSE),
                  BBNC = dplyr::case_when(Spawn & SpawnWinter & datetime >= SpawnStart & datetime >= SpawnEnd ~ 13,
                                          Spawn & SpawnWinter & datetime <= SpawnStart & datetime <= SpawnEnd ~ 13,
                                          Spawn & datetime >= SpawnStart & datetime <= SpawnEnd ~ 13,
                                          TRUE ~ Temp_Criteria),
                  legend = "Temperature Criteria",
                  jday = datetime) %>%
    dplyr::select(MLocID, jday, Year, Month, BBNC, legend)


  p1 <- ggplot()

  if (critical_plot) {

    p1 <- p1 +
      ggplot2::annotate("rect",
                        xmin = lubridate::ymd(paste0("2020-", critical_start)),
                        xmax = lubridate::ymd(paste0("2020-", critical_end)),
                        ymin = 0,
                        ymax = y_axis_max, alpha = 0.3, fill = "yellow")
  }

  p1 <- p1 +
    ggplot2::geom_boxplot(data = df2,
                          ggplot2::aes(x = date_boxplot,
                                       y = Result_Numeric,
                                       linewidth = legend,
                                       group = interaction(legend, date_boxplot)),
                          color = "grey47") +
    ggplot2::geom_line(data = df.bbnc,
                       ggplot2::aes(x = jday,
                                    y = BBNC,
                                    color = legend,
                                    group = interaction(MLocID, legend)),
                       linetype = "dashed",
                       linewidth = 0.5) +
    ggplot2::scale_linewidth_manual(values = c("Results" = 0.5)) +
    ggplot2::scale_color_manual(values = c("Temperature Criteria" = "black")) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                   strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
                   panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                   text = element_text(size = 10, family = "sans")) +
    ggplot2::scale_x_date(name = "Date",
                          limits = as.Date(c("2020-01-01", "2020-12-31")),
                          date_breaks = "1 month",
                          date_labels = "%b") +
    ggplot2::scale_y_continuous(name = y_label,
                                limits = c(0, y_axis_max),
                                breaks = y_axis_breaks)

  if (facet) {
    p1 <- p1 +
      ggplot2::facet_wrap(facet = ggplot2::vars(MLocID), ncol = facet_col)
  }

  return(p1)

}

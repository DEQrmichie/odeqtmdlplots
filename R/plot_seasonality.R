#' plot_seasonality
#'
#' This function produces a plot of seven day average daily maximum river
#' temperatures for one or more monitoring locations grouped by year. These plots are intended
#' to be placed in the seasonal variation and critical period section of
#' temperature TMDLs. The temperature criteria line is drawn based of
#' fish use station designation.
#' A yellow boxes can be included to represent times of impairment.
#'
#' @param df Data frame of data to be graphed
#' @param station_col Name of column containing station ID, name ,or grouping label. This is the value used for facet. Default is "MLocID".
#' @param date_col Name of the datetime column. Format as character in "yyyy-mm-dd". Defaults is "SampleStartDate".
#' @param result_col Name of column containing result information. Default is 'Result_Numeric'.
#' @param temp_criteria_col Name of column containing the numeric non spawning temperature criteria. Defaults is "Temp_Criteria".
#' @param spawn_start_col Name of column containing the spawning start date. Format as character in "mm-dd". Defaults is "SpawnStart".
#' @param spawn_end_col Name of column containing the spawning end date. Format as character in "mm-dd". Defaults is "SpawnEnd".
#' @param critical_plot Logical if the plot should highlight the critical period. The critical period shown on the plot is the same for all data in df. If different periods are desired the data will need to be seperated and sent to this fuction individually.
#' @param critical_start Start of the critical period. Format as character in "mm-dd" or "guess". Guess rounds the date down by half month periods based on the earliest date temperatures exceed the temperature criteria in any given year. Default is "guess".
#' @param critical_end End of the critical period. Format as character in "mm-dd" or "guess. Guess rounds the date up by half month periods based on the latest date temperatures exceed the temperature criteria in a given year. Default is "guess".
#' @param highlight_year Optional year to highlight on plot. Default is NULL.
#' @param facet Logical if the plot should be a facet wrap using the station_col name to facet by. Default is FALSE.
#' @param facet_cols Number of columns in the facet wrap. Ignored if facet = FALSE. Default is 2.
#' @param show_legend Logical if the legend should be included on the plot. default is TRUE. the highlight year is not included  on the legend.
#' @param y_axis_max Maximum value for the y-axis. Default is 28.
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
#'  plot_seasonality(df = df)
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


plot_seasonality <- function(df,
                             station_col = 'MLocID',
                             date_col = 'SampleStartDate',
                             result_col = 'Result_Numeric',
                             temp_criteria_col = 'Temp_Criteria',
                             spawn_start_col = 'SpawnStart',
                             spawn_end_col = 'SpawnEnd',
                             critical_plot = TRUE,
                             critical_start = "guess",
                             critical_end = "guess",
                             highlight_year = NULL,
                             facet = FALSE,
                             facet_col = 2,
                             show_legend = TRUE,
                             y_axis_max = 28,
                             y_label = "7DADM Temperature (deg-C)") {


  df1 <-  df[,c(station_col, date_col, result_col, temp_criteria_col, spawn_start_col, spawn_end_col)]

  colnames(df1) <- c("MLocID", "SampleStartDate", "Result_Numeric", "Temp_Criteria", "SpawnStart", "SpawnEnd")

  df2 <- df1 %>%
    dplyr::mutate(datetime = lubridate::ymd(SampleStartDate),
                  Year = lubridate::year(datetime),
                  Month = lubridate::month(datetime),
                  Day = lubridate::day(datetime),
                  SpawnStart = lubridate::mdy(paste0(SpawnStart, "/", Year)),
                  SpawnEnd = lubridate::mdy(paste0(SpawnEnd, "/", Year)),
                  SpawnWinter = ifelse(lubridate::month(SpawnStart) > lubridate::month(SpawnEnd), TRUE, FALSE),
                  BBNC = dplyr::case_when(SpawnWinter & datetime >= SpawnStart & datetime >= SpawnEnd ~ 13,
                                          SpawnWinter & datetime <= SpawnStart & datetime <= SpawnEnd ~ 13,
                                          datetime >= SpawnStart & datetime <= SpawnEnd ~ 13,
                                          TRUE ~ Temp_Criteria),
                  jday = lubridate::ymd(paste0("2020-", Month, "-", Day)),
                  legend = "Results") %>%
    dplyr::arrange(MLocID, datetime) %>%
    dplyr::select(MLocID, jday, Year, Result_Numeric, BBNC, legend)

  if (!is.null(highlight_year)) {

    df.highlight <- df2 %>%
      dplyr::filter(Year == highlight_year)

  }

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


  df3 <- df2 %>%
    dplyr::select(MLocID, jday, BBNC) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Result_Numeric = BBNC,
                  Year = lubridate::year(jday),
                  legend = "Temperature Criteria") %>%
    dplyr::arrange(MLocID, jday, legend) %>%
    dplyr::select(MLocID, jday, Year, Result_Numeric, BBNC, legend) %>%
    rbind(df2)

  p1 <-  df3 %>%
    ggplot2::ggplot(ggplot2::aes(x = jday))


  if (critical_plot) {

    p1 <- p1 +
      ggplot2::annotate("rect",
                        xmin = lubridate::ymd(paste0("2020-", critical_start)),
                        xmax = lubridate::ymd(paste0("2020-", critical_end)),
                        ymin = 0,
                        ymax = y_axis_max, alpha = 0.3, color = "yellow")
  }


  p1 <- p1 +
    ggplot2::geom_line(ggplot2::aes(y = Result_Numeric,
                                    color = legend,
                                    linetype = legend,
                                    group = interaction(MLocID, Year, legend)),
                       linewidth = 0.5, show.legend = show_legend) +
    ggplot2::scale_color_manual(values = c("Temperature Criteria" = "black", "Results" = "grey47")) +
    ggplot2::scale_linetype_manual(values = c("Temperature Criteria" = "dashed", "Results" = "solid")) +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
                   strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
                   panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                   text = element_text(size = 10, family = "sans")) +
    ggplot2::scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b") +
    ggplot2::scale_y_continuous(name = y_label,
                                limits = c(0, y_axis_max))

  if (!is.null(highlight_year)) {
    p1 <- p1 +
      ggplot2::geom_line(data = df.highlight,
                         ggplot2::aes(x = jday,
                                      y = Result_Numeric,
                                      group = interaction(MLocID, Year)),
                         linewidth = 1,
                         linetype = "solid",
                         color = "black",
                         show.legend = FALSE, inherit.aes = FALSE)

  }

  if (facet) {
    p1 <- p1 +
      ggplot2::facet_wrap(facet = ggplot2::vars(MLocID), ncol = facet_col)
  }

  return(p1)

}

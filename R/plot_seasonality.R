#' plot_seasonality
#'
#' This function produces a graph pf seven day average daily maximum river
#' temperatures for a single monitoring location. These graphs are intended
#' to be placed in the thrid section of the temperture TMDL. Data is pulled
#' from AWQMS using the AWQMSdata package.  Choose a monitoring location with
#' sufficient continuous temperature (many years worth of continuous data).
#' Criteria line is drawn based of fish use station desgination.
#' Blue boxed represent times of unlikely impairments. They are drawn at the
#' first and last day in dataset with a value of buffer * criteria.
#'
#' @param station Monitoring station to be used
#' @param buffer Value < 1 used to draw boxed of unlikely impariemnts. Defaults to 0.80
#' @param label.y vertical adjustment of "Impairment unlikely" label
#' @return plot of all 7DADM data, with criteria and identified unlikely impariment seasons
#' @importFrom magrittr "%>%"
#' @export
#'

plot_seasonality <- function(station, buffer = 0.80, label.y = 12){


AWQMS_dat <- AWQMSdata::AWQMS_Data(station = station, char = 'Temperature, water', stat_base = '7DADM', crit_codes = TRUE)


graph_data <- AWQMS_dat %>%
  dplyr::left_join(AWQMSdata::Temp_crit, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::left_join(AWQMSdata::LU_spawn, by = "SpawnCode") %>%
  dplyr::left_join(AWQMSdata::LU_FishUse, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::select(OrganizationID, Org_Name,  HUC8, HUC8_Name, AU_ID, MLocID,
         Lat_DD, Long_DD, SampleStartDate,Result_Operator,
         Result, Result_Numeric, Result_Unit, Statistical_Base,FishCode,
         Temp_Criteria, SpawnStart, SpawnEnd, FishUse) %>%
  dplyr::mutate(year = lubridate::year(SampleStartDate),
         month = lubridate::month(SampleStartDate)) %>%
  dplyr::mutate(doy = strftime(SampleStartDate, format = "%j"),
         fakedate = as.Date(as.numeric(doy),  origin = as.Date("2018-01-01")))



season <- graph_data %>%
  dplyr::filter(Result_Numeric > (0.90 * dplyr::first(graph_data$Temp_Criteria)))


p <- ggplot2::ggplot() +
  ggplot2::geom_line(data = graph_data,
            ggplot2::aes(x = as.Date(as.numeric(doy),
                            origin = as.Date("2018-01-01")),
                y = Result_Numeric, group = year),
            color = "grey47")+
  ggplot2::geom_hline(yintercept =  dplyr::first(graph_data$Temp_Criteria),
             linetype = "longdash",
             size = 0.7) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(0,
                               origin = as.Date("2018-01-01")),
                xmax = as.Date(as.numeric(min(season$doy)),
                               origin = as.Date("2018-01-01")),
                ymin = 0,
                ymax = dplyr::first(graph_data$Temp_Criteria)),
            alpha = 0.2, fill = 'steelblue2'
  ) +
  ggplot2::geom_rect(ggplot2::aes(xmax = as.Date(365,
                               origin = as.Date("2018-01-01")),
                xmin = as.Date(as.numeric(max(season$doy)),
                               origin = as.Date("2018-01-01")),
                ymin = 0,
                ymax = dplyr::first(graph_data$Temp_Criteria)),
            alpha = 0.2, fill = 'steelblue2')+
  ggplot2::theme_classic()+
  ggplot2::scale_y_continuous(breaks=seq(0,100,5),
                     limits = c(0,NA),
                     expand = c(0,0)) +
  ggplot2::scale_x_date(breaks = seq(as.Date("2018-01-01"),
                            as.Date("2018-12-31"),
                            by = "1 months"),
               date_labels = "%b",
               expand = ggplot2::expand_scale()) +
  ggplot2::annotate("text", label = dplyr::first(graph_data$FishUse),
           x = as.Date('2018-03-15'),
           y = dplyr::first(graph_data$Temp_Criteria) + 0.5,
           size = 3) +
  ggplot2::labs(x = "Date",
       y = "7DADM Temperature (°C)",
       caption = paste("Data source:", dplyr::first(graph_data$Org_Name))) +
  ggplot2::annotate("text", label = "Impairment \n unlikely",
           x = as.Date('2018-01-10'),
           y = label.y,
           size = 3,
           hjust = 0) +
  ggplot2::annotate("text", label = "Impairment \n unlikely",
           x = as.Date('2018-12-28'),
           y = label.y,
           size = 3,
           hjust = 1)


return(p)

}



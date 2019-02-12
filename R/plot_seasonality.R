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
#' @param station Monitoring station to be used. Can be single station or vector of stations.
#' @param startdate Startdate of data pull
#' @param enddate Enddate of data pull
#' @param buffer Value < 1 used to draw boxed of unlikely impariemnts. Defaults to 0.80
#' @param label_y vertical adjustment of "Impairment unlikely" label
#' @param highlight_year year, or vector of years to highlight on the plot
#' @param include_station_legend TRUE/FALSE for whether to show the station legend. Defauls to false
#' @param include_highlight_legend TRUE/FALSE for whether to show the highlighted year legend. Defauls to false
#' @examples plot_seasonality(station = '14034470', buffer = 0.8,
#'   label_y = 12, highlight_year = 2014, include_station_legend = FALSE,
#'   include_highlight_legend = FALSE)
#' @return plot of all 7DADM data, with criteria and identified unlikely impariment seasons
#'  \if{html}{\figure{seasonality.png}{Plot}}
#'  \if{latex}{\figure{seasonality.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export
#'

plot_seasonality <- function(station,
                             startdate = NULL,
                             enddate = NULL,
                             buffer = 0.80,
                             label_y = 12,
                             highlight_year = NULL,
                             include_station_legend = FALSE,
                             include_highlight_legend = FALSE ){


# Get data from AWQMS -----------------------------------------------------


  if(!is.null(startdate) & !is.null(enddate)){


    AWQMS_dat <- AWQMSdata::AWQMS_Data(startdate = startdate,
                                     enddate = enddate,
                                     station = station,
                                     char = 'Temperature, water',
                                     stat_base = '7DADM',
                                     crit_codes = TRUE)

    }  else if (!is.null(startdate) & is.null(enddate)){


    AWQMS_dat <- AWQMSdata::AWQMS_Data(startdate = startdate,
                                       station = station,
                                       char = 'Temperature, water',
                                       stat_base = '7DADM',
                                       crit_codes = TRUE)

  } else if (is.null(startdate) & !is.null(enddate)){



    AWQMS_dat <- AWQMSdata::AWQMS_Data(startdate = startdate,
                                       enddate = enddate,
                                       station = station,
                                       char = 'Temperature, water',
                                       stat_base = '7DADM',
                                       crit_codes = TRUE)

  } else {

    AWQMS_dat <- AWQMSdata::AWQMS_Data(station = station, char = 'Temperature, water', stat_base = '7DADM', crit_codes = TRUE)

  }






# Join with standards and prep data for graph -----------------------------


graph_data <- AWQMS_dat %>%
  dplyr::left_join(AWQMSdata::Temp_crit, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::left_join(AWQMSdata::LU_spawn, by = "SpawnCode") %>%
  dplyr::left_join(AWQMSdata::LU_FishUse, by = c("FishCode" = "FishUse_code")) %>%
  dplyr::mutate(SampleStartDate = lubridate::ymd(SampleStartDate)) %>%
  dplyr::select(OrganizationID, Org_Name,  HUC8, HUC8_Name, AU_ID, MLocID,
         Lat_DD, Long_DD, SampleStartDate,Result_Operator,
         Result, Result_Numeric, Result_Unit, Statistical_Base,FishCode,
         Temp_Criteria, SpawnStart, SpawnEnd, FishUse) %>%
  dplyr::group_by(MLocID) %>%
  tidyr::complete(SampleStartDate = seq.Date(min(SampleStartDate), max(SampleStartDate), by="day")) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(year = lubridate::year(SampleStartDate),
         month = lubridate::month(SampleStartDate)) %>%
  dplyr::mutate(doy = strftime(SampleStartDate, format = "%j"),
         fakedate = as.Date(as.numeric(doy),  origin = as.Date("2018-01-01")))


# create dataframe filtered to highlighted years, if applicable -----------


if(!is.null(highlight_year)){

  highlight <- graph_data %>%
    dplyr::filter(year %in% highlight_year)

}



# Set the dounds of the boxes ---------------------------------------------

# Move to nearest 1st or 15th of the month.

season <- graph_data %>%
  dplyr::filter(Result_Numeric > (buffer * dplyr::first(graph_data$Temp_Criteria)))

first_date <- as.Date(as.numeric(min(season$doy, na.rm = TRUE)),
                      origin = as.Date("2018-01-01"))

if(lubridate::day(first_date) > 15){

  sub <- lubridate::day(first_date)-15

  first_date <- first_date - sub

} else if(lubridate::day(first_date) < 15 &
          lubridate::day(first_date) > 1) {

  sub <- lubridate::day(first_date)

   first_date <- first_date - (sub - 1)

}


last_date <- as.Date(as.numeric(max(season$doy, na.rm = TRUE)),
                      origin = as.Date("2018-01-01"))

if(lubridate::day(last_date) > 15){

  last_date <- lubridate::round_date(last_date, unit = "month")

} else if(lubridate::day(last_date) < 15 &
          lubridate::day(last_date) > 1) {

  sub <- lubridate::day(last_date)

  last_date <- last_date + (15 - sub)

}



# Create plot -------------------------------------------------------------



p <- ggplot2::ggplot() +
  ggplot2::geom_line(data = graph_data,
            ggplot2::aes(x = as.Date(as.numeric(doy),
                            origin = as.Date("2018-01-01")),
                         y = Result_Numeric,
                         group = interaction(year,MLocID),
                         linetype = MLocID),
            color = "grey47")+
  ggplot2::geom_hline(yintercept =  dplyr::first(graph_data$Temp_Criteria),
             linetype = "longdash",
             size = 0.7) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(0,
                               origin = as.Date("2018-01-01")),
                xmax = first_date,
                ymin = 0,
                ymax = dplyr::first(graph_data$Temp_Criteria)),
            alpha = 0.3, fill = 'steelblue2'
  ) +
  ggplot2::geom_rect(ggplot2::aes(xmax = as.Date(365,
                               origin = as.Date("2018-01-01")),
                xmin = last_date,
                ymin = 0,
                ymax = dplyr::first(graph_data$Temp_Criteria)),
            alpha = 0.3, fill = 'steelblue2')+
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
       caption = paste("Data source:", dplyr::first(graph_data$Org_Name)),
       linetype = "Stations") +
  ggplot2::annotate("text", label = "Impairment \n unlikely",
           x = as.Date('2018-01-10'),
           y = label_y,
           size = 3,
           hjust = 0) +
  ggplot2::annotate("text", label = "Impairment \n unlikely",
           x = as.Date('2018-12-28'),
           y = label_y,
           size = 3,
           hjust = 1)


# Add highligted year sections, if applicable -----------------------------



if(!is.null(highlight_year)){

  p <- p +
    ggplot2::geom_line(data = highlight,
                       ggplot2::aes(x = as.Date(as.numeric(doy),
                                                origin = as.Date("2018-01-01")),
                                    y = Result_Numeric,
                                    group = interaction(year,MLocID),
                                    color = as.character(highlight_year),
                                    linetype = MLocID),
                       size = 1.2) +
    ggplot2::scale_color_manual(name = '', values = "black")

}


# manage showing of legends -----------------------------------------------


# hide mlocId legend if applicable
if(isFALSE(include_station_legend)){

  p <- p +
    ggplot2::guides(linetype=FALSE)
}

# hide highlited year legend if applicable
if(isFALSE(include_highlight_legend)){

  p <- p +
    ggplot2::guides(color=FALSE)
}


# Return graph ------------------------------------------------------------


return(p)

}



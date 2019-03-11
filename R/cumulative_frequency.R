#' cfreqdist
#'
#' This function creates a graphc of two plots First plot is the
#' cumulative frequency distribution of two monitoring locations.
#' The second is the cumulative frequency of the difference between the
#' two monitoring locations (station1 - station2). Data input should be a
#' dataframe of data in 'long' format for two locations. Dataframe should have
#' at least 3 columns, station, data, and result.
#'
#' @param df Dataframe of data. Should be in long format
#' @param station1 Name of station 1 in the station column (station_col)
#' @param station2 Name of station 2 in the station column (station_col)
#' @param station_col Name of the column conatining the station names
#' @param date_col Name of the column containing the date information
#' @param result_col Name of the column containing the results to plot
#' @param x_int_cfd Verical line on the cfd graph. Used to show water quality
#'   standard. If NULL, no line is drawn.
#' @param x_int_cfd_diff Vertical line on the cfd_difference graph. Defaults to 0
#' @param p1title Title of the cumulative frequency distribution plot
#' @param p2title Title of the difference cumulative frequency distribution plot
#' @param leg_x_nudge Move the legend left or right. (-0.25 to 0.25)
#' @param leg_y_nudge Move the legend up or down. (-0.25 to 0.25)
#' @examples
#'  \dontrun{
#'  library(tidyverse)
#'  library(AWQMSdata)
#'  data <- AWQMS_Data(station = c('14046778', '14034470'),
#'  char = 'Temperature, water',
#'  stat_base = '7DADM',
#'  crit_codes = TRUE)
#'
#'
#'  data <- data %>%
#'  mutate(stationname = ifelse(MLocID == "14046778", "without dams",
#'                       ifelse(MLocID == "14034470", "with dams", "ERROR" ) )) %>%
#'  filter(lubridate::month(SampleStartDate) %in% c(7,8))
#'
#'
#'
#'
#'  cfreqdist(df = data,
#'            station1 = "without dams",
#'            station2 = "with dams",
#'            station_col = "stationname",
#'            date_col = "SampleStartDate",
#'            result_col = "Result_Numeric",
#'            p1title = "July - August",
#'            p2title = "Difference")
#'   }
#' @return plot of all cumulative frequency and cumulative frequency
#'  difference
#'  \if{html}{\figure{cumfreq.png}{Plot}}
#'  \if{latex}{\figure{cumfreq.png}{options: width=0.5in}}
#' @importFrom magrittr "%>%"
#' @export




cfreqdist <- function(df,
                      station1 = NULL,
                      station2 = NULL,
                      station_col = "MLocID",
                      date_col = "SampleStartDate",
                      result_col = "Result_Numeric",
                      p1title = NULL,
                      p2title = NULL,
                      leg_x_nudge = 0,
                      leg_y_nudge = 0,
                      x_int_cfd = NULL,
                      x_int_cfd_diff = 0){

#get consistent naming and discard unneeded columns
graph_data <- df[,c(station_col, date_col,result_col)]
colnames(graph_data) <- c("MLocID", "SampleStartDate", "Result_Numeric")



# Spread the data out to use for cfd_diff graph ---------------------------

data_spread <- graph_data %>%
  dplyr::mutate(MLocID = ifelse(MLocID == station1, 'station1',
                ifelse(MLocID == station2,'station2', 'ERROR'))) %>%
  dplyr::select(MLocID, SampleStartDate, Result_Numeric) %>%
  tidyr::spread(key = MLocID, value = Result_Numeric)


# Calculate difference
data_spread$diff = data_spread$station1 - data_spread$station2



# Plot cfd graoh ----------------------------------------------------------


p1 <- ggplot2::ggplot(data = graph_data) +
  ggplot2::stat_ecdf(ggplot2::aes(x = Result_Numeric, color =MLocID, linetype = MLocID ),
            geom = "line",
            size = 1,
            na.rm = TRUE) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5,
                                  size = 10),
        legend.position = c(0.75 + leg_x_nudge, 0.25 + leg_y_nudge),
        legend.text=element_text(size=7),
        legend.background = element_rect(fill=alpha(0.01))) +
  ggplot2::scale_y_continuous(breaks=seq(0,1,0.2),
                              limits = c(0,1),
                              expand = c(0,0)) +
  ggplot2::scale_color_discrete("") +
  ggplot2::scale_linetype_manual("",
                        values=rep(c(1,2)))



#Draw x intercept line if cfd_x_int is not null
if(!is.NULL(x_int_cfd)){

  p1 <- P1 +
    ggplot2::geom_vline(xintercept = x_int_cfd)

}


#Add titles. If p1title is null, leave out that title.
if(!is.Null(p1title)){

  p1 <- p1+
    ggplot2::labs(x = "Temperature (°C)",
                y = "Quantile",
                title = p1title
  )

} else {
  p1 <- p1+
    ggplot2::labs(x = "Temperature (°C)",
                  y = "Quantile",
                  title = ggplot2::element_blank()
    )

}



# Plot cfd diff graph----------------------------------------------------------

p2 <- ggplot2::ggplot(data = data_spread) +
  ggplot2::stat_ecdf(ggplot2::aes(x = diff),
            geom = "line",
            size = 1,
            na.rm = TRUE) +
  ggplot2::geom_vline(xintercept = x_int_cfd_diff) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5,
                                  size = 10)) +
  ggplot2::scale_y_continuous(position = 'right',
                     breaks=seq(0,1,0.1),
                     limits = c(0,1),
                     expand = c(0,0))

if(!is.NULL(p2title)){
  p2 <- p2 +
    ggplot2::labs(x = "Temperature (°C)", title = p2title, y = NULL)

} else {
  p2 <- p2 +
    ggplot2::labs(x = "Temperature (°C)", title = ggplot2::element_blank(), y = NULL)
  }


cowplot::plot_grid(p1, NULL, p2, nrow = 1, rel_widths = c(2, 0.1, 1))

}





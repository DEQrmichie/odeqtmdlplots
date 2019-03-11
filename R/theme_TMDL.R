#' theme_TMDL
#'
#' A theme to base graphs on for TMDL reports
#'
#' @inheritParams ggplot2::theme_classic
#'
#' @export
#' @importFrom ggplot2 theme_bw



theme_TMDL <- function(base_size = 11){


  theme <- ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      size = 10),
                 legend.background = element_rect(fill=alpha(0.01)))

}



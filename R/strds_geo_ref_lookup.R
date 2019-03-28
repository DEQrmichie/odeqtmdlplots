#' standards_lookup
#'
#' Exports data from the georeferenced standards database. Geodatabase is
#' fairly large, so input requires a vector if HUC8 codes to use to filter
#' data. Function will return data table of data from those HUC8s
#'
#' @param HUC8 Vector of HUC8s to filter on.
#' @param layer Which layer of geodatabase to use, Defaults to "Oregon_Standards"
#' @param gdb_file Location of the gdf folder to use
#' @examples
#'  \dontrun{
#'   standards_lookup(HUC8 = c("17050116", "17050117", "17050118", "17050119", "17050115"))
#'   }
#' @return Dataframe of data from the standards layer
#' @export




standards_lookup <- function(HUC8,
                             exclude_field = NULL,
                             exclude_params = NULL,
                             layer = "Oregon_Standards",
                             gdb_file = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_Standards/GeoRef_Standards.gdb"){



  query <- "select * from Oregon_Standards"

  for(i in 1:length(HUC8)){

    if(i == 1){
      query <- paste0(query, " WHERE ReachCode like '", HUC8[i], "%'")

    } else {
      query <- paste0(query, " OR ReachCode like '", HUC8[i], "%'")

    }

  }

if(!is.null(exclude_field)) {




  query <- paste0(query, " AND ", exclude_field, " NOT IN (",paste0("'", exclude_params, "'", collapse=", ") , ")" )

}


  import_data <- sf::st_read(gdb_file,
                         layer = layer,
                         query = query,
                         stringsAsFactors = FALSE)

  return(import_data)

}




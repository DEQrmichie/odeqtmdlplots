#'impaired_waters
#'
#'This function will return a dataframe of waterbodies listed as either category 4 or 5 for
#'a given set of subbasins (huc4's) and parameters
#'@param database Source database
#'@param table Table in database to source data from.
#'@param parameter string, or vector of strings, for parameter to be used
#'@param subbasins string, or vector of strings, of HUC 4 subbasin names
#'@return Dataframe of IR category 4 and category 5
#'@export



impaired_waters <- function(database = "WQ303d", table = "tbl2012303dIntegratedList",  parameter = NULL, HUC8Code = NULL){


  #Connect to database
  con <- DBI::dbConnect(odbc::odbc(), database)

  # Change these subbasins to change results in produced table


  query <- paste0("SELECT [4thFieldName] as 'Subasin',
  [StreamLakeName] as 'Waterbody',
  [Miles] as 'River Mile',
  [Parameter],
  [NumericCriteria] as 'Criterion',
  right([ListingYear], 4) as 'List Date',
  [PKmatrixID] as 'Record ID'
  FROM [WQ303d].[dbo].[",table,"]
  Where ListingStatusKey in (20, 14, 15, 16,17)")

#
# Parameter in ({parameter})
#   AND (ListingStatus like 'Cat 4%' or
#   ListingStatus like 'Cat 5%')
#   AND ("

  if(!is.null(parameter)){

    query <- paste(query, "\n AND Parameter in ({parameter})" )

  }

  if(!is.null(subbasins)){


    query <- paste(query, "AND \n (")

  for (i in 1:length(HUC8Code)) {
    query <- paste0(query, "[4thFieldHUC] like '%",subbasins[i], "%'" )

    if(i < length(subbasins)) {
      query <- paste0(query, " or ")
    } else {
      query <- paste0(query,")")
    }

  }

  }



  query <- paste0(query, "\n order by Subasin, Waterbody")



  # Create query language
  qry <- glue::glue_sql(query, .con = con)

  #
  # Query the database
  impairments <- DBI::dbGetQuery(con, qry)

  #qry
  return(impairments)
}



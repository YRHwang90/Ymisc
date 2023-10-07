#' wide2long function is data-reshaping function for data.
#' This function targets mainly on the brain structure data that contains the data from the left and right hemisphere
#'
#' @param data The wide format data.
#' @param ID The column of identifiers.
#' @param seperator A character vector that separating character in the variable names.
#' @param hemisphere Whether a hemisphere indicator in the variable names is prefix or suffix. At this point, only "prefix" option is available.
#' @param start The column that indicate a sets of variables that correspond to be more variables in wide format
#' @param end The column that indicate a sets of variables that correspond to be more variables in wide format
#' @return The long format data
#'
#' @export
#'
#'
#' @example
#'
#'long<-wide2long(
#' data=sample_data,
#' ID="ID",
#' separator="_",
#' start=2,
#' end=15,
#' hemisphere="prefix"
#' )


wide2long<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){

  if(hemisphere!="prefix"){
    stop("Hemisphere indicator should be the prefix.")
  } else{


    start<-match(start,names(data))
    end<-match(end,names(data))

    d<-reshape(data=data,
                    direction="long",
                    idvar=ID,
                    varying=start:end,
                    sep=separator,
                    timevar="region")

  data<-data.frame(d[order(d[,ID]),])


  }

  return(data)

}


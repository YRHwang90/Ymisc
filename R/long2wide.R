
#' long2wide function is data-reshaping function for data.#'
#' This function targets mainly on that brain structure data that contains the data from the left and right hemisphere
#'
#' @param data The long-formatted data.
#' @param ID The column of identifiers.
#' @param seperator A character vector that separating character in the variable names.
#' @param hemisphere Whether a hemisphere indicator in the variable names is prefix or suffix. At this point, only "prefix" option is available.
#' @param start The column that indicate a sets of variables that correspond to be more variables in wide format
#' @param end The column that indicate a sets of variables that correspond to be more variables in wide format
#' @return The wide format data
#'
#' @export
#'
#' @example
#'
#' wide<-long2wide(
#' data = long,
#' ID="ID",
#' separator="_",
#' hemisphere="prefix",
#' start="
#'
#' )

long2wide<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){

  if(hemisphere!="prefix"){
    stop("Hemisphere indicator should be the prefix.")
  }else {


    start<-match(start,names(data))
    end<-match(end,names(data))

    ID<-data[,-c(start:end)]

    reshaped_data <- reshape(results,
                             idvar = colnames(ID),
                             timevar = "region",
                             direction = "wide",
                             sep=separator)

    data<- reshaped_data[order(reshaped_data[,ID]),]
    return(data)
  }
  }

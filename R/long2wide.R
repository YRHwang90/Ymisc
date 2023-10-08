#' long2wide function is data-reshaping function for long format data.
#' This function mainly targets the brain structure data that contains the data from the left and right hemisphere
#'
#' @param data The long format data.
#' @param ID The column of identifiers.
#' @param separator A character vector that separates characters in the variable names.
#' @param hemisphere The character vector that indicates whether a hemisphere indicator in the variable names is the prefix or suffix. At this point, only a "prefix" option is available.
#' @param start The column that specifies the starting point of a set of variables that distinguish multiple types of information from one participant.
#' @param end The column that specifies the endpoint of a set of variables that distinguish multiple types of information from one participant.
#' @return The wide format data
#'
#' @export
#'
#' @examples
#'
#' long2wide(
#' data = sample_data,
#' ID="ID",
#' separator="_",
#' hemisphere="prefix",
#' start="lh_Thalamus",
#' end="rh_AccumbensArea"
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

    data<- data.frame(reshaped_data[order(reshaped_data[,ID]),])

    return(data)
  }
  }

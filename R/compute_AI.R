#' compute_AI function calculates regional brain asymmetry index (AI) using widely-used formula.
#' AI = [(left-right)/(left+right)]
#'
#' @param data The wide format data
#' @param ID The column of identifiers.
#' @param left_hemisphere The prefix or suffix that indicates the left hemisphere in the variable names
#' @param right_hemisphere The prefix or suffix string that indicates the right hemisphere in the variable names
#' @param hemisphere The character vector that indicates whether a hemisphere indicator in the variable names is a prefix or suffix.
#' @param separator A character vector that separates characters in the variable names.
#' @param start The column that specifies the starting point of the column that contains brain measures needed to calculate AI.
#' @param end The column that specifies the starting point of the column that contains brain measures needed to calculate AI.
#' @return The data with AIs.
#'
#' @export
#'
#' @example
#'
#'compute_AI(sample_data,
#'left_hemisphere = "lh",
#'right_hemisphere = "rh",
#'separator="_",
#'ID="ID",
#'hemisphere="prefix",
#'start="lh_Thalamus",
#'end="rh_AccumbensArea")

compute_AI <- function(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       separator="_",
                       ID="ID",
                       hemisphere="prefix",
                       start ,
                       end  ) {



  start<-match(start,names(data))
  end<-match(end,names(data))

  NID<-match(ID,names(data))
  data2<-data[,c(start:end,NID)]

  namelist<-colnames(data[,c(start:end)])

  slist<-strsplit(namelist,split="_")

  llist<-list()

  if(hemisphere=="prefix"){

    for(i in 1:length(slist)){
     llist[i]<-  slist[[i]][2]
    }

  } else if(hemisphere=="suffix"){

    for(i in 1:length(slist)){
      llist[i]<-  slist[[i]][1]
    }
  }
  llist<-unique(llist)

  for( i in 1:length(llist)){

    data2[[paste0("AI",seperator,llist[[i]])]] <-
      (data2[[paste0(left_hemisphere,seperator,llist[[i]])]] - data2[[paste0(right_hemisphere,seperator,llist[[i]])]]) / (data2[[paste0(left_hemisphere,seperator,llist[[i]])]] + data2[[paste0(right_hemisphere,seperator,llist[[i]])]])

  }

data4<-data2[,grep("AI", names(data2))]
data<-data.frame(cbind(data,data4))

return(data)
}

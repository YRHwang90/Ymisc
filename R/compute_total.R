#' compute_total function calculates bilateral (L+R) measures.
#'
#' @param data The wide format data
#' @param ID The column of identifiers.
#' @param left_hemisphere The prefix or suffix that indicate left hemisphere in the variable names
#' @param right_hemisphere The prefix or suffix string that indicate right hemisphere in the variable names
#' @param hemisphere Whether a hemisphere indicator in the variable names is prefix or suffix.
#' @param seperator A character vector that separating character in the variable names.
#' @param start The column that contains  brain measures that needs to calculate AI.
#' @param end The column that contains  brain measures that needs to calculate AI.
#' @return The data with the bilateral (L+R) measures.
#'
#' @export
#'
#' @example
#'
#'compute_total(sample_data,
#'left_hemisphere = "lh",
#'right_hemisphere = "rh",
#'separator="_",
#'ID="ID",
#'hemisphere="prefix",
#'start="lh_Thalamus",
#'end="rh_AccumbensArea")

compute_total <- function(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       seperator="_",
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

  if (hemisphere=="prefix"){
    for(i in 1:length(slist)){
      llist[i]<-  slist[[i]][2]
    }
  } else if (hemisphere=="suffix"){
    for(i in 1:length(slist)){
      llist[i]<-  slist[[i]][1]
    }
  }

  llist<-unique(llist)

  for( i in 1:length(llist)){

    data2[[paste0("total",seperator,llist[[i]])]] <-
   data2[[paste0(left_hemisphere,seperator,llist[[i]])]] + data2[[paste0(right_hemisphere,seperator,llist[[i]])]]

  }

  data4<-data2[,grep("total", names(data2))]
  data<-data.frame(cbind(data,data4))

}

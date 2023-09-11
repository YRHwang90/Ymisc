compute_AI <- function(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       seperator="_",
                       ID="IDD",
                       start = 1,
                       end = 14) {
  data$ID<-NULL
  data$IDD<-1:100
  NID<-match(ID,names(data))
  data2<-data[,c(start:end,NID)]

  namelist<-colnames(data2)
  use strsplit.

  if(colnames[i] %in% seperator)

  }

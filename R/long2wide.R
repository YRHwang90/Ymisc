
wide2long<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){


  ID<-data[,-c(start:end)]

  reshaped_data <- reshape(results,
                           idvar = colnames(ID),
                           timevar = "region",
                           direction = "wide",
                           sep=separator)

  data<- reshaped_data[order(reshaped_data[,ID]),]
return(data)
  }

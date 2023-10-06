
long2wide<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){

  if(hemisphere!="prefix"){
    stop("Hemisphere indicator should be the prefix.")
  }else {


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

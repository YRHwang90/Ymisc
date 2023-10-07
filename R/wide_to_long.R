
wide2long<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){

  if(hemisphere!="prefix"){
    stop("Hemisphere indicator should be the prefix.")
  } else{

#
#     start<-match(start,names(data))
#     end<-match(end,names(data))

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

#example

long<-wide2long(data=sample_data,
                   ID="ID",
                   separator="_",
                   start=2,
                   end=15,
                   hemisphere="prefix"
                   )

long<-data.frame(long)

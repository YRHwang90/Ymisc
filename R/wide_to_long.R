
wide2long<-function(data,
                    ID="ID",
                    separator="_",
                    hemisphere="prefix",
                    start,
                    end){

  d<-reshape(data=data,
             direction="long",
             idvar=ID,
             varying=start:end,sep=separator,
             timevar="region")

  data<-d[order(d[,ID]),]

  return(data)

}

#example

results<-wide2long(data=sample_data,
                   ID="ID",
                   separator="_",
                   start=2,
                   end=15
                   )


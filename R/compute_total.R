compute_total <- function(data = sample_data,
                       left_hemisphere = "lh",
                       right_hemisphere = "rh",
                       seperator="_",
                       ID="ID",
                       hemisphere="prefix",
                       start ,
                       end  ) {


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
  data<-cbind(data,data4)

}

#example

data7<-compute_total(sample_data,
                  left_hemisphere = "lh",
                  right_hemisphere = "rh",
                  seperator="_",
                  ID="ID",
                  hemisphere="prefix",
                  start=2 ,
                  end=15 )

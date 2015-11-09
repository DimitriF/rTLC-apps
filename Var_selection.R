var_selection <- function(data,selection,Rf.min,Rf.max){
  channel <- seq(4)
  Rf <- round(seq(Rf.max,Rf.min,length.out=dim(data)[2]),3)
  data <- t(apply(data[,,channel],c(1),cbind))
  channel <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',channel))))
  new.rf <- c()
  for(i in channel){
    new.rf <- c(new.rf,paste(i,Rf,sep='_'))
  }
  colnames(data) <- new.rf
  
  selection$channel <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',selection$channel))))
  selected.rf <- c()
  for(i in seq(12)){
    if(selection[i,1] == T){
      # print(i)
      temp.rf <- paste(selection[i,2],
                       seq(selection[i,3],selection[i,4],by=0.001),
                       sep='_')
      selected.rf <- c(selected.rf,temp.rf)
    }
  }
  # print(selected.rf)
  data <- data[,colnames(data) %in% selected.rf]
  return(data)
}
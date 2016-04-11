#### License ####
#Copyright (C) {2014}  {Fichou Dimitri}
#{dimitrifichou@laposte.net}

#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
# any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License along
#with this program; if not, write to the Free Software Foundation, Inc.,
#51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#### rTLC ######

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
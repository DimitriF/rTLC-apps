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

list.of.packages <- c("jpeg","png",'caret','klaR','xlsx',"ChemometricsWithR","gplots","kohonen",'memoise',"devtools","chemometrics",
                      "ggplot2","abind","plyr",'dplyr',"prospectr","DiscriMiner","baseline","knitr","xtable",'rmarkdown',
                      "ptw","dtw",'shinyAce','shinydashboard','d3heatmap','randomForest','kernlab','ipred','extraTrees','evtree',
                      'htmltools','httpuv','mime','MASS','pls','rpart','e1071'
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://ftp.ussg.iu.edu/CRAN/")
lapply(list.of.packages, require, character.only=TRUE)

source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")

MAC.inverse=F
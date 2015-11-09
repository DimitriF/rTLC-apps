list.of.packages <- c("jpeg","png",'caret','klaR','xlsx',"ChemometricsWithR","gplots","kohonen","devtools","chemometrics",
                      "ggplot2","abind","plyr",'dplyr',"prospectr","DiscriMiner","baseline","knitr","xtable",
                      "ptw","dtw",'shinyAce','shinydashboard','d3heatmap','randomForest','kernlab','ipred','extraTrees','evtree'
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://ftp.ussg.iu.edu/CRAN/")
lapply(list.of.packages, require, character.only=TRUE)


MAC.inverse=F
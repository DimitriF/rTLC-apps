rTLC V.1.0
===========

This shiny application is made to perform multivariate analysis of HPTLC chromatograms.

Two options to use it:

Go to this url:
http://shinyapps.ernaehrung.uni-giessen.de/rtlc/

Install the application localy with the following instructions:

## Local Installation

Depending on your system, it could be difficult to install this software without basic knowledge of informatics.

Download and install R:
http://cran.r-project.org/

Install a fresh version of java:
https://www.java.com/

If you want to produce a pdf report, you will need to install LaTex:
http://latex-project.org/ftp.html

Install the dependencies by running this in the R console:

```r
install.packages(
    c('shiny',"jpeg","png",'tiff','caret','klaR','xlsx',"ChemometricsWithR","gplots","kohonen",'memoise',"devtools","chemometrics",
      "ggplot2","abind","plyr",'dplyr',"prospectr","DiscriMiner","baseline","knitr","xtable",'rmarkdown',
      "ptw","dtw",'shinyAce','shinydashboard','d3heatmap','randomForest','kernlab','ipred','extraTrees','evtree',
      'htmltools','httpuv','mime','MASS','pls','rpart','e1071','FBN',"threejs")
    )

```

Finally, launch the application by running this line:

```r
shiny::runGitHub(repo='rTLC-apps', username='DimitriF')
```




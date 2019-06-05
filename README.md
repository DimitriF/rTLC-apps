rTLC V.1.0
===========

This shiny application is made to perform multivariate analysis of HPTLC chromatograms.

Two options to use it:

Go to this url:
http://shinyapps.ernaehrung.uni-giessen.de/rtlc/

Install the application localy with the following instructions:

## Local Installation

Depending on your system, it could be difficult to install this software without basic knowledge of informatics.

1. Download and install R:
http://cran.r-project.org/


2. Install the dependencies by running this in the R console:

```r
install.packages(
    c('shiny',"jpeg","png",'tiff','caret','klaR','readxl',"ChemometricsWithR","gplots","kohonen",'memoise',"devtools","chemometrics",
      "ggplot2","abind","plyr",'dplyr',"prospectr","DiscriMiner","baseline","knitr","xtable",'rmarkdown',
      "ptw","dtw",'shinydashboard','d3heatmap','randomForest','kernlab','ipred','extraTrees','evtree',
      'htmltools','httpuv','mime','MASS','pls','rpart','e1071','FBN')
    )
```

3. Clone this repository (or download the zip file if you are on windows)

```
## From the command line
git clone git@github.com:DimitriF/rTLC-apps.git
```

4. In R launch the application using this command.

```
shiny::runApp('path/to/the/app')
```





rTLC
===========

This Shiny application is made to perform multivariate analysis of HPTLC pictures.

3 options to use it:

Go to this url:
http://shinyapps.ernaehrung.uni-giessen.de/rtlc/

Go to this url:
https://dimitrif.shinyapps.io/rTLC

Install the application localy with the following instructions:

## Local Installation

Note that this software has a lot of dependencies and that depending of your system, it could be difficult to install it without basic knowledge of informatic.

Download and install R:
http://cran.r-project.org/

Install a fresh version of java:
https://www.java.com/

If you want to produce pdf report, you'll need to install Latex:
http://latex-project.org/ftp.html

Install the dependencies by running this in the R console:

```r
install.packages(
    c('shiny',"jpeg","png",'tiff','caret','klaR','xlsx',"ChemometricsWithR","gplots","kohonen",'memoise',"devtools","chemometrics",
      "ggplot2","abind","plyr",'dplyr',"prospectr","DiscriMiner","baseline","knitr","xtable",'rmarkdown',
      "ptw","dtw",'shinyAce','shinydashboard','d3heatmap','randomForest','kernlab','ipred','extraTrees','evtree',
      'htmltools','httpuv','mime','MASS','pls','rpart','e1071','FBN')
    )

```

Finally, launch the application by running this line

```r
shiny::runGitHub(repo='rTLC-apps', username='DimitriF')
```




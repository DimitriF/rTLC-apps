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
#### EPTLC ######

library(shinyAce)
library(shinysky)
library(shinydashboard)
library(caret)
library(plyr)
library(dplyr)



body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'CollectChromExtract',
            fluidRow(
              box(title=NULL,collapsible=F,width=4,height=400,
                  selectizeInput('filedemouse','Data to use',
                                 choices=c('Your own data' = 'YourOwnData',
                                           'demo 1: Medicinal plants, 20 samples' = 'demo1',
                                           'demo 2: Medicinal plants, 80 samples'='demo2',
                                           'demo 3: Carbohydrates, 80 samples'='demo3',
                                           'demo 4: Propolis, 55 samples'='demo4'),
                                 selected='YourOwnData'),
                  conditionalPanel(condition = "input.filedemouse == 'YourOwnData'",
                                   fileInput('fileX', 'Choice of the batch'),
                                   selectizeInput("mono.Format.type","Select the format",choices=c("jpeg",'png'),selected="jpeg"),
                                   fileInput('filemonop', 'Choice of the picture(s) file',multiple=T)
                  )
                  ),
              box(title=NULL,collapsible = F,width=8,height=400,
                  uiOutput("select.image.redim.mono"),
                  imageOutput("image.redim.mono")
              ),
              box(title="Vertical Dimensions",collapsible = T,width=3,height=500,
                  numericInput('redim.height','Number of pixels to redimension',512),
                  numericInput('hauteur.mono','Height of the plate',10),
                  numericInput('Zf.mono','Retention front',7),
                  numericInput('dist.bas.mono','Bottom distance',0.8),
                  uiOutput('slider.subset.height')
              ),
              box(title="Horizontale Dimensions",collapsible = T,width=9,height=500,
                  tableOutput('TableDimension')
                  # hotable("TableDimension")
              )
            )
            
    ),
    tabItem(tabName = 'CollectBatch',
            tableOutput("table1")
            ),
    tabItem(tabName = 'CollectChromVisu',
            fluidRow(
              uiOutput('choice.band.mono.bef.1'),
              flowLayout(
                plotOutput("plot.v.mono.bef.1")
              ),
              uiOutput('choice.band.mono.bef.2'),
              flowLayout(
                plotOutput("plot.v.mono.bef.2")
              ),
              uiOutput('choice.band.mono.bef.tot'),
              plotOutput("plot.v.mono.bef.tot")
            )
            
    ),
    tabItem(tabName = 'CheckPoint',
            wellPanel(
              h4("Use this tab to save your data after the DataInput panel, you can download your data analysis progression here and upload it here in an other session"),
              textInput("checkpoint.1.name","Name of the file","EPTLC_checkpoint_1"),
              downloadButton("checkpoint.1.download"),
              checkboxInput("checkpoint.1.use","Use the upload file",F),
              h5("Note that by using this checkpoint, you will not be able to show the pictures in the report anymore, only the extracted chromatograms are saved"),
              fileInput("checkpoint.1.upload","Rdata file to upload")
                      )
            ),
    tabItem(tabName = 'PreProcess',
            sidebarLayout(
              sidebarPanel(
                numericInput("training.ratio","Ratio of data to keep in the training set, the other will be use for validation, only for prediction pruposes, let it to the 1 if you want to do exploratory statistics",1),
                h4("Here you can choose different preprocessing of the data before start the analysis."),
                tags$hr(),
                h4("Smoothing"),
                helpText(   a("Click Here for help with this smoothing feature",target="_blank",     
                              href="http://www.inside-r.org/node/206625")
                ),
                checkboxInput("Smoothing","Use a smoothing",F),
                numericInput("window.size","size of the windows",3,min=3,max=NA,step=2),
                numericInput("poly.order","polynomial order",1),
                numericInput("diff.order","differentiation order",0),
                tags$hr(),
                h4("Wrapping"),
                checkboxInput("warp","Use warping",F),
                conditionalPanel(condition="input.warp",
                                 selectizeInput("warpmethod","Warping method to use",choices=(c("ptw","VPdtw","other")),selected="ptw"),
                                 conditionalPanel(condition="input.warpmethod=='ptw'",
                                                  helpText(   a("Click Here for help with the PTW funtion",target="_blank",     
                                                                href="http://www.inside-r.org/packages/cran/ptw/docs/ptw")
                                                  ),
                                                  p("The best results I had was with respectively : ref=1, 'c(0,1,0)',individual,WCC,20 "),
                                                  numericInput("ptw.warp.ref","id of the reference",1),
                                                  textInput("ptw.init.coef","init.coef","c(0,1,0)"),
                                                  selectizeInput("ptw.warp.type","warp.type",choices=c("individual", "global"),selected="global"),
                                                  selectizeInput("ptw.optim.crit","optim.crit",choices=c("WCC", "RMS"),selected="WCC"),
                                                  numericInput("ptw.trwdth","trwdth",20)
                                 ),
                                 conditionalPanel(condition="input.warpmethod=='VPdtw'",
                                                  helpText(   a("Click Here for help with the VPdtw funtion",target="_blank",     
                                                                href="http://www.inside-r.org/packages/cran/VPdtw/docs/VPdtw")
                                                  ),
                                                  numericInput("VPdtw.warp.ref","id of the reference, 0 to use Reference.type",0),
                                                  selectizeInput("VPdtw.reference.type","warp.type",choices=c("random","median","mean","trimmed"),selected="global"),
                                                  numericInput("VPdtw.maxshift","maxshift",50)
                                 ),
                                 conditionalPanel(condition="input.warpmethod=='other'",
                                                  h4("use the tab 'Warping editor'"),
                                                  helpText(   a("Click Here for help with the PTW funtion",target="_blank",     
                                                                href="http://www.inside-r.org/packages/cran/ptw/docs/ptw")
                                                  ),
                                                  helpText(   a("Click Here for help with the DTW funtion",target="_blank",     
                                                                href="http://www.inside-r.org/packages/cran/dtw/docs/dtw")
                                                  ),
                                                  helpText(   a("Click Here for help with the VPdtw funtion",target="_blank",     
                                                                href="http://www.inside-r.org/packages/cran/VPdtw/docs/VPdtw")
                                                  )
                                 )
                ),
                h4("Standardisation"),
                helpText(   a("Click Here for help with the SNV feature",target="_blank",     
                              href="http://www.inside-r.org/packages/cran/prospectr/docs/standardNormalVariate")
                ),
                checkboxInput("SNV","Use Standard Normal Variate",F),
                helpText(   a("Click Here for help with the Autoscale feature",target="_blank",     
                              href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html")
                ),
                checkboxInput("Center","AutoScale the Chromato",F),
                tags$hr(),
                h4("Baseline"),
                checkboxInput("Baselinecorrection", "Baseline Correction", FALSE),
                helpText(   a("Click Here for help with the Baseline feature",target="_blank",     
                              href="http://cran.r-project.org/web/packages/baseline/baseline.pdf")
                ),
                conditionalPanel(condition="input.Baselinecorrection",
                                 selectizeInput("baseline", "type of baseline", choices=c("als","fillPeaks","irls","lowpass","medianWindow","modpolyfit","peakDetection","rfbaseline","rollingBall"),select=NULL),
                                 conditionalPanel(condition="input.baseline=='als'",
                                                  numericInput("lambda.1","lambda : 2nd derivative constraint",5),
                                                  numericInput("p","p : weighting of positive residuals",0.05),
                                                  numericInput("maxit.1","maxit : maximum number of iterations",20)
                                 ),
                                 conditionalPanel(condition="input.baseline=='fillPeaks'",
                                                  numericInput("lambda.2","lambda : 2nd derivative constraint for primary smoothing",6),
                                                  numericInput("hwi","hwi : half width of local windows",100),
                                                  numericInput("it","it : number of iterations in suppression loop",10),
                                                  numericInput("int","int : number of buckets to divide spectra into",200)
                                 ),
                                 conditionalPanel(condition="input.baseline=='irls'",
                                                  numericInput("lambda1","lambda1 : 2nd derivative constraint for primary smoothing",5),
                                                  numericInput("lambda2","lambda2 : 2nd derivative constraint for secondary smoothing",9),
                                                  numericInput("maxit.2","maxit : maximum number of iterations",200),
                                                  numericInput("wi","wi : weighting of positive residuals",0.05)
                                 ),
                                 conditionalPanel(condition="input.baseline=='lowpass'",
                                                  numericInput("steep","steep : steepness of filter curve",2),
                                                  numericInput("half","half : half way point of filter curve",5)
                                 ),
                                 conditionalPanel(condition="input.baseline=='medianWindow'",
                                                  numericInput("hwm","hwm : window half width for local medians",300),
                                                  numericInput("hws","hws : window half width for local smoothing",5),
                                                  checkboxInput("end","end : original endpoint handling",F)
                                 ),
                                 conditionalPanel(condition="input.baseline=='modpolyfit'",
                                                  numericInput("degree","degree : degree of polynomial",4),
                                                  numericInput("tol","tol : tolerance of difference between iterations",0.001),
                                                  numericInput("rep","rep : maximum number of iterations",100)
                                 ),
                                 conditionalPanel(condition="input.baseline=='peakDetection'",
                                                  numericInput("left","left : smallest window size for peak widths",30),
                                                  numericInput("right","right : largest window size for peak widths",300),
                                                  numericInput("lwin","lwin : Smallest window size for minimums and medians in peak removed spectra",50),
                                                  numericInput("rwin","rwin : Largest window size for minimums and medians in peak removed spectra",50),
                                                  numericInput("snminimum","snminimum : Minimum signal to noise ratio for accepting peaks",10)
                                 ),
                                 conditionalPanel(condition="input.baseline=='rollingBall'",
                                                  numericInput("wm","wm : Width of local window for minimization/maximization",200),
                                                  numericInput("ws","ws : Width of local window for smoothing",200)
                                 )
                )
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Chromatograms",
                           uiOutput('choice.band.mono.aft.1'),
                           plotOutput("plot.v.mono.aft.1"),
                           uiOutput('choice.band.mono.aft.2'),
                           plotOutput("plot.v.mono.aft.2")
                  ),
                  tabPanel("Chromatograms comparaison",
                           uiOutput('choice.band.mono.aft.tot'),
                           plotOutput("plot.v.mono.aft.tot")
                  ),
                  tabPanel("Warping editor",
                           aceEditor("editwarping","
                                     ## This is a comment
                                     ## the next values are the argument used later in the function
                                     ref <- '1' 
                                     init.coef <- 'c(0,1,0)' 
                                     warp.type <- 'individual' 
                                     optim.crit <- 'WCC' 
                                     trwdth <- 20 
                                     dataX <- dataX.mono.pre() ## this is the batch object
                                     
                                     ## Here we change the value of the object 'data', a 3D array containing our data, the each row is a chromatogram, each column is a variable (an RT), each layer is a channel : red/green/blue/grey
                                     ## We create 4 objects by calling the 'warped sample' of the resulting object of the function ptw apply on our data with arguments chosen before
                                     data.a<-ptw(ref = data[ref,,1],data[,,1],init.coef=eval(parse(text=init.coef)),warp.type=warp.type,optim.crit=optim.crit,trwdth=trwdth)$warped.sample
                                     data.b<-ptw(ref = data[ref,,2],data[,,2],init.coef=eval(parse(text=init.coef)),warp.type=warp.type,optim.crit=optim.crit,trwdth=trwdth)$warped.sample
                                     data.c<-ptw(ref = data[ref,,3],data[,,3],init.coef=eval(parse(text=init.coef)),warp.type=warp.type,optim.crit=optim.crit,trwdth=trwdth)$warped.sample
                                     data.d<-ptw(ref = data[ref,,4],data[,,4],init.coef=eval(parse(text=init.coef)),warp.type=warp.type,optim.crit=optim.crit,trwdth=trwdth)$warped.sample
                                     ## We then bind these 4 objects (one for each layer) together to reconstruct the data object
                                     data<-abind(data.a,data.b,data.c,data.d,along=3)
                                     ## We must rename the row of the data by the 'id' variable of the batch
                                     rownames(data)<-dataX$id
                                     ## here, we remove the empty value resulting of the warping
                                     data[is.na(data)] <- 0
                                     ",mode="r")
                           )
                           )
                           )
                           )
            ),
    tabItem(tabName = 'ExpPCA',
            sidebarLayout(
              sidebarPanel(
                helpText(   a("Click Here for help with the PCA feature",target="_blank",     
                              href="http://www.inside-r.org/node/98667")
                ),
                h4("Choice of the parameters for the PCA"),
                tags$hr(),
                h4("Variable of interest"),
                uiOutput("select.col.plot.pca"),
                uiOutput("select.shape.plot.pca"),
                uiOutput("select.label.plot.pca"),
                tags$hr(),
                h4("Channel of interest"),
                radioButtons("col.pca", "Channel to select for the PCA", choices=c("red","green","blue","grey","all"),select="red"),
                tags$hr(),
                h4("Other options"),
                selectizeInput('PCA.comp.a', '1st componant for the plot', choice=c("comp1","comp2","comp3","comp4","comp5","comp6","comp7","comp8","comp9","comp10"),select="comp1"),
                selectizeInput('PCA.comp.b', '2nd componant for the plot', choice=c("comp1","comp2","comp3","comp4","comp5","comp6","comp7","comp8","comp9","comp10"),select="comp2"),
                tableOutput("Table.dim.just.pca.label")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("PCA",
                           textInput("pca.plot.1.title","title of the graph","Principal Component Analysis"),
                           plotOutput("pca.plot.1",height="800px"),
                           dataTableOutput("pca.table.1"),
                           verbatimTextOutput("pca.summary.1")       
                  ),
                  tabPanel("Outlier",
                           checkboxGroupInput("comp.outlier.pca.1","Choies of the component of the PCA to use",choices=seq(10),selected=c(1,2)),
                           numericInput("quantile.outlier.pca.1","quantile to use for the cutoff",0.975),
                           plotOutput("quantile.outlier.pca.1"),
                           verbatimTextOutput("quantile.outlier.pca.2")
                  )
                )
              )
            )
    ),
    tabItem(tabName = 'ExpCluster',
            sidebarLayout(
              sidebarPanel(
                helpText(   a("Click Here for help with the Cluster feature",target="_blank",     
                              href="http://www.inside-r.org/r-doc/stats/dist")
                ),
                helpText(   a("and here",target="_blank",     
                              href="http://www.inside-r.org/r-doc/stats/hclust")
                ),
                h4("Ward Hierarchical Clustering parameters"),
                tags$hr(),
                h4("Variable of interest"),
                uiOutput("select.col.plot.cluster.1"),
                tags$hr(),
                h4("Channel of interest"),
                radioButtons("col.cluster.1","select the channel for the cluster",choices=c("red","green","blue","grey","all"),select="red"),
                tags$hr(),
                h4("Other options"),
                selectizeInput("method.dist.cluster.1","select the method for the distance",choices=c("euclidean", "maximum", "manhattan", "canberra", "binary","minkowski"),select="euclidean"),
                selectizeInput("method.clust.cluster.1","select the method for the cluster",choices=c("ward", "single", "complete", "average", "mcquitty", "median","centroid"),select="ward"),
                numericInput("cluster.nbr.1","number of cluster to cut into the tree",5)
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Hierarchical Clustering",
                           plotOutput("plot.cluster.1.1",height=800),
                           dataTableOutput("Cluster.table.1")
                  )
                ))
            )
            ),
    tabItem(tabName = 'Expheatmap',
            sidebarLayout(
              sidebarPanel(
                helpText(   a("Click Here for help with the Heatmap feature",target="_blank",     
                              href="http://www.inside-r.org/r-doc/stats/heatmap")
                ),
                h4("Heatmap"),
                tags$hr(),
                h4("Variable of interest"),
                uiOutput("select.col.plot.heatmap.1"),
                tags$hr(),
                h4("Channel of interest"),
                radioButtons("col.heatmap.1","select the channel for the cluster",choices=c("red","green","blue","grey","all"),select="red"),
                tags$hr(),
                h4("Other options"),
                p("None")
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Heatmap",
                           plotOutput("plot.heatmap.1",height=800)
                  )
                ))
            )
    ),
    tabItem(tabName = 'ExpDPE',
            wellPanel(
              tabsetPanel(
                tabPanel("Editor",
                         aceEditor("DPEeditor","
                                   ## This is a comment
                                   ## This feature allow you to directly enter R code to perform data analysis
                                   ## Two data are used here :
                                   ## data : the chromatograms, they are store in a 3d array 
                                   ## each row is an observation
                                   ## each column is a variable (a Retention time)
                                   ## each layer is a channel (1 for red, 2 for green, 3 for blue and 4 for grey)
                                   ## dataX : the batch file
                                   
                                   ## Uncomment the next line to plot the 1st chromatogram of the red channel
                                   # plot(data['1',,1],type='l')
                                   
                                   ## Uncomment the next line to plot the loading plot of the PCA model for the green channel
                                   # loadingplot(PCA(data[,,2]))
                                   
                                   ## Uncomment the next line to plot the score plot of the PCA model for the grey channel
                                   # scoreplot(PCA(data[,,2]))
                                   
                                   ## Uncomment the next line to plot the code of the kohonen som model for the green channel
                                   # plot(kohonen::som(data[,,2],somgrid(2,2,'hexagonal')),type='codes')
                                   
                                   ## Map of som kohonen
                                   # model <- kohonen::som(data[,,2],somgrid(2,2,'hexagonal'))
                                   # plot(model,type='mapping',labels=paste0(dataX$Drug,dataX$id,sep=' ; '))
                                   
                                   ## Uncomment the next line to plot the hist of the kmeans model for the green channel
                                   # hist(kmeans(data[,,2],center=3,iter.max=1,nstart=1,algorithm='Hartigan-Wong')$cluster)
                                   
                                   ## Uncomment the next lines to plot the hist of the kmeans model for the green channel
                                   model <- kmeans(data[,,2],center=3,iter.max=1,nstart=1,algorithm='Hartigan-Wong')
                                   Var.Dep <- 'Drug'
                                   data <- data.frame(box = model$cluster,Var.Dep=dataX[,Var.Dep])
                                   print(ggplot(data,aes(x=box,fill=Var.Dep))+geom_bar())
                                   
                                   ## svm try
                                   #reduce <- PCA(data[,,4])$scores[,1:10]
                                   #colnames(reduce) <- paste0('PC',seq(10))
                                   #colnames(reduce)
                                   #model <- svm(x=reduce,y=factor(dataX$drug),type='C-classification')
                                   #print(summary(model))
                                   #table.conf <- table(dataX$drug,predict(model,newdata=reduce))
                                   #diag(table.conf) <- 0
                                   #print(sum(table.conf)/nrow(dataX))
                                   #table(dataX$drug,predict(model,newdata=data[,,4]))
                                   
                                   ## plsDA try
                                   #model <- plsDA(data[,,3],dataX$drug,autosel=F,comps=5)
                                   #plot(model)
                                   #print(model$error_rate)
                                   #print(model$confusion)
                                   ",mode="r")
                                                               ),
                                            tabPanel("Plot",
                                                     imageOutput("DPEplot")
                                                     ),
                                            tabPanel("Print",
                                                     verbatimTextOutput("DPEprint")
                                            )
                                            )
                                          )
    ),
    tabItem(tabName = 'Pred',
            h4('incomming')
    ),
    tabItem(tabName = 'Report',
            checkboxInput("mono.knitr.file.name", "Print the name of the file", TRUE),
            checkboxInput("monoknitrpicture", "Print the analysis picture(s)", TRUE),
            checkboxInput("mono.knitr.batch.simple", "Print the batch", TRUE),
            selectizeInput("mono.knitr.plot.brut","Print the chromatograms before process",choices=c("None","2","all"),selected="None"),
            checkboxInput("mono.knitr.preprocess","Print the summary of the preprocess",F),
            selectizeInput("mono.knitr.plot.net","Print the chromatograms after process",choices=c("None","2","all"),selected="None"),
            #                                         checkboxInput("mono.knitr.preprocess.input", "Print the Preprocessing input", FALSE),
            #                                         checkboxInput("mono.knitr.pca.input", "Print the pca input", FALSE),
            checkboxInput("mono.knitr.pca.plot", "Print the pca plot", FALSE),
            #                                         checkboxInput("mono.knitr.cluster.input", "Print the cluster input", FALSE),
            checkboxInput("mono.knitr.cluster.plot", "Print the cluster plot", FALSE),
            checkboxInput("mono.knitr.heatmap.plot", "Print the heatmap plot", FALSE),
            downloadButton('mono.knitr.download','Download the report')
    )
  ),
  tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
  tags$head(tags$style(HTML(".shiny-output-error-validation {color: green;}"))),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
  tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                       border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Data Input',
             menuSubItem('Chromatogram Extraction',tabName = "CollectChromExtract"),
             menuSubItem('Batch',tabName = "CollectBatch"),
             menuSubItem('Chromatogram Visualisation',tabName = "CollectChromVisu")
             ),
    menuSubItem('CheckPoint',tabName='CheckPoint'),
    menuSubItem('Preprocessing',tabName='PreProcess'),
    menuItem('Exploratory Statistic',
             menuSubItem('PCA',tabName = "ExpPCA"),
             menuSubItem('cluster',tabName = "ExpCluster"),
             menuSubItem('heatmap',tabName = "Expheatmap"),
             menuSubItem('K means',tabName = "ExpKmeans"),
             menuSubItem('DPE',tabName = "ExpPCA")
    ),
    menuItem('Predictive Statistic',
             menuSubItem('Caret',tabName = "Pred")
    ),
    menuSubItem('Report Output',tabName='Report')
  )
)

dashboardPage(title = "EPTLC",
              dashboardHeader(title = "EPTLC"),
              sidebar,
              body
)

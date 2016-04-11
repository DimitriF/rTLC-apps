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


require("jpeg");require("png");require('caret');require('klaR');require('xlsx');
require("ChemometricsWithR");require("gplots");require("kohonen");require("devtools");
require("chemometrics");require("ggplot2");require("abind");require("plyr");require('dplyr');
require("prospectr");require("DiscriMiner");require("baseline");require("knitr");
require("xtable");require("ptw");require("dtw");
require('randomForest');require('kernlab');require('ipred');
require('extraTrees');require('evtree')

# require('shinyRGL');require('rgl')

require('shinyAce');require('shinydashboard');require('d3heatmap');



shinyUI(navbarPage(title="rTLC",
                   tabPanel("Data input",
                            tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
                            tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
                            tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
                            tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}")),
                            fluidRow(
                              column(width=3,
                                selectizeInput('filedemouse','Data to use',
                                               choices=c('Your own data' = 'YourOwnData',
                                                         'demo 1: Medicinal plants, 20 samples' = 'demo1',
#                                                          'demo 2: Medicinal plants, 80 samples'='demo2',
#                                                          'demo 3: Carbohydrates, 80 samples'='demo3',
#                                                          'demo 4: Propolis, 55 samples'='demo4',
                                                         'Saved data' = 'checkpoint',
                                                         'Predict data - QC'='QC'),
                                               selected='demo1'),
                                conditionalPanel(condition = "input.filedemouse == 'YourOwnData' | input.filedemouse == 'QC'",
                                                 tags$hr(),
                                                 h4("Load"),
                                                 fileInput('fileX', 'Choice of the batch '),
                                                 selectizeInput("mono.Format.type","Select the format",choices=c("jpeg","png","tiff"),selected="jpeg"),
                                                 fileInput('filemonop', 'Choice of the plate(s) file',multiple=T)
                                ),
                                conditionalPanel(condition = "input.filedemouse == 'QC'",
                                                 tags$hr(),
                                                 fileInput('Pred.upload.model','Model file')
                                ),
                                conditionalPanel(condition = "input.filedemouse == 'checkpoint'",
                                                 tags$hr(),
                                                 fileInput("checkpoint.1.upload","Rdata file to upload")
                                ),
                               hr(),
                               textInput('checkpoint.1.download.text','filename','rTLC_checkpoint_1'),
                               downloadButton("checkpoint.1.download",'Save Chromatograms'),
                               tags$hr(),
                               textInput('checkpoint.1.download.zip.text','filename','rTLC_zip_export'),
                               downloadButton("checkpoint.1.download.zip",'Save zip file with csv')
                              ),
                              column(width=9,
                                     wellPanel(
                                       tabsetPanel(
                                         tabPanel("Chromatograms Extraction",
                                                  fluidRow(
                                                    shinydashboard::box(title=NULL,collapsible = F,width=8,height=350,
                                                        uiOutput("select.image.redim.mono"),
                                                        imageOutput("image.redim.mono")
                                                    ),
                                                    shinydashboard::box(title="Vertical Dimensions (mm)",collapsible = F,width=4,height=350,
                                                        tableOutput('TableDimensionVerticale')
                                                    ),
                                                    shinydashboard::box(title="Horizontal Dimensions (mm)",collapsible = F,width=12,height=500,
                                                        tableOutput('TableDimension'),
                                                        radioButtons('TableDimensionConvention','Convention to use in the Horizontal table',choices=c('Linomat','ATS-4'),selected='ATS-4'),
                                                        textInput('TableDimensionSave.text','filename','TableDimensionSave'),
                                                        downloadButton('TableDimensionSave','Save the Dimension table'),
                                                        fileInput("TableDimensionUpload","Upload the saved table"),
                                                        plotOutput('TableDimensionPlot')

                                                        # hotable("TableDimension")
                                                    )
                                                  )
                                         ),
                                         tabPanel("batch",
                                                  column(3,hr(),uiOutput('batch.Truc.mono'),h4('Column Filter: Keep only selected, if none, keep all.'),uiOutput('batch.filter')),
                                                  column(9,hr(),tableOutput("table1"))
                                         ),
                                         tabPanel("Chromatograms",
                                                  uiOutput('choice.band.mono.bef.1'),
                                                  flowLayout(
                                                    plotOutput("plot.v.mono.bef.1")
                                                  ),
                                                  uiOutput('choice.band.mono.bef.2'),
                                                  flowLayout(
                                                    plotOutput("plot.v.mono.bef.2")
                                                  )
                                         ),
                                         tabPanel("Band Comparison",
                                                  uiOutput("choice.band.m.comp.1"),
                                                  imageOutput("image.comparaison.1",height=500)
                                         ),
                                         tabPanel("Chromatograms comparison",
                                                  uiOutput('choice.band.mono.bef.tot'),
                                                  plotOutput("plot.v.mono.bef.tot")
                                         ),
                                         tabPanel('Image reconstruction',
                                                  uiOutput("select.image.reconstruct"),
                                                  plotOutput('image.reconstruct')
                                                  ),
                                         tabPanel("Prediction (QC only)",
                                                  tableOutput("table2")
                                         )

                                       )
                                     )
                                     )
                            )
                   ),
                   tabPanel("Data preprocessing",
                            sidebarLayout(
                              sidebarPanel(
                                h4("Here you can choose different data preprocessing before starting the analysis."),
                                tags$hr(),
                                selectizeInput('Preprocess.order','Preprocess choice (order is important)',
                                               choices=c('medianFilter','gammaCorrection','Smoothing','Baseline.correction','Warping','Standard.Normal.Variate',
                                                         'Mean.centering','Autoscaling'),
                                               selected='',multiple=T)

                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Preprocess Details",
                                           column(3,
                                                  h4("Median Filtering"),
                                                  numericInput('preprocess.medianfilter','The half-size of the filtering window',3),
                                                  h4('Gamma Correction'),
                                                  numericInput('preprocess.gammacorrection','Value',2),
                                                  h4("Smoothing"),
                                                  helpText(   a("Click Here for help with this smoothing feature",target="_blank",
                                                                href="http://www.inside-r.org/node/206625")
                                                  ),
                                                  helpText(   a("Wikipedia link",target="_blank",
                                                                href="https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter")
                                                  ),
                                                  numericInput("window.size","size of the windows",3,min=3,max=NA,step=2),
                                                  numericInput("poly.order","polynomial order",1),
                                                  numericInput("diff.order","differentiation order",0)
                                                  ),
                                           column(3,
                                                  h4("Baseline"),
                                                  helpText(   a("Click Here for help with the Baseline feature",target="_blank",
                                                                href="http://cran.r-project.org/web/packages/baseline/baseline.pdf")
                                                  ),
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
                                           ),
                                           column(3,
                                                  h4("Warping"),
                                                  helpText(   a("Wikipedia link about peak alignment",target="_blank",
                                                                href="https://en.wikipedia.org/wiki/Dynamic_time_warping")
                                                  ),
                                                  selectizeInput("warpmethod","Warping method to use",choices=(c("ptw",'dtw')),selected="ptw"),
                                                  conditionalPanel(condition="input.warpmethod=='ptw'",
                                                                   helpText(   a("Click Here for help with the PTW funtion",target="_blank",
                                                                                 href="http://www.inside-r.org/packages/cran/ptw/docs/ptw")
                                                                   ),
                                                                   #p("The best results I had was with respectively : ref=1, 'c(0,1,0)',individual,WCC,20 "),
                                                                   uiOutput('ptw.warp.ref')
                                                                   # numericInput("ptw.warp.ref","id of the reference",1)#,
                                                                   #textInput("ptw.init.coef","init.coef","c(0,1,0)"),
                                                                   #selectizeInput("ptw.warp.type","warp.type",choices=c("individual", "global"),selected="global"),
                                                                   #selectizeInput("ptw.optim.crit","optim.crit",choices=c("WCC", "RMS"),selected="WCC"),
                                                                   #numericInput("ptw.trwdth","trwdth",20)
                                                  ),
                                                  conditionalPanel(condition="input.warpmethod=='dtw'",
                                                                   helpText(   a("Click Here for help with the DTW funtion",target="_blank",
                                                                                 href="http://www.inside-r.org/packages/cran/dtw/docs/dtw")
                                                                   ),
                                                                   uiOutput('ptw.warp.ref.bis'),
                                                                   # numericInput("ptw.warp.ref","id of the reference",1),
                                                                   checkboxInput('dtw.split','Do the alignment on the 4 channels separatly',F)
                                                  )
                                                  ),
                                           column(3,
                                                  h4("Standardisation"),
                                                  helpText(   a("Click Here for help with the SNV feature",target="_blank",
                                                                href="http://www.inside-r.org/packages/cran/prospectr/docs/standardNormalVariate")
                                                  ),
                                                  helpText(   a("Click Here for help with the Autoscale feature",target="_blank",
                                                                href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html")
                                                  )
                                                  )
                                           ),
                                  tabPanel("Chromatograms",
                                           uiOutput('choice.band.mono.aft.1'),
                                           plotOutput("plot.v.mono.aft.1"),
                                           uiOutput('choice.band.mono.aft.2'),
                                           plotOutput("plot.v.mono.aft.2")
                                  ),
                                  tabPanel("Chromatograms comparison",
                                           uiOutput('choice.band.mono.aft.tot'),
                                           plotOutput("plot.v.mono.aft.tot")
                                  )
                                )
                              )
                            )
                   ),
                   tabPanel('Variables selection',
                            column(6,
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_1", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_1", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=1)),
                                     column(9,uiOutput('VS_slider_1'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_2", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_2", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=2)),
                                     column(9,uiOutput('VS_slider_2'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_3", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_3", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=3)),
                                     column(9,uiOutput('VS_slider_3'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_4", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_4", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_4'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_5", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_5", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=1)),
                                     column(9,uiOutput('VS_slider_5'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_6", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_6", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=2)),
                                     column(9,uiOutput('VS_slider_6'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_7", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_7", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=3)),
                                     column(9,uiOutput('VS_slider_7'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_8", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_8", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_8'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_9", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_9", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=1)),
                                     column(9,uiOutput('VS_slider_9'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_10", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_10", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=2)),
                                     column(9,uiOutput('VS_slider_10'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_11", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_11", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=3)),
                                     column(9,uiOutput('VS_slider_11'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_12", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_12", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_12'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_13", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_13", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_13'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_14", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_14", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_14'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_15", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_15", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_15'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_16", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_16", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_16'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_17", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_17", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_17'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_18", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_18", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_18'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_19", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_19", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_19'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_20", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_20", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'grey'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_20'))
                                   )

                            ),
                            column(6,
                                   plotOutput('VS_plot')
                                   )

                            ),
                   navbarMenu("Exploratory Statistics",
                              tabPanel("PCA",
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
                                           # h4("Channel of interest"),
                                           # checkboxGroupInput("col.pca", "Channel to select for the PCA", choices=c("red"=1,"green"=2,"blue"=3,"grey"=4),select=seq(4)),
                                           tags$hr(),
                                           h4("Other options"),
                                           selectizeInput('PCA.comp.a', '1st componant for the plot', choice=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10"),select="PC1"),
                                           selectizeInput('PCA.comp.b', '2nd componant for the plot', choice=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10"),select="PC2"),
                                           checkboxInput('pca.ellipse','Plot the ellipse according to the color',F),
                                           numericInput('pca.ellipse.level','Level to calculate the ellipse',0.95),
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
                                             tabPanel("Loading Plot",
                                                      radioButtons('pca.loading.choice','Componant',choices=seq(10),selected=1),
                                                      p('The RF value here are wrong, all the channels were merged during the variables selection so RF do not make sense anymore, except if only one full channel is used'),
                                                      plotOutput("pca.loading"),
                                                      checkboxInput('pcaloadinglocalmaxima','pick peak for local maxima',F),
                                                      numericInput('pca.loading.local.maxima.span','Neighbourhood, used to define local maxima',20),
                                                      checkboxInput('pcaloadinglocalminima','pick peak for local minima',F),
                                                      numericInput('pca.loading.local.minima.span','Neighbourhood, used to define local minima',20),
                                                      # conditionalPanel(condition="input.pcaloadinglocalmaxima==T",
                                                                       verbatimTextOutput('pca.loading.local.maxima'),
                                                      # ),
                                                      # conditionalPanel(condition="input.pcaloadinglocalminima==T",
                                                                       verbatimTextOutput('pca.loading.local.minima')
                                                      # )
                                                      ),
#                                              tabPanel("PCA3D",
#                                                       webGLOutput("myWebGL.1",height="600px")
#                                              ),
                                             tabPanel("Outlier",
                                                      checkboxGroupInput("comp.outlier.pca.1","Choies of the component of the PCA to use",choices=seq(10),selected=c(1,2)),
                                                      numericInput("quantile.outlier.pca.1","quantile to use for the cutoff",0.975),
                                                      plotOutput("quantile.outlier.pca.1"),
                                                      verbatimTextOutput("quantile.outlier.pca.2")
                                             ),
                                              tabPanel('score and loading together',
                                                       p('Note that for this graphic, the variable selection is by passed, could evolve in the futur though'),
                                                       uiOutput('VS_slider_score.loading'),
                                                       uiOutput('pca.plot.score.loading.title'),
                                                       plotOutput('pca.plot.score.loading',height='800px')
                                                       )
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Cluster",  ####### cluster #######
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
                                           # h4("Channel of interest"),
                                           # checkboxGroupInput("col.cluster.1","select the channel for the cluster",choices=c("red"=1,"green"=2,"blue"=3,"grey"=4),select=seq(4)),
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
                              tabPanel("Heatmap",  ####### Heatmap #######
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
                                           # h4("Channel of interest"),
                                           # checkboxGroupInput("col.heatmap.1","select the channel for the cluster",choices=c("red"=1,"green"=2,"blue"=3,"grey"=4),select=seq(4)),
                                           tags$hr(),
                                           h4("Other options"),
                                           p("None")
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Heatmap",
                                                      plotOutput("plot.heatmap.1",height=800)
                                             ),
                                             tabPanel('Interactive Heatmap',
                                                      d3heatmapOutput('plot.heatmap.2',height=1000)
                                             )
                                           ))
                                       )
                              ),
                              tabPanel("DPE",
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
                                       )
                                       ),
                   tabPanel("Predictive statisitcs",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           sliderInput('Train.partition','Part of data to train with (the preprocess will be rerun if changed)',min=0,max=1,value = 0.75),
                                           uiOutput('Train.model.algo'),
                                           uiOutput('Train.model.algo.wiki'),
                                           uiOutput("Train.column"),
                                           radioButtons('Trainproblem','Type',choices=c('classification','regression'),selected='classification'),
                                           # checkboxGroupInput("col.Pred","Choice of the channel(s)",choices=c("red"=1,"green"=2,"blue"=3,"grey"=4),select=seq(4)),
                                           hr(),
                                           div(class="btn btn-default action-button shiny-bound-input",actionButton('Train.go','Train')),
                                           hr(),
                                           uiOutput('Train.down.model.text'),
                                           downloadButton("Train.down.model","Download model")
                              ),
                              mainPanel(width=9,
                                tabsetPanel(
                                  tabPanel("Tuning Options",
                                           fluidRow(
                                             box(title='General Options',width=3,collapsible = F,
                                                 helpText(   a("Click Here to learn about the Validation techniques",target="_blank",
                                                               href="https://en.wikipedia.org/wiki/Cross-validation_%28statistics%29")
                                                 ),
                                                 selectizeInput('Train.control.method','Validation method for the tunning',
                                                                choices=c('boot', 'repeatedcv', 'LOOCV'),
                                                                selected='repeatedcv'),
                                                 uiOutput('Train.metric'),
                                                 numericInput('Train.tunning.CV','Either the number of folds or number of resampling iterations',5),
                                                 numericInput('Train.tunning.repeat','For repeated k-fold cross-validation only: the number of complete sets of folds to compute',1)
                                                 ),
                                             box(title='Grid',width=9,collapsible = F,
                                                 numericInput('Train.tunning.length','Tuning length',10),
                                                 tableOutput("Train.model.grid.edit")
                                             )
                                           )
                                  ),
                                  tabPanel('Validation metrics',
                                           radioButtons('TrainValidMetricsUse','Data to use',choices=c('Cross-validation data','Training data','Test data'),selected='Test data'),
                                           conditionalPanel(condition = "input.Trainproblem == 'classification'",
                                                            tableOutput('TrainValidMetricsClassTable'),
                                                            verbatimTextOutput('TrainValidMetricsClassPrint')
                                           ),
                                           conditionalPanel(condition = "input.Trainproblem == 'regression'",
                                                            plotOutput('TrainValidMetricsRegPlot')
                                           )

                                           ),
                                  tabPanel("Prediction table",
                                           dataTableOutput('Train.pred.table')
                                           ),
                                  tabPanel("Algorythm information",
                                           verbatimTextOutput('Train.model.algo.info')
                                           ),
                                  tabPanel('Model Summary',
                                           verbatimTextOutput('Train.validation')
                                           ),
                                  tabPanel('Tuning Curve',
                                           plotOutput('Train.tunning.plot')
                                           ),
                                  tabPanel('DPE',
                                           tabsetPanel(
                                             tabPanel("Editor.pred",
                                                      aceEditor("DPEeditorpred","model <- Train.model() \nInd <- Train.Ind()\nDep <- Train.Dep()",mode="r")
                                             ),
                                             tabPanel("Plot.pred",
                                                      imageOutput("DPE.pred.plot")
                                             ),
                                             tabPanel("Print.pred",
                                                      verbatimTextOutput("DPE.pred.print")
                                             ),
                                             tabPanel('Template',
                                                      includeMarkdown('Prediction-template.md')
                                                      )
                                           )
                                  )
                                  )
                                )
                            )
                            ),
                   tabPanel("Report Output",
                            column(2,h4('Data Input'),
                                   checkboxInput("mono.knitr.file.name", "Print the name of the file", TRUE),
                                   checkboxInput("monoknitrpicture", "Print the analysis picture(s)", TRUE),
                                   checkboxInput("mono.knitr.batch.simple", "Print the batch", TRUE),
                                   checkboxInput("mono.knitr.batch.pred", "Print the batch with the prediction (QC only)", FALSE),
                                   selectizeInput("mono.knitr.plot.brut","Print the chromatograms before process",choices=c("None","2","all"),selected="None"),
                                   checkboxInput('mono.knitr.band.comp','Print the band comparison plot',F)
                                   ),
                            column(2,h4('Data Preprocessing and Variable Selection'),
                                   checkboxInput("mono.knitr.preprocess","Print the summary of the preprocess",F),
                                   selectizeInput("mono.knitr.plot.net","Print the chromatograms after process",choices=c("None","2","all"),selected="None"),
                                   checkboxInput("mono.knitr.var.select","Print the Variable.selection table",F)
                                   ),
                            column(2,h4('Exploratory Statistics'),
                                   checkboxInput("mono.knitr.pca.plot", "Print the pca plot", FALSE),
                                   checkboxInput('mono.knitr.pca.score.loading','Print the plot with score and loadings',F),
                                   checkboxInput('mono.knitr.pca.score.loading.split','Print the plot with score and loadings splited',F),
                                   checkboxInput("mono.knitr.cluster.plot", "Print the cluster plot", FALSE),
                                   checkboxInput("mono.knitr.heatmap.plot", "Print the heatmap plot", FALSE)
                                   ),
                            column(2,h4('Predictive Statistics'),
                                   checkboxInput('mono.knitr.prediction.summary.model','Print model summary',F),
                                   checkboxGroupInput('mono.knitr.prediction.validation','Print the validation results for ',choices=c('Cross-validation data','Training data','Test data'))
                                   ),
                            column(4,h4('Download'),
                                   textInput('mono.knitr.download.text','filename','rTLC-report'),
                                   # downloadButton('mono.knitr.download','Download the report'),
                                   radioButtons('reportformat', 'Document format', c('PDF', 'HTML', 'Word'),
                                                inline = TRUE),
                                   downloadButton('downloadReport')
                                   )
                   ),
#                    tabPanel('Batch Creator',
#                             sidebarLayout(
#                               sidebarPanel(
#                                 fileInput('batch.creator.file','Upload a precedent file to get a template'),
#                                 uiOutput('batch.creator.plate')
#                               ),
#                               mainPanel(
#                                 tabsetPanel(
#                                   tabPanel('test',
#                                     p('incoming')
#                                   )
#                                 )
#                               )
#                             )
#                             ),
                   tabPanel('About/help',
                            wellPanel(
                              tabsetPanel(
                                tabPanel('ReadMe',
                                         includeMarkdown("README.md")
                                         ),
                                tabPanel('R packages and Session Info',
                                          verbatimTextOutput('sessionInfo')
                                          ),
                                tabPanel('License',
                                         includeMarkdown('LICENSE.md')
                                         ),
                                tabPanel('Contact',
                                         h5('for information and specific help, contact:'),
                                         hr(),
                                         HTML('<a href="mailto:dimitrifichou@gmail.com">Dimitri Fichou</a> '),
                                         hr(),
                                         HTML('<a href="mailto:p.ristivojevic@gmail.com">Petar Ristivojevic</a> '),
                                         hr(),
                                         p('Dimitri Fichou and Dr Petar Ristivojević contributed to this application.
                                            Both of them discussed about design, visualisation tools and multivariate analysis.
                                            Mr Fichou designed all features of the application and Dr Petar Ristivojević contributed by ideas and feed backs.'),
                                         hr(''),
                                         p('This application was supported by Pr. Gertrud Morlock and her team at the Justus Liebig University of Giessen and is generously hosted on the university server.')
                                         ),
                                tabPanel('Manual',
                                         downloadButton('manual.pdf','Download the pdf manual')
                                         ),
                                tabPanel('Global Pipeline',
                                         imageOutput('help.global.pipeline')
                                         ),
                                tabPanel('Prediction Pipeline',
                                         imageOutput('help.predict.pipeline')
                                         )
                              )
                            )
                            )

                     ))

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
require("threejs")

# require('shinyRGL');require('rgl')

require('shinyAce');require('shinydashboard');require('d3heatmap');



shinyUI(navbarPage(title="rTLC V.1.0",
                   tabPanel("Data input",
                            tags$head(tags$style(type="text/css", "tfoot {display: table-header-group}")),
                            tags$head(tags$style(HTML(".shiny-output-error-validation {color: red;font-size: 24px}"))),
                            tags$head(tags$style(type="text/css", ".shiny-progress .progress {position: absolute;width: 100%;top: 100px;height: 10px;margin: 0px;}")),
                            tags$head(tags$style(type="text/css", ".shiny-progress .progress-text {position: absolute;border-style: solid;
                                                 border-width: 2px;right: 10px;height: 36px;width: 50%;background-color: #EEF8FF;margin: 0px;padding: 2px 3px;opacity: 1;}")),
                            fluidRow(
                              column(width=3,
                                radioButtons('filedemouse','Data to use',
                                               choices=c('Your own data' = 'YourOwnData',
                                                         'demo 1: Medicinal plants, 20 samples' = 'demo1',
                                                         'demo 2: Propolis dataset'='demoPropolis',
                                                          'demo 3: Medicinal plants, 80 samples'='demo2',
#                                                          'demo 3: Carbohydrates, 80 samples'='demo3',
                                                         # 'demo 4: Propolis, 55 samples'='demo4',
                                                         'Saved data' = 'checkpoint',
                                                         'Predict data - QC'='QC'),
                                               selected='demo1'),
                                conditionalPanel(condition = "input.filedemouse == 'YourOwnData' | input.filedemouse == 'QC'",
                                                 tags$hr(),
                                                 h4("Upload"),
                                                 fileInput('fileX', 'Choice of the batch '),
                                                 selectizeInput("mono.Format.type","Select the image format",choices=c("jpeg","png","tiff"),selected="jpeg"),
                                                 fileInput('filemonop', 'Choice of the plate file(s)',multiple=T)
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
                               textInput('checkpoint.1.download.text','Filename','rTLC_checkpoint_1'),
                               downloadButton("checkpoint.1.download",'Save Chromatograms'),
                               tags$hr(),
                               textInput('checkpoint.1.download.zip.text','Filename','rTLC_zip_export'),
                               downloadButton("checkpoint.1.download.zip",'Save zip file with csv')
                              ),
                              column(width=9,
                                     wellPanel(
                                       tabsetPanel(
                                         tabPanel("Chromatogram extraction",
                                                  fluidRow(
                                                    shinydashboard::box(title=NULL,collapsible = F,width=8,height=350,
                                                        uiOutput("select.image.redim.mono"),
                                                        imageOutput("image.redim.mono")
                                                    ),
                                                    shinydashboard::box(title="Vertical dimensions (mm)",collapsible = F,width=4,height=350,
                                                        tableOutput('TableDimensionVerticale')
                                                    ),
                                                    shinydashboard::box(title="Horizontal dimensions (mm)",collapsible = F,width=12,height=500,
                                                        tableOutput('TableDimension'),
                                                        radioButtons('TableDimensionConvention','Convention how to use the horizontal table',choices=c("Calculation from the exterior of the band"='Linomat',
                                                                                                                                                       "Calculation from the middle of the band"='ATS-4' ),selected='ATS-4'),
                                                        textInput('TableDimensionSave.text','Filename','TableDimensionSave'),
                                                        downloadButton('TableDimensionSave','Save the Dimension table'),
                                                        fileInput("TableDimensionUpload","Upload the saved table"),
                                                        plotOutput('TableDimensionPlot')
                                                    )
                                                  )
                                         ),
                                         tabPanel("Batch",
                                                  column(3,
                                                         hr(),
                                                         uiOutput('batch.Truc.mono'),
                                                         h4('Column filter: Keep only selected, if none, keep all.'),
                                                         uiOutput('batch.filter')),
                                                  column(9,hr(),tableOutput("table1"))
                                         ),
                                         tabPanel("Track plot",
                                                  h5("Use this tab to compare two tracks, use the pdf report to access all of them."),
                                                  uiOutput('choice.band.mono.bef.1'),
                                                  flowLayout(
                                                    plotOutput("plot.v.mono.bef.1")
                                                  ),
                                                  uiOutput('choice.band.mono.bef.2'),
                                                  flowLayout(
                                                    plotOutput("plot.v.mono.bef.2")
                                                  )
                                         ),
                                         tabPanel("Chromatogram comparison",
                                                  uiOutput("choice.band.m.comp.1"),
                                                  imageOutput("image.comparaison.1",height=500)
                                         ),
                                         tabPanel("Densitogram comparison",
                                                  uiOutput('choice.band.mono.bef.tot'),
                                                  plotOutput("plot.v.mono.bef.tot")
                                         ),
                                         tabPanel('Image reconstruction',
                                                  h5("This tab shows how the densitograms are extracted from the chromatograms."),
                                                  uiOutput("select.image.reconstruct"),
                                                  plotOutput('image.reconstruct')
                                                  ),
                                         tabPanel("Prediction (QC)",
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
                                sliderInput('Train.partition','Proportion of training data (needed for predictive statistics)',min=0,max=1,value = 0.75),
                                 tags$hr(),
                                selectizeInput('Preprocess.order','Selcetion of preprocessing algorithms (order is important)',
                                               choices=c("Median filter" = 'medianFilter',"Gamma correction" = 'gammaCorrection','Smoothing' = 'Smoothing',
                                                         'Baseline correction' = 'Baseline.correction','Warping' = 'Warping','Standard normal variate' ='Standard.Normal.Variate',
                                                         'Mean centering' = 'Mean.centering','Autoscaling' = 'Autoscaling'),
                                               selected='',multiple=T)

                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Preprocess Details",
                                           column(3,
                                                  h4("Median filter"),
                                                  numericInput('preprocess.medianfilter','Half-size of the filtering window',3),
                                                  h4('Gamma correction'),
                                                  numericInput('preprocess.gammacorrection','Value',2),
                                                  h4("Smoothing"),
                                                  helpText(   a("Help for this feature",target="_blank",
                                                                href="https://www.rdocumentation.org/packages/prospectr/versions/0.1.3/topics/savitzkyGolay?")
                                                  ),
                                                  helpText(   a("Wikipedia link",target="_blank",
                                                                href="https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter")
                                                  ),
                                                  numericInput("window.size","Size of the window",3,min=3,max=NA,step=2),
                                                  numericInput("poly.order","Polynomial order",1),
                                                  numericInput("diff.order","Differentiation order",0)
                                                  ),
                                           column(3,
                                                  h4("Baseline"),
                                                  helpText(   a("Help for this feature",target="_blank",
                                                                href="http://cran.r-project.org/web/packages/baseline/baseline.pdf")
                                                  ),
                                                  selectizeInput("baseline", "Type of baseline", choices=c("als","fillPeaks","irls","lowpass","medianWindow","modpolyfit","peakDetection","rfbaseline","rollingBall"),select=NULL),
                                                  conditionalPanel(condition="input.baseline=='als'",
                                                                   numericInput("lambda.1","lambda: 2nd derivative constraint",5),
                                                                   numericInput("p","p: weighting of positive residuals",0.05),
                                                                   numericInput("maxit.1","maxit: maximum number of iterations",20)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='fillPeaks'",
                                                                   numericInput("lambda.2","lambda: 2nd derivative constraint for primary smoothing",6),
                                                                   numericInput("hwi","hwi: half width of local windows",100),
                                                                   numericInput("it","it: number of iterations in suppression loop",10),
                                                                   numericInput("int","int: number of buckets to divide spectra into",200)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='irls'",
                                                                   numericInput("lambda1","lambda1: 2nd derivative constraint for primary smoothing",5),
                                                                   numericInput("lambda2","lambda2: 2nd derivative constraint for secondary smoothing",9),
                                                                   numericInput("maxit.2","maxit: maximum number of iterations",200),
                                                                   numericInput("wi","wi: weighting of positive residuals",0.05)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='lowpass'",
                                                                   numericInput("steep","steep: steepness of filter curve",2),
                                                                   numericInput("half","half: half way point of filter curve",5)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='medianWindow'",
                                                                   numericInput("hwm","hwm: window half width for local medians",300),
                                                                   numericInput("hws","hws: window half width for local smoothing",5),
                                                                   checkboxInput("end","end: original endpoint handling",F)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='modpolyfit'",
                                                                   numericInput("degree","degree: degree of polynomial",4),
                                                                   numericInput("tol","tol: tolerance of difference between iterations",0.001),
                                                                   numericInput("rep","rep: maximum number of iterations",100)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='peakDetection'",
                                                                   numericInput("left","left: smallest window size for peak widths",30),
                                                                   numericInput("right","right: largest window size for peak widths",300),
                                                                   numericInput("lwin","lwin: Smallest window size for minimums and medians in peak removed spectra",50),
                                                                   numericInput("rwin","rwin: Largest window size for minimums and medians in peak removed spectra",50),
                                                                   numericInput("snminimum","snminimum: Minimum signal to noise ratio for accepting peaks",10)
                                                  ),
                                                  conditionalPanel(condition="input.baseline=='rollingBall'",
                                                                   numericInput("wm","wm: Width of local window for minimization/maximization",200),
                                                                   numericInput("ws","ws: Width of local window for smoothing",200)
                                                  )
                                           ),
                                           column(3,
                                                  h4("Warping"),
                                                  helpText(   a("Wikipedia link",target="_blank",
                                                                href="https://en.wikipedia.org/wiki/Dynamic_time_warping")
                                                  ),
                                                  selectizeInput("warpmethod","Warping method",choices=(c("ptw",'dtw')),selected="ptw"),
                                                  conditionalPanel(condition="input.warpmethod=='ptw'",
                                                                   helpText(   a("Help for this feature",target="_blank",
                                                                                 href="https://www.rdocumentation.org/packages/ptw/versions/1.9-11/topics/ptw")
                                                                   ),
                                                                   #p("The best results I had was with respectively: ref=1, 'c(0,1,0)',individual,WCC,20 "),
                                                                   uiOutput('ptw.warp.ref')
                                                                   # numericInput("ptw.warp.ref","id of the reference",1)#,
                                                                   #textInput("ptw.init.coef","init.coef","c(0,1,0)"),
                                                                   #selectizeInput("ptw.warp.type","warp.type",choices=c("individual", "global"),selected="global"),
                                                                   #selectizeInput("ptw.optim.crit","optim.crit",choices=c("WCC", "RMS"),selected="WCC"),
                                                                   #numericInput("ptw.trwdth","trwdth",20)
                                                  ),
                                                  conditionalPanel(condition="input.warpmethod=='dtw'",
                                                                   helpText(   a("Help for this feature",target="_blank",
                                                                                 href="https://www.rdocumentation.org/packages/dtw/versions/1.18-1/topics/dtw?")
                                                                   ),
                                                                   uiOutput('dtw.warp.ref'),
                                                                   # numericInput("ptw.warp.ref","id of the reference",1),
                                                                   checkboxInput('dtw.split','Do the alignment on the 4 channels separately.',F)
                                                  )
                                                  ),
                                           column(3,
                                                  h4("Standardization"),
                                                  helpText(   a("Help for standard normal variate",target="_blank",
                                                                href="https://www.rdocumentation.org/packages/prospectr/versions/0.1.3/topics/standardNormalVariate")
                                                  ),
                                                  helpText(   a("Help for autoscaling",target="_blank",
                                                                href="http://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html")
                                                  )
                                                  )
                                           ),
                                  tabPanel("Track plot",
                                           uiOutput('choice.band.mono.aft.1'),
                                           plotOutput("plot.v.mono.aft.1"),
                                           uiOutput('choice.band.mono.aft.2'),
                                           plotOutput("plot.v.mono.aft.2")
                                  ),
                                  tabPanel("Densitograms comparison",
                                           uiOutput('choice.band.mono.aft.tot'),
                                           plotOutput("plot.v.mono.aft.tot")
                                  )
                                )
                              )
                            )
                   ),
                   tabPanel('Variables selection',
                            h4("Select channels and Rf ranges"),
                            column(6,
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_1", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_1", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=1)),
                                     column(9,uiOutput('VS_slider_1'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_2", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_2", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=2)),
                                     column(9,uiOutput('VS_slider_2'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_3", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_3", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=3)),
                                     column(9,uiOutput('VS_slider_3'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_4", label = NULL, value=T)),
                                     column(2,selectizeInput("VS_select_4", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_4'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_5", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_5", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=1)),
                                     column(9,uiOutput('VS_slider_5'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_6", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_6", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=2)),
                                     column(9,uiOutput('VS_slider_6'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_7", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_7", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=3)),
                                     column(9,uiOutput('VS_slider_7'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_8", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_8", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_8'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_9", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_9", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=1)),
                                     column(9,uiOutput('VS_slider_9'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_10", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_10", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=2)),
                                     column(9,uiOutput('VS_slider_10'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_11", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_11", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=3)),
                                     column(9,uiOutput('VS_slider_11'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_12", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_12", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_12'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_13", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_13", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_13'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_14", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_14", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_14'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_15", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_15", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_15'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_16", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_16", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_16'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_17", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_17", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_17'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_18", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_18", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_18'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_19", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_19", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_19'))
                                   ),
                                   fluidRow(
                                     column(1,checkboxInput("VS_check_20", label = NULL, value=F)),
                                     column(2,selectizeInput("VS_select_20", label = NULL, choices=c('red'=1,'green'=2,'blue'=3,'gray'=4),selected=4)),
                                     column(9,uiOutput('VS_slider_20'))
                                   )

                            ),
                            column(6,
                                   plotOutput('VS_plot')
                                   )

                            ),
                   navbarMenu("Exploratory statistics",
                              tabPanel("PCA",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                           h4("Choice of the parameters for PCA"),
                                           helpText(   a("Help for this feature",target="_blank",
                                                         href="https://www.rdocumentation.org/packages/ChemometricsWithR/versions/0.1.9/topics/PCA")
                                           ),
                                           tags$hr(),
                                           h4("Variable of interest"),
                                           uiOutput("select.col.plot.pca"),
                                           uiOutput("select.shape.plot.pca"),
                                           uiOutput("select.label.plot.pca"),
                                           tags$hr(),
                                           selectizeInput('PCA.comp.a', '1st component for the plot', choice=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10"),select="PC1"),
                                           selectizeInput('PCA.comp.b', '2nd component for the plot', choice=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10"),select="PC2"),
                                           checkboxInput('pca.ellipse','Plot the ellipse according to the color',F),
                                           checkboxInput('pca.axis','Plot the axis',F),
                                           numericInput('pca.ellipse.level','Confidence interval to calculate the ellipse',0.95),
                                           selectizeInput("pca.col.palette","Palette color",choices = c("default","Set1","Set2","Set3","Greys","Spectral","Pastel1","Pastel2","Paired","Dark2","Accent")),
                                           tableOutput("Table.dim.just.pca.label")
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("PCA",
                                                      textInput("pca.plot.1.title","Title of the graph","Principal component analysis"),
                                                      plotOutput("pca.plot.1",height="800px"),
                                                      dataTableOutput("pca.table.1"),
                                                      verbatimTextOutput("pca.summary.1")
                                             ),
                                             tabPanel("Pair plot (new)",
                                                      numericInput("PCA.pair","Number of components to consider",3),
                                                      div(style="display:inline-block",selectizeInput("PCA.pair.upper.continuous","Upper continuous",choices=c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),selected="density")),
                                                      # div(style="display:inline-block",selectizeInput("PCA.pair.upper.combo","Upper combo",choices=c('box', 'dot', 'facethist', 'facetdensity', 'denstrip', 'blank'),selected="box")),
                                                      #div(style="display:inline-block",selectizeInput("PCA.pair.upper.discrete","Upper discrete",choices=c('facetbar', 'ratio', 'blank'),selected="facetbar")),
                                                      div(style="display:inline-block",selectizeInput("PCA.pair.lower.continuous","Lower continuous",choices=c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),selected="points")),
                                                      # div(style="display:inline-block", selectizeInput("PCA.pair.lower.combo","Lower combo",choices=c('box', 'dot', 'facethist', 'facetdensity', 'denstrip', 'blank'),selected='facethist')),
                                                      #div(style="display:inline-block",selectizeInput("PCA.pair.lower.discrete","Lower discrete",choices=c('facetbar', 'ratio', 'blank'),selected="facetbar")),
                                                      # div(style="display:inline-block",selectizeInput("PCA.pair.diag.continuous","Diag continuous",choices=c('densityDiag', 'barDiag', 'blankDiag'),selected="densityDiag")), 
                                                      # div(style="display:inline-block",selectizeInput("PCA.pair.diag.discrete","Diag discrete",choices=c('barDiag', 'blankDiag'),selected="barDiag")),
                                                      plotOutput("pca.plot.pair",height="800px")
                                                      ),
                                             tabPanel("PCA 3D",
                                                      uiOutput("PCA_3d")
                                             ),
                                             tabPanel("Loading Plot",
                                                      radioButtons('pca.loading.choice','Component',choices=seq(10),selected=1),
                                                      plotOutput("pca.loading"),
                                                      checkboxInput('pcaloadinglocalmaxima','Pick peak for local maxima',F),
                                                      numericInput('pca.loading.local.maxima.span','Span of local maxima',20),
                                                      checkboxInput('pcaloadinglocalminima','Pick peak for local minima',F),
                                                      numericInput('pca.loading.local.minima.span','Span of local minima',20),
                                                      # conditionalPanel(condition="input.pcaloadinglocalmaxima==T",
                                                      HTML("<h4><i>R</i><sub>F</sub>  values of local maxima</h4>"),
                                                      verbatimTextOutput('pca.loading.local.maxima'),
                                                      HTML("<h4><i>R</i><sub>F</sub>  values of local minima</h4>"),
                                                      verbatimTextOutput('pca.loading.local.minima')
                                                      ),
#                                              tabPanel("PCA3D",
#                                                       webGLOutput("myWebGL.1",height="600px")
#                                              ),
                                             tabPanel("Outlier",
                                                      helpText(   a("Help for this feature",target="_blank",
                                                                    href="https://www.rdocumentation.org/packages/chemometrics/versions/1.4.1/topics/Moutlier")
                                                      ),
                                                      checkboxGroupInput("comp.outlier.pca.1","PCA component",choices=seq(10),selected=c(1,2)),
                                                      numericInput("quantile.outlier.pca.1","Quantile to use for the cutoff",0.975,min=0,max=1),
                                                      plotOutput("quantile.outlier.pca.1"),
                                                      dataTableOutput("quantile.outlier.pca.table")
                                             )
                                           )
                                         )
                                       )
                              ),
                              tabPanel("T-sne (new)",
                                       sidebarLayout(
                                         sidebarPanel(
                                           helpText(   a("Wikipedia link",target="_blank",
                                                         href="https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding")
                                           ),
                                           tags$hr(),
                                           actionButton("tsne.go","Analyse"),
                                           h4("Variable of interest"),
                                           uiOutput("select.col.plot.tsne.1"),
                                           h4("Options"),
                                           numericInput("tsne.k","the dimension of the resulting embedding.",2),
                                           numericInput("tsne.initial_dims","The number of dimensions to use in reduction method.",30),
                                           numericInput("tsne.perplexity","Perplexity parameter. (optimal number of neighbors) ",30),
                                           numericInput("tsne.max_iter","Maximum number of iterations to perform. ",1000),
                                           checkboxInput("tsne.whiten","whether the matrix data should be whitened. ",T)
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("2D plot",
                                                      div(style="display:inline-block",selectizeInput("tsne.pair.upper.continuous","Upper continuous",choices=c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),selected="density")),
                                                      div(style="display:inline-block",selectizeInput("tsne.pair.lower.continuous","Lower continuous",choices=c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank'),selected="points")),
                                                      plotOutput("plot.tsne.1",height=800)
                                             )
                                           ))
                                       )
                                       
                                       ),
                              tabPanel("k-means (new)",
                                      sidebarLayout(
                                        sidebarPanel(
                                          helpText(   a("Wikipedia link",target="_blank",
                                                        href="https://en.wikipedia.org/wiki/K-means_clustering")
                                          ),
                                          tags$hr(),
                                          selectizeInput("kmeans_data","data to use",choices = c("raw data","preprocessed data","data after variable selection")),
                                          h4("Variable of interest"),
                                          uiOutput("select.col.plot.kmeans.1"),
                                          h4("Options"),
                                          numericInput("kmeans.centers","The number of clusters",3),
                                          numericInput("kmeans.iter.max","The maximum number of iterations allowed.",10),
                                          selectizeInput("kmeans.algorithm","Algorithm",choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
                                        ),
                                        mainPanel(
                                          plotOutput("plot.kmeans.1"),
                                          uiOutput("select.cluster.kmeans.1"),
                                          conditionalPanel(condition="input.kmeans_data == 'preprocessed data'",
                                                           checkboxInput("kmeans.deprocess","depreprocess to see the raster",F)
                                                           ),
                                          plotOutput("plot.kmeans.2",height=800)
                                          )
                                      )
                                       ),
                              tabPanel("Cluster",  ####### cluster #######
                                       sidebarLayout(
                                         sidebarPanel(
                                           h4("Ward hierarchical clustering parameters"),
                                           helpText(   a("Help with this feature",target="_blank",
                                                         href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/dist.html")
                                           ),
                                           helpText(   a("Further help with this feature",target="_blank",
                                                         href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html")
                                           ),
                                           tags$hr(),
                                           h4("Variable of interest"),
                                           uiOutput("select.col.plot.cluster.1"),
                                           uiOutput("select.col.plot.cluster.2"),
                                           tags$hr(),
                                           selectizeInput("method.dist.cluster.1","Method for distance calculation",
                                                          choices=c("Euclidean" = "euclidean" , "Maximum"="maximum", "Manhattan"="manhattan", "Canberra"="canberra", "Binary"="binary","Minkowski"="minkowski"),select="euclidean"),
                                           selectizeInput("method.clust.cluster.1","Method for cluster analysis",
                                                          choices=c("Ward"="ward", "Single"="single", "Complete"="complete", "Average"="average", "Mcquitty"="mcquitty", "Median"="median","Centroid"="centroid"),select="ward"),
                                           numericInput("cluster.nbr.1","Number of clusters",5)
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Hierarchical clustering",
                                                      plotOutput("plot.cluster.1.1",height=800),
                                                      dataTableOutput("Cluster.table.1")
                                             )
                                           ))
                                       )
                              ),
                              tabPanel("Heatmap",  ####### Heatmap #######
                                       sidebarLayout(
                                         sidebarPanel(
                                           h4("Heatmap"),
                                           helpText(   a("Help with this feature",target="_blank",
                                                         href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/heatmap.html")
                                           ),
                                           tags$hr(),
                                           h4("Variable of interest"),
                                           uiOutput("select.col.plot.heatmap.1")
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
                              tabPanel("R console",
                                       wellPanel(
                                         tabsetPanel(
                                           tabPanel("Editor",
                                                    aceEditor("DPEeditor","## Take inspiration from the template to produce custom made plot",mode="r")
                                                    ),
                                           tabPanel("Plot",
                                                    numericInput("DPEplot_width", "Plot Width (px)",value = 800),
                                                    numericInput("DPEplot_height", "Plot Height (px)", value = 800),
                                                    imageOutput("DPEplot")
                                           ),
#                                            tabPanel("Print",
#                                                     verbatimTextOutput("DPEprint")
#                                            ),
                                           tabPanel('Template',
                                                    includeMarkdown('Exploratory-template.md')
                                           )
                                                    )
                                         )
                                       )
                                       ),
                   tabPanel("Predictive statistics",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           uiOutput('Train.model.algo'),
                                           uiOutput('Train.model.algo.wiki'),
                                           checkboxInput("Train.model.algo.all","Enable all algorithms (experimentale)",F),
                                           uiOutput("Train.column"),
                                           radioButtons('Trainproblem','Type',choices=c("Classification"='classification',"Regression"='regression'),selected='classification'),
                                           # checkboxGroupInput("col.Pred","Choice of the channel(s)",choices=c("red"=1,"green"=2,"blue"=3,"gray"=4),select=seq(4)),
                                           hr(),
                                           div(class="btn btn-default action-button shiny-bound-input",actionButton('Train.go','Train')),
                                           hr(),
                                           uiOutput('Train.down.model.text'),
                                           downloadButton("Train.down.model","Download model")
                              ),
                              mainPanel(width=9,
                                tabsetPanel(
                                  tabPanel("Tuning options",
                                           fluidRow(
                                             box(title='Cross validation',width=3,collapsible = F,
                                                 helpText(   a("Wikipedia link",target="_blank",
                                                               href="https://en.wikipedia.org/wiki/Cross-validation_%28statistics%29")
                                                 ),
                                                 helpText(   a("Help for this feature",target="_blank",
                                                               href="https://www.rdocumentation.org/packages/caret/versions/6.0-71/topics/trainControl")
                                                 ),
                                                 selectizeInput('Train.control.method','Validation method for tuning',
                                                                choices=c('boot', 'repeatedcv', 'LOOCV'),
                                                                selected='repeatedcv'),
                                                 uiOutput('Train.metric'),
                                                 numericInput('Train.tunning.CV','Number of folds or resampling iterations',5),
                                                 numericInput('Train.tunning.repeat','For repeated k-fold cross-validation: number of complete sets of folds',1)
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
                                  tabPanel("Algorithm information",
                                           verbatimTextOutput('Train.model.algo.info')
                                           ),
                                  tabPanel('Model summary',
                                           verbatimTextOutput('Train.validation')
                                           ),
                                  tabPanel('Tuning curve',
                                           plotOutput('Train.tunning.plot')
                                           ),
                                  tabPanel('R console',
                                           tabsetPanel(
                                             tabPanel("Editor",
                                                      aceEditor("DPEeditorpred","model <- Train.model() \nInd <- Train.Ind()\nDep <- Train.Dep()",mode="r")
                                             ),
                                             tabPanel("Plot",
                                                      imageOutput("DPE.pred.plot")
                                             ),
#                                              tabPanel("Print",
#                                                       verbatimTextOutput("DPE.pred.print")
#                                              ),
                                             tabPanel('Template',
                                                      includeMarkdown('Prediction-template.md')
                                                      )
                                           )
                                  )
                                  )
                                )
                            )
                            ),
                   tabPanel("Report output",
                            column(2,h4('Data input'),
                                   checkboxInput("mono.knitr.file.name", "Name of file", TRUE),
                                   checkboxInput("monoknitrpicture", "Chromatograms", TRUE),
                                   checkboxInput("mono.knitr.batch.simple", "Batch", TRUE),
                                   checkboxInput("mono.knitr.batch.pred", "Batch with prediction (QC)", FALSE),
                                   selectizeInput("mono.knitr.plot.brut","Track plot before preprocessing",choices=c("None","2","all"),selected="None")
                                   ),
                            column(2,h4('Data preprocessing and variable selection'),
                                   checkboxInput("mono.knitr.preprocess","Summary of preprocessing",T),
                                   selectizeInput("mono.knitr.plot.net","Track plot after preprocessing",choices=c("None","2","all"),selected="None"),
                                   checkboxInput("mono.knitr.var.select","Variable selection table",T)
                                   ),
                            column(2,h4('Exploratory statistics'),
                                   checkboxInput("mono.knitr.pca.plot", "PCA plot", FALSE),
                                   checkboxInput("mono.knitr.cluster.plot", "Cluster plot", FALSE),
                                   checkboxInput("mono.knitr.heatmap.plot", "Heatmap plot", FALSE)
                                   ),
                            column(2,h4('Predictive statistics'),
                                   checkboxInput('mono.knitr.prediction.summary.model','Model summary',F),
                                   checkboxGroupInput('mono.knitr.prediction.validation','',choices=c('Cross-validation data','Training data','Test data'))
                                   ),
                            column(2,h4('Report download'),
                                   textInput('mono.knitr.download.text','Filename','rTLC-report'),
                                   # downloadButton('mono.knitr.download','Download the report'),
                                   radioButtons('reportformat', 'Document format', c('PDF', 'HTML', "MS word"='Word'),
                                                inline = TRUE),
                                   downloadButton('downloadReport')
                                   ),
                            column(2,h4("Data download"),
                                   checkboxGroupInput("data.download.choice","Content",choices = c("Batch PCA"="batch.PCA","Loading PCA"="loading.PCA")),
                                   textInput('data.download.zip.text','Filename','rTLC_data_export'),
                                   downloadButton("data.download.zip",'Zip file')
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
                                tabPanel('R packages and session info',
                                          verbatimTextOutput('sessionInfo')
                                          ),
                                tabPanel('License',
                                         includeMarkdown('LICENSE.md')
                                         ),
                                tabPanel('Contact',
                                         h5('For information and specific help:'),
                                         hr(),
                                         HTML('<a href="mailto:dimitrifichou@gmail.com">Dimitri Fichou</a> '),
                                         hr(),
                                         HTML('<a href="mailto:p.ristivojevic@gmail.com">Petar Ristivojevic</a> '),
                                         hr(),
                                         p('Dimitri Fichou and Dr. Petar Ristivojević contributed to this application.
                                            Both of them discussed about design, visualization tools and multivariate analysis.
                                            Mr. Fichou designed all features of the application and Dr. Petar Ristivojević contributed by ideas and feedbacks.'),
                                         hr(''),
                                         p('This application was supported by Prof. Dr. Gertrud Morlock and her team at the Justus Liebig University Giessen and is generously hosted on the university server.')
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

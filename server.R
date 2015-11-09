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
require("prospectr");require("DiscriMiner");require("baseline");require("knitr")
require("xtable");require("ptw");require("dtw");
require('d3heatmap');require('randomForest');require('kernlab');require('ipred');
require('extraTrees');require('evtree')#;require('shinyRGL');require('rgl')
require('shinyAce');require('shinydashboard');


options(shiny.maxRequestSize=1000*1024^2)
# options(stringsAsFactors = FALSE)
source("rTLC-function.R")
source("PreProcess.function.R")
source('Var_selection.R')
MAC.inverse=F

shinyServer(function(input, output,session) {
#   session$onSessionEnded(function() {
#     obs$suspend()
#   })
  #### demo batch #####
  output$download.demo.batch <- downloadHandler(
    filename = "rTLC_demobatch.xls",
    content = function(file) {
      file.copy('www/rTLC_demobatch.xls', file)
    }
  ) 
  output$download.demo.picture <- downloadHandler(
    filename = "rTLC_demopicture.JPG",
    content = function(file) {
      file.copy('www/rTLC_demopicture.JPG', file)
    }
  ) 
  output$download.demo.data.bis <- downloadHandler(
    filename = "rTLC_demodata_bis.zip",
    content = function(file) {
      file.copy('www/rTLC_demodata_bis.zip', file)
    }
  ) 
  
  output$checkpoint.1.download <- downloadHandler(
    filename = "rTLC_checkpoint_1.Rdata",
    content = function(con) {
      assign("data",list(chrom = data.mono.2(),batch = dataX.mono.pre(), Vertical.dim = c(dim(data.mono.2())[2],input$hauteur.mono,input$Zf.mono,input$dist.bas.mono)))
      save(list="data", file=con)
    }
    )
  output$checkpoint.1.download.xlsx <- downloadHandler(
    filename = "rTLC_checkpoint_1.xslx",
    content = function(file) {
      tempFile <- tempfile(fileext = ".xlsx")
      write.xlsx(data.mono.2()[,,1],file=tempFile,sheetName = 'red',col.names=F,row.names =F,append=F)
      write.xlsx(data.mono.2()[,,2],file=tempFile,sheetName = 'green',col.names=F,row.names =F,append=T)
      write.xlsx(data.mono.2()[,,3],file=tempFile,sheetName = 'blue',col.names=F,row.names =F,append=T)
      write.xlsx(data.mono.2()[,,4],file=tempFile,sheetName = 'grey',col.names=F,row.names =F,append=T)
      write.xlsx(dataX.mono.pre(),file=tempFile,sheetName = 'batch',col.names=T,row.names =F,append=T)
      file.rename(tempFile, file)
    }
  )
    inFile.photo <- reactive({
        validate(
          need(input$filedemouse != "checkpoint", "Picture and dimension table not available, chromatograms already extracted.")
        )
    if(input$filedemouse == 'YourOwnData' | input$filedemouse == 'QC'){
      validate(
        need(input$filemonop != "", "Please upload your(s) picture(s) or choose to use the demo files")
      )
      inFile<- input$filemonop
    }
    if(input$filedemouse == 'demo1'){
      inFile<- data.frame(name = 'rTLC_demopicture.JPG',size=T,type='JPG',datapath = 'www/rTLC_demopicture.JPG')
    }
    if(input$filedemouse == 'demo2'){
      name = dir('www',patter='_2_')
      inFile<- data.frame(name = name,
                          size=rep(T,length(name)),
                          type=rep('JPG',length(name)),
                          datapath = paste0('www/',name)
      )
    }
    if(input$filedemouse == 'demo3'){
      name = dir('www',patter='_3_')
      inFile<- data.frame(name = name,
                          size=rep(T,length(name)),
                          type=rep('JPG',length(name)),
                          datapath = paste0('www/',name)
      )    
    }
    if(input$filedemouse == 'demo4'){
      name = dir('www',patter='_4_')
      inFile<- data.frame(name = name,
                          size=rep(T,length(name)),
                          type=rep('JPG',length(name)),
                          datapath = paste0('www/',name)
      )    
    }
    inFile
  })
  
  inFile.X <- reactive({
    validate(
      need(input$filedemouse != "checkpoint", "Picture and dimension table not available, chromatograms already extracted.")
    )
    if(input$filedemouse == 'YourOwnData' | input$filedemouse == 'QC'){
      if(is.null(input$fileX)){
        inFile <- NULL
      }else{
        inFile <- input$fileX
      }
    }
    if(input$filedemouse == 'demo1'){
      inFile <- data.frame(name = 'rTLC_demobatch.xls',size=T,type='xls',datapath = 'www/rTLC_demobatch.xls')
    }
    if(input$filedemouse == 'demo2'){
      inFile <- data.frame(name = 'rTLC_demobatch.xls',size=T,type='xls',datapath = 'www/rTLC_demobatch_2.xls')
    }
    if(input$filedemouse == 'demo3'){
      inFile <- data.frame(name = 'rTLC_demobatch.xls',size=T,type='xls',datapath = 'www/rTLC_demobatch_3.xls')
    }
    if(input$filedemouse == 'demo4'){
      inFile <- data.frame(name = 'rTLC_demobatch.xls',size=T,type='xls',datapath = 'www/rTLC_demobatch_4.xls')
    }
    return(inFile)
  })
  dataX.editable <- reactive({
    if(input$filedemouse == 'checkpoint'){
      validate(
        need(input$checkpoint.1.upload != "", "Please upload your Rdata file")
      )
      inFile <- input$checkpoint.1.upload
      load(inFile[1,4])
      data <- data[[2]]
      data$id <- seq(nrow(data))
    }else{
      inFile <- inFile.X()    
      if(is.null(inFile)){
        data <- data.mono.1.1()
        data <- data.frame(id = seq(dim(data)[1]),class = rep('unknow',dim(data)[1]),ref = rep('unknow',dim(data)[1]),info=rep('unknow',dim(data)[1]))
      }else{
        data <- read.xlsx(as.character(inFile[1,4]),sheetIndex=1)
      }
    }
    data$id <- seq(nrow(data))
    data
  })
  dataX <- reactive({
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    if(input$filedemouse == 'checkpoint'){
      validate(
        need(input$checkpoint.1.upload != "", "Please upload your Rdata file")
      )
      inFile <- input$checkpoint.1.upload
      load(inFile[1,4])
      data <- data[[2]]
      data$id <- seq(nrow(data))
    }else{
      inFile <- inFile.X()    
      if(is.null(inFile)){
        data <- data.mono.1.1()
        data <- data.frame(id = seq(dim(data)[1]),class = rep('unknow',dim(data)[1]),ref = rep('unknow',dim(data)[1]),info=rep('unknow',dim(data)[1]))
      }else{
        data <- read.xlsx(as.character(inFile[1,4]),sheetIndex=1)
      }
    }
    data.return <- data.frame()
    for(j in c(2:ncol(data))){
      truc <- c();for(i in seq(nrow(data))){truc <- c(truc,input[[paste0(colnames(data)[j],".",i)]])}
      data[,j] <- truc
    }
    data$id <- seq(nrow(data))
    data
  })
  ################ dataX.mono   ##########
  dataX.mono.pre.pre<-reactive({ 
    data <- dataX()
    validate(
      need(length(colnames(data)) >= 4, "Your batch must contain at least 4 columns"),
      need(colnames(data)[1] == "id", "The first column of your batch is not 'id'"),
      need(data[,1] == seq(1:nrow(data)) , "Your id column is not a sequence of number starting from 1")
    )
    data
  })
  dataX.mono.pre<-reactive({ 
    data<-dataX.mono.pre.pre()
    return(data[!Not.Use(),])
  })
  output$table1 <-renderTable({
    if(input$filedemouse == 'checkpoint'){
      validate(
        need(input$checkpoint.1.upload != "", "Please upload your Rdata file")
      )
      inFile <- input$checkpoint.1.upload
      load(inFile[1,4])
      data <- data[[2]]
      data$id <- seq(nrow(data))
    }else{
      data <- dataX.editable()
      validate(
        need(length(colnames(data)) >= 4, "Your batch must contain at least 4 columns"),
        need(colnames(data)[1] == "id", "The first column of your batch is not 'id'"),
        need(data[,1] == seq(1:nrow(data)) , "Your id column is not a sequence of number starting from 1")
      )
    }
    Not.Use <- paste0("<input id='Not.Use.", 1:nrow(data), "' class='shiny-bound-input' type='checkbox' value='1'>")
    for(i in c(2:ncol(data))){
      data[,i] <- paste0("<input id='",colnames(data)[i],'.', 1:nrow(data),"' class='shiny-bound-input' type='text' value='",data[,i],"'>")
    }
    data <- data.frame(cbind(data,Not.Use))
    return(data)
  }, sanitize.text.function = function(y) y)
  
  Not.Use <- reactive({
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    Not.Use <- c() 
    for(i in seq(nrow(dataX.mono.pre.pre()))){
      Not.Use <- c(Not.Use, input[[paste0("Not.Use.",i)]])
    }
    return(Not.Use)
  })
  
  
  ################ data.mono.1   ##########
  output$slider.subset.height<-renderUI({
    sliderInput('slider.subset.height','Subset to extract',min=0.01,max=input$hauteur.mono,value=c(0,input$hauteur.mono),
                step=0.01)
  })
  
  output$TableDimensionVerticale <-renderTable({
    if(input$filedemouse == 'QC'){print('truc');Default <- Pred.upload.model()[[7]]}
    if(input$filedemouse == 'checkpoint'){Default <- checkpoint.vert.dim()}
    if(input$filedemouse != 'QC' & input$filedemouse != 'checkpoint'){Default <- c(512,10,7,0.8)}
    data <- data.frame(Option = c('Pixel height','Plate height','Retention Front','Bottom distance'),
                       Value = c('redim.height','hauteur.mono','Zf.mono','dist.bas.mono'),
                       Default = Default
    )
    if(input$filedemouse == 'QC' | input$filedemouse == 'checkpoint'){
      data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='number' readonly='readonly' value='",data$Default,"'>")
    }else{
      data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='number'  value='",data$Default,"'>")
    }
    data[,c(1,2)]
  },include.rownames=F,include.colnames=F, sanitize.text.function = function(y) y)

  output$TableDimension <-renderTable({
    inFile <- inFile.photo()
    truc <- nrow(inFile)
    if(input$TableDimensionConvention == 'Linomat'){
      data <- data.frame(Plate_width = rep(200,truc),
                         Left_distance = rep(20,truc),
                         Band_width = rep(6,truc),
                         Gap_between_band = rep(2,truc),
                         Tolerance = rep(2,truc)
      )
    }else{
      data <- data.frame(Plate_width = rep(200,truc),
                         First_appli_position = rep(23,truc),
                         Band_Length = rep(6,truc),
                         Distance_between_tracks = rep(8,truc),
                         Tolerance = rep(2,truc)
      )
    }
    for(i in c(1:ncol(data))){
      data[,i] <- paste0("<input id='",colnames(data)[i],'.', 1:nrow(data),"' class='shiny-bound-input' type='number'  value='",data[,i],"'>")
    }
    data
  }, sanitize.text.function = function(y) y)

#   output$TableDimension <-renderHotable({
#     inFile <- inFile.photo()
#     truc <- nrow(inFile)
#     data <- data.frame(width_of_the_plate = rep(20,truc),
#                        left_distance = rep(2,truc),
#                        band_width = rep(0.6,truc),
#                        gap_between_band = rep(0.2,truc),
#                        tolerance_for_the_calcul = rep(0.2,truc)
#     )
#     data
#   }, readOnly=F)
  
  TableDimension <- reactive({
    # hot.to.df(input$TableDimension)
    inFile <- inFile.photo()
    truc <- nrow(inFile)
    if(input$TableDimensionConvention == 'Linomat'){
      data <- data.frame(Plate_width = rep(200,truc),
                         Left_distance = rep(20,truc),
                         Band_width = rep(6,truc),
                         Gap_between_band = rep(2,truc),
                         Tolerance = rep(2,truc)
      )
    }else{
      data <- data.frame(Plate_width = rep(200,truc),
                         First_appli_position = rep(23,truc),
                         Band_Length = rep(6,truc),
                         Distance_between_tracks = rep(8,truc),
                         Tolerance = rep(2,truc)
      )
    }
    for(j in c(1:ncol(data))){
      truc <- c();for(i in seq(nrow(data))){truc <- c(truc,input[[paste0(colnames(data)[j],".",i)]])}
      data[,j] <- truc
    }
    data
  })
  
  output$select.image.redim.mono<-renderUI({
    truc <- paste(seq(nrow(inFile.photo())),inFile.photo()$name,sep="  -  ")
    selectizeInput("select.image.redim.mono","Choice of the picture for chromatograms extraction with the dimension table",choices=truc)
  })
  output$image.redim.mono <- renderImage({
    n.pic<-as.numeric(substr(input$select.image.redim.mono,1,3))
    inFile <- inFile.photo()
    if(input$TableDimensionConvention == 'Linomat'){
      largeur<-as.numeric(TableDimension()[n.pic,1])
      dist.gauche<-as.numeric(TableDimension()[n.pic,2])
      band<-as.numeric(TableDimension()[n.pic,3])
      ecart<-as.numeric(TableDimension()[n.pic,4])
      tolerance<-as.numeric(TableDimension()[n.pic,5])
    }else{
      largeur<-as.numeric(TableDimension()[n.pic,1])
      band<-as.numeric(TableDimension()[n.pic,3])
      dist.gauche<-as.numeric(TableDimension()[n.pic,2])-band/2
      ecart<-as.numeric(TableDimension()[n.pic,4])-band
      tolerance<-as.numeric(TableDimension()[n.pic,5])
    }
    nbr.band<-round((largeur-2*dist.gauche)/(band+ecart))
    
    outfile <- tempfile(fileext='.png')
    png(outfile, width=1000, height=500)
    par(mar=c(5,4,0,0))
    plot(c(0,largeur),c(0,input$hauteur.mono), type='n',ylab="",xlab="",bty='n')
    rasterImage(f.read.image(as.character(inFile[n.pic,4]),native=T,input$mono.Format.type,height=0),0 , 0, largeur, input$hauteur.mono)
    for(i in seq(nbr.band)){
      text(x=(dist.gauche+tolerance+(i-1)*(band+ecart)),y=9,labels=i,col="red",cex=2)
      abline(v=dist.gauche+tolerance+(i-1)*(band+ecart),col="red")
      abline(v=dist.gauche-tolerance+band+(i-1)*(band+ecart),col="green")
      abline(h=input$Zf.mono,col='white')
      abline(h=input$dist.bas.mono,col='white')
      abline(h=input$slider.subset.height,col=c('red','green'))
    }
    dev.off()
    list(src = outfile,
         contentType = 'image/png',
         width = 600,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  ################ data.mono.2   ##########
  data.mono.1.1<-reactive({
    withProgress(message = "Work in Progress", value=0, {
      if(input$filedemouse == 'checkpoint'){
        validate(
          need(input$checkpoint.1.upload != "", "Please upload your Rdata file")
        )
        inFile <- input$checkpoint.1.upload
        load(inFile$datapath)
        data <- data[[1]]
      }else{
        inFile <- inFile.photo() 
    #     height <- min(dim.pictures()[,1])
        height <- input$redim.height
        for(n.pic in seq(nrow(inFile.photo()))){
          if(input$TableDimensionConvention == 'Linomat'){
            largeur<-as.numeric(TableDimension()[n.pic,1])
            dist.gauche<-as.numeric(TableDimension()[n.pic,2])
            band<-as.numeric(TableDimension()[n.pic,3])
            ecart<-as.numeric(TableDimension()[n.pic,4])
            tolerance<-as.numeric(TableDimension()[n.pic,5])
          }else{
            largeur<-as.numeric(TableDimension()[n.pic,1])
            band<-as.numeric(TableDimension()[n.pic,3])
            dist.gauche<-as.numeric(TableDimension()[n.pic,2])-band/2
            ecart<-as.numeric(TableDimension()[n.pic,4])-band
            tolerance<-as.numeric(TableDimension()[n.pic,5])
          }
          data.temp<-f.read.image(as.character(inFile[n.pic,4]),native=F,input$mono.Format.type,height=height)
          data.temp<-f.eat.image(data.temp,largeur,dist.gauche,band,ecart,tolerance)
          if(n.pic == 1){
            data <- data.temp
          }else{
            data <- abind(data,data.temp,along=1)
          }
        }
      }
    })
    return(data)
  })
  data.mono.1 <- reactive({
    data <- data.mono.1.1()
    dataX<-dataX()
    validate(
      need(dim(data)[1] == nrow(dataX), "The number of chromatograms extracted do not match the number of row in your batch, please check your batch or your dimension table")
    )
    rownames(data)<-dataX$id
    data
  })
  data.mono.2 <- reactive({
    data.mono.1()[!Not.Use(),,]
  })
  
  ################ Preprocess   ##########
Train.partition <- reactive({
  tot <- nrow(data.mono.2())
  set.seed(1)
  sample(c(T,F),tot,replace=T,prob=c(input$Train.partition,1-input$Train.partition))
})

  
  Preprocess.order <- reactive({
    if(input$filedemouse != 'QC'){
      input$Preprocess.order
    }else{
      Pred.upload.model()[[5]]
    }
  })
  Preprocess.options <- reactive({
    if(input$filedemouse != 'QC'){
      data <- data.mono.2()
      Smoothing <- list(window.size = input$window.size,poly.order=input$poly.order,diff.order=input$diff.order)
      if(input$warpmethod == 'ptw'){
        Warping <- list(warpmethod = input$warpmethod,
                        ptw.warp.ref = input$ptw.warp.ref,
                        ptw.init.coef=input$ptw.init.coef,
                        ptw.warp.type=input$ptw.warp.type,
                        ptw.optim.crit=input$ptw.optim.crit,
                        ptw.trwdth=input$ptw.trwdth)
      }
#       Center <- list(colMeans(data[Train.partition(),,1]),colMeans(data[Train.partition(),,2]),colMeans(data[Train.partition(),,3]),colMeans(data[Train.partition(),,4]))
#       Scale <- list(apply(data[Train.partition(),,1],2,sd),apply(data[Train.partition(),,2],2,sd),apply(data[Train.partition(),,3],2,sd),apply(data[Train.partition(),,4],2,sd))
      if(input$baseline == "als"){Baseline <- list(method=input$baseline,lambda.1=input$lambda.1,p=input$p,maxit.1=input$maxit.1)}
      if(input$baseline == "fillPeaks"){Baseline <- list(method=input$baseline,lambda.2=input$lambda.2,hwi=input$hwi,it=input$it,int=input$int)}
      if(input$baseline == "irls"){Baseline <- list(method=input$baseline,lambda1=input$lambda1,lambda2=input$lambda2,maxit.2=input$maxit.2,wi=input$wi)}
      if(input$baseline == "lowpass"){Baseline <- list(method=input$baseline,steep=input$steep,half=input$half)}
      if(input$baseline == "medianWindow"){Baseline <- list(method=input$baseline,hwm=input$hwm,hws=input$hws,end=input$end)}
      if(input$baseline == "modpolyfit"){Baseline <- list(method=input$baseline,degree=input$degree,tol=input$tol,rep=input$rep)}
      if(input$baseline == "peakDetection"){Baseline <- list(method=input$baseline,left=input$left,right=input$right,lwin=input$lwin,rwin=input$rwin)}
      if(input$baseline == "rfBaseline"){Baseline <- list(method=input$baseline)}
      if(input$baseline == "rollingBall"){Baseline <- list(method=input$baseline,wm=input$wm,ws=input$ws)}
      return(list(Smoothing=Smoothing,Warping=Warping,Baseline.correction=Baseline))
    }else{
      Pred.upload.model()[[4]]
    }
  })

  data.mono.3<-reactive({  
    if(input$filedemouse != 'QC'){
      validate(
        need(input$window.size %% 2 == 1, "The window size must be an odd value"),
        need(input$window.size > input$poly.order, "The window size must be greater than the polynomial order"),
        need(input$poly.order > input$diff.order, "The polynomial order must be greater than the differential order")
      )
      validate(
        need(!Not.Use()[input$ptw.warp.ref], "the reference id for the warping is not in the batch")
      )
      validate(
        need(Train.partition()[input$ptw.warp.ref], "the reference id for the warping is not in the training set")
      )
      withProgress(message = "Work in Progress", value=0, {
        data<-data.mono.2()
        data <- f.preprocess(data,preprocess.order = Preprocess.order(),preprocess.option = Preprocess.options(),
                             training.data = data[Train.partition(),,])
      })
    }else{
      withProgress(message = "Work in Progress", value=0, {
        data<-data.mono.2()
        data <- f.preprocess(data,preprocess.order = Preprocess.order(),preprocess.option = Preprocess.options(),
                             training.data = Pred.upload.model()[[2]])
      })
    }
    
    return(data)
  })

  ##### Variable.selection #####
  RF.max <- reactive({
    hauteur<-input$hauteur.mono
    dist.bas<-input$dist.bas.mono
    Zf <- input$Zf.mono
    round((hauteur-dist.bas)/(Zf-dist.bas),3)
  })
  RF.min <- reactive({
    hauteur<-input$hauteur.mono
    dist.bas<-input$dist.bas.mono
    Zf <- input$Zf.mono
    round(-dist.bas/(Zf-dist.bas),3)
  })
  output$VS_slider_1 <- renderUI({sliderInput("VS_slider_1", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_2 <- renderUI({sliderInput("VS_slider_2", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_3 <- renderUI({sliderInput("VS_slider_3", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_4 <- renderUI({sliderInput("VS_slider_4", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_5 <- renderUI({sliderInput("VS_slider_5", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_6 <- renderUI({sliderInput("VS_slider_6", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_7 <- renderUI({sliderInput("VS_slider_7", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_8 <- renderUI({sliderInput("VS_slider_8", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_9 <- renderUI({sliderInput("VS_slider_9", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_10 <- renderUI({sliderInput("VS_slider_10", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_11 <- renderUI({sliderInput("VS_slider_11", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  output$VS_slider_12 <- renderUI({sliderInput("VS_slider_12", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.01)})
  
  selection.table <- reactive({
    if(input$filedemouse != 'QC'){
      n <- 12
      use <- c();for(i in seq(n)){use <- c(use,input[[paste0('VS_check_',i)]])}
      channel <- c();for(i in seq(n)){channel <- c(channel,input[[paste0('VS_select_',i)]])}
      start <- c();for(i in seq(n)){start <- c(start,input[[paste0('VS_slider_',i)]][1])}
      stop <- c();for(i in seq(n)){stop <- c(stop,input[[paste0('VS_slider_',i)]][2])}
      data.frame(
        use = use,
        channel = channel,
        start = start,
        stop = stop
      )
    }else{
      Pred.upload.model()$channel
    }
    
  })
  output$VS_plot <- renderPlot({
    par(mfrow=c(1,2))
    selection <- selection.table()
    min=RF.min();max=RF.max()
    plot(c(min,max),c(1,12),type='n',xlab = 'Rf',ylab = 'index')
    selection$channel <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',selection$channel))))
    
    for(i in seq(12)){
      if(selection[i,1] == T){
        arrows(x0=selection[i,3], y0=i, x1 =selection[i,4],col=selection[i,2])
        arrows(x1=selection[i,3], y0=i, x0 =selection[i,4],col=selection[i,2])
      }
    }
    plot(x=seq(dim(data.mono.4())[2]),data.mono.4()[1,],type='l')
  })
    
  
  data.mono.4 <- reactive({
    data <- data.mono.3()
    validate(
      need(sum(selection.table()$use) != 0, "At least one channel must be used")
    )
    var_selection(data,selection.table(),RF.min(),RF.max())
  })

  
  ################ plot.monovariate before and after and integration    ##########
  output$plot.v.mono.bef.1 <- renderPlot({
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    data <- data.mono.2()
    n.band<-as.numeric(substr(input$name.band.mono.bef.1,7,9))
    f.plot.array(data,n.band,Truc.mono(),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,inverse=MAC.inverse)
    abline(v=input$z.min,col=5)
    abline(v=input$z.max,col=7)
  },height = 400,width=700)
  # output$plot.v.mono.bef.zoom.1 <- renderPlot({
  #   n.band<-as.numeric(substr(input$name.band.mono.bef.1,7,9))
  #   f.plot.array(data.mono.2(),n.band,Truc.mono(),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,xlim=c(input$z.min,input$z.max))
  #   abline(v=input$z.min,col=5)
  #   abline(v=input$z.max,col=7)
  # },height = 400,width=700)
  output$plot.v.mono.bef.2 <- renderPlot({
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    data <- data.mono.2()
    n.band<-as.numeric(substr(input$name.band.mono.bef.2,7,9))
    f.plot.array(data,n.band,Truc.mono(),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,inverse=MAC.inverse)
    abline(v=input$z.min,col=5)
    abline(v=input$z.max,col=7)
  },height = 400,width=700)
  # output$plot.v.mono.bef.zoom.2 <- renderPlot({
  #   n.band<-as.numeric(substr(input$name.band.mono.bef.2,7,9))
  #   f.plot.array(data.mono.2(),n.band,Truc.mono(),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,xlim=c(input$z.min,input$z.max))
  #   abline(v=input$z.min,col=5)
  #   abline(v=input$z.max,col=7)
  # },height = 400,width=700)
  output$plot.v.mono.aft.1 <- renderPlot({
    data <- data.mono.3()
    n.band<-as.numeric(substr(input$name.band.mono.aft.1,7,9))
    #   par(mar=c(5,4,4,0))
    f.plot.array(data,n.band,Truc.mono(),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,reconstruct=F)
    #   abline(v=input$z.min,col=5)
    #   abline(v=input$z.max,col=7)
    abline(h=0)
  },height = 400,width=700)
  output$plot.v.mono.aft.2 <- renderPlot({
    data <- data.mono.3()
    n.band<-as.numeric(substr(input$name.band.mono.aft.2,7,9))
    #   par(mar=c(5,4,4,0))
    f.plot.array(data,n.band,Truc.mono(),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,reconstruct=F)
    #   abline(v=input$z.min,col=5)
    #   abline(v=input$z.max,col=7)
    abline(h=0)
  },height = 400,width=700)

output$plot.v.mono.bef.tot <- renderPlot({
  validate(
    need(length(input$name.band.mono.bef.tot) > 1,"Select at least 2 band for comparaison")
    )
  n.band<-as.numeric(substr(input$name.band.mono.bef.tot,7,9))
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  data <- data.mono.2()
  par(mar=c(5,4,4,0), mfrow=c(4,1))
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,1])),type="l",main="Red channel",xlab=expression("R"['F']),ylab="intensity")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,2])),type="l",main="Green channel",xlab=expression("R"['F']),ylab="intensity")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,3])),type="l",main="Blue channel",xlab=expression("R"['F']),ylab="intensity")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,4])),type="l",main="Grey channel",xlab=expression("R"['F']),ylab="intensity")
},height = 1200,width=800)

output$plot.v.mono.aft.tot <- renderPlot({
  validate(
    need(length(input$name.band.mono.aft.tot) > 1,"Select at least 2 band for comparaison")
  )
  n.band<-as.numeric(substr(input$name.band.mono.aft.tot,7,9))
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  data <- data.mono.3()
  par(mar=c(5,4,4,0), mfrow=c(4,1))
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,1])),type="l",main="Red channel",xlab=expression("R"['F']),ylab="intensity")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,2])),type="l",main="Green channel",xlab=expression("R"['F']),ylab="intensity")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,3])),type="l",main="Blue channel",xlab=expression("R"['F']),ylab="intensity")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),
          y=t(as.matrix(data[n.band,,4])),type="l",main="Grey channel",xlab=expression("R"['F']),ylab="intensity")
},height = 1200,width=800)
output$image.comparaison.1 <- renderPlot({
  validate(
    need(length(input$name.band.m.com.1) > 1,"Select at least 2 band for comparaison")
  )
  data<-data.mono.2()
  band<-as.numeric(substr(input$name.band.m.com.1,7,9))
  plot(c(0,length(band)),c(0,10), type='n',ylab="",xlab="",xaxt = "n",yaxt = "n")
  for(i in seq(band)){
    data2<-f.rebuilt(data[as.character(band[i]),,1],data[as.character(band[i]),,2],data[as.character(band[i]),,3])
    rasterImage(data2,i-1,0,i,10)
    text(x=i-0.5,y=9,labels=band[i],col="red",cex=2)
    par(new=T)
  }
})
# ################ model pca ##########
model.pca<-reactive({
  data <- data.mono.4()
#   channel <- as.numeric(input$col.pca)
#   validate(
#     need(length(channel) != 0, "At least one channel must be used")
#   )
#   hauteur<-input$hauteur.mono
#   dist.bas<-input$dist.bas.mono
#   Zf <- input$Zf.mono
#   data <- f.rebind(data=data,channel = channel,hauteur = hauteur,dist.bas=dist.bas,Zf=Zf)
  PCA(as.matrix(data))
})
# ################# output$pca.plot.1 ################# 
pca.plot.1<-reactive({ 
  data<-model.pca()
#   label.color <- paste(input$col.pca,collapse=', ')
#   label.color <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',label.color))))
  xlabel<-paste(round(data$var[as.numeric(substr(input$PCA.comp.a,5,5))]/data$totalvar*100,2),"%")
  ylabel<-paste(round(data$var[as.numeric(substr(input$PCA.comp.b,5,5))]/data$totalvar*100,2),"%")
  data<-scores(data,npc=10)
  colnames(data)<-c("comp1","comp2","comp3","comp4","comp5","comp6","comp7","comp8","comp9","comp10")
  data<-data[,c(input$PCA.comp.a,input$PCA.comp.b)]
  colnames(data)<-c("comp1","comp2")
  data<-cbind(dataX.mono.pre(),data)
  plot<-ggplot()+geom_point(data=data,aes(x=comp1,y=comp2),size=as.numeric(input$cex.pca))+ 
    labs(x=xlabel, y=ylabel)
  if(input$shape.plot.pca != "None"){
    validate(
      need(length(unique(data[,input$shape.plot.pca])) < 6, "The number of factor in your shape variable must be lower than 6")
    )
  }
  if(input$shape.plot.pca != "None" & input$col.plot.pca == "None"){   
    data$Shape<-data[,input$shape.plot.pca]
    plot<-ggplot()+geom_point(data=data,aes(x=comp1,y=comp2,shape=Shape),size=as.numeric(input$cex.pca))+ 
      labs(x=xlabel, y=ylabel)
  }
  if(input$col.plot.pca != "None" & input$shape.plot.pca == "None"){
    data$Color<-data[,input$col.plot.pca]
    plot<-ggplot()+geom_point(data=data,aes(x=comp1,y=comp2,col=Color),size=as.numeric(input$cex.pca))+ 
      labs(x=xlabel, y=ylabel)
  }   
  if(input$col.plot.pca != "None" & input$shape.plot.pca != "None"){
    data$Color<-data[,input$col.plot.pca]
    data$Shape<-data[,input$shape.plot.pca]
    plot<-ggplot()+geom_point(data=data,aes(x=comp1,y=comp2,col=Color,shape=Shape),size=as.numeric(input$cex.pca))+ 
      labs(x=xlabel, y=ylabel)
  }   
#   if(input$plotlyPCA==T){
#     p <- plotly(username=input$plot.ly.user, key=input$plot.ly.key)
#     p$ggplotly(plot)
#   }
  if(input$label.plot.pca != "None"){
    data$Label<-data[,input$label.plot.pca]
    plot<-plot+geom_text(data=data,aes(x=comp1,y=comp2,label=Label),hjust=as.numeric(input$hjust.pca),vjust=as.numeric(input$vjust.pca))
  }     
  if(input$pca.ellipse == T){plot <- plot+ stat_ellipse(data=data,aes(x=comp1,y=comp2,col=Color))}
  return(plot+ggtitle(input$pca.plot.1.title))
})
output$pca.plot.1<-renderPlot({
  truc <- pca.plot.1()
  print(truc)
})

# ################# output$pca.summary ################# 
output$pca.summary.1<-renderPrint({ 
  summary(model.pca())
})

# ################# output$pca.table.1 ################# 
output$pca.table.1<-renderDataTable({ 
  data<-model.pca()
  data<-scores(data,npc=4)
  colnames(data)<-c("comp1","comp2","comp3","comp4")
  cbind(dataX.mono.pre(),data) 
})
## render a selectize input with the name of the columns as choice for the pca
output$select.col.plot.pca<-renderUI({
  selectizeInput("col.plot.pca","Choice of the color",choices=c("None",colnames(dataX.mono.pre())),selected="None")
})
output$select.shape.plot.pca<-renderUI({
  selectizeInput("shape.plot.pca","Choice of the shape (no more than 5 or it's not shown)",choices=c("None",colnames(dataX.mono.pre())),selected="None")
})
output$select.label.plot.pca<-renderUI({
  selectizeInput("label.plot.pca","Choice of the label (location to chose bellow)",choices=c("None",colnames(dataX.mono.pre())),selected="None")
})
output$Table.dim.just.pca.label <-renderTable({
  hjust <- paste0("<input id='hjust.pca", "' class='shiny-bound-input' type='text' value='0'>")
  vjust <- paste0("<input id='vjust.pca", "' class='shiny-bound-input' type='text' value='0'>")
  point.size <- paste0("<input id='cex.pca", "' class='shiny-bound-input' type='text' value='5'>")
  data.frame("aesthetic" = rbind(hjust,vjust,point.size))
}, sanitize.text.function = function(y) y)

output$pca.loading <- renderPlot({
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  model <- model.pca()
  data <- loadings.PCA(model)[,as.numeric(input$pca.loading.choice)]
  maxi <- (hauteur-dist.bas)/(Zf-dist.bas)
  mini <- -dist.bas/(Zf-dist.bas)
  RF = seq(maxi,mini,length.out=length(data))
  # par(xaxp  = c(min(RF), max(RF), 0.1))
  plot(x=RF, xaxt = "n",
          y=as.matrix(data),type="l",main=paste0("Loading plot: PC",input$pca.loading.choice),xlab=expression("R"['F']),ylab="intensity")
  axis(side = 1, at = round(seq(maxi,mini,length.out=(maxi-mini)*10),2))
  if(input$pcaloadinglocalmaxima == T){
    abline(v = RF[pick.peaks(data, input$pca.loading.local.maxima.span)], col = "blue")
  }
  if(input$pcaloadinglocalminima == T){
    abline(v = RF[pick.peaks(-data, input$pca.loading.local.minima.span)], col = "red")
  }
})
output$pca.loading.local.minima <- renderPrint({
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  model <- model.pca()
  data <- loadings.PCA(model)[,as.numeric(input$pca.loading.choice)]
  maxi <- (hauteur-dist.bas)/(Zf-dist.bas)
  mini <- -dist.bas/(Zf-dist.bas)
  RF = seq(maxi,mini,length.out=length(data))
  print(RF[pick.peaks(-data, input$pca.loading.local.minima.span)])
})
output$pca.loading.local.maxima <- renderPrint({
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  model <- model.pca()
  data <- loadings.PCA(model)[,as.numeric(input$pca.loading.choice)]
  maxi <- (hauteur-dist.bas)/(Zf-dist.bas)
  mini <- -dist.bas/(Zf-dist.bas)
  RF = seq(maxi,mini,length.out=length(data))
  print(RF[pick.peaks(data, input$pca.loading.local.maxima.span)])
})

# output$myWebGL.1 <- renderWebGL({ ## NOT WORKING
#   data<-model.pca()
#   data<-scores(data,npc=3)
#   colnames(data)<-c("comp1","comp2","comp3")
#   data<-cbind(dataX.mono.pre(),data)
#   # dataX <- dataX.mono.pre()
#   data$Color <- data$Drug
#   text3d(data$comp1, data$comp2,data$comp3,text=data$id,col=rainbow(length(levels(factor(data$Color))))[factor(data$Color)])
#   axes3d()
#   title3d(xlab="comp1",ylab="comp2",zlab="comp3")
# })


################# Outliers PCA #################
Moutlier.pca.1<-reactive({Moutlier(scores(model.pca(),npc=10)[,as.numeric(input$comp.outlier.pca.1)],quantile = input$quantile.outlier.pca.1, plot=F)})
output$quantile.outlier.pca.1<-renderPlot({
  par(mfrow=c(1,2))
  plot( x = rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$md,
        pch = '', xlab = 'Index of the observations',  ylab = 'Classical Mahalanobis distance' )
  text( x = rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$md,
        labels = rownames(scores(model.pca(),npc=2)) )
  abline(h=Moutlier.pca.1()$cutoff)
  
  plot( x = rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$rd,
        pch = '', xlab = 'Index of the observations',  ylab = 'Robust Mahalanobis distance' )
  text( x = rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$rd,
        labels = rownames(scores(model.pca(),npc=2)))
  abline(h=Moutlier.pca.1()$cutoff)
})
output$quantile.outlier.pca.2<-renderPrint({
  Moutlier(scores(model.pca(),npc=10)[,as.numeric(input$comp.outlier.pca.1)],quantile = input$quantile.outlier.pca.1)
})

################# Cluster #################
# Method 1 : Ward Hierarchical Clustering
data.cluster.1<-reactive({
  data <- data.mono.4()
#   channel <- as.numeric(input$col.cluster.1)
#   validate(
#     need(length(channel) != 0, "At least one channel must be used")
#   )
#   hauteur<-input$hauteur.mono
#   dist.bas<-input$dist.bas.mono
#   Zf <- input$Zf.mono
#   data <- f.rebind(data=data,channel = channel,hauteur = hauteur,dist.bas=dist.bas,Zf=Zf)
  return(data)
})
plot.cluster.1.1 <- reactive({
  data<-data.cluster.1()
  if(input$Var.cluster.1 != "id"){rownames(data)<-paste(dataX.mono.pre()[,input$Var.cluster.1],dataX.mono.pre()[,"id"],sep=" , ")}
  d <- dist(data, method = input$method.dist.cluster.1) # distance matrix
  fit <- hclust(d, method=input$method.clust.cluster.1)
#   label.color <- paste(input$col.cluster.1,collapse=', ')
#   label.color <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',label.color))))
  plot(fit,main="Cluster Dentogram",xlab="",
       sub=paste0("Distance Method: ",input$method.dist.cluster.1,"\n","Cluster Method: ",input$method.clust.cluster.1)) # display dendogram
  groups <- cutree(fit, k=input$cluster.nbr.1)
  rect.hclust(fit, k=input$cluster.nbr.1, border="red")
})
output$plot.cluster.1.1<-renderPlot({
  plot.cluster.1.1()
})

output$Cluster.table.1<-renderDataTable({
  data<-data.cluster.1()
  d <- dist(data, method = input$method.dist.cluster.1) # distance matrix
  fit <- hclust(d, method=input$method.clust.cluster.1)
  groups <- cutree(fit, k=input$cluster.nbr.1) 
  data<-cbind(dataX.mono.pre(),groups)
  data
})
## render a selectize input with the name of the columns as choice for the cluster method 1
output$select.col.plot.cluster.1<-renderUI({
  radioButtons("Var.cluster.1","Choice of the variable for the cluster plot",choices=colnames(dataX.mono.pre()))
})

################# output$heatmap #################
data.heatmap<-reactive({
  data <- data.mono.4()
#   channel <- as.numeric(input$col.heatmap.1)
#   validate(
#     need(length(channel) != 0, "At least one channel must be used")
#   )
#   hauteur<-input$hauteur.mono
#   dist.bas<-input$dist.bas.mono
#   Zf <- input$Zf.mono
#   data <- f.rebind(data=data,channel = channel,hauteur = hauteur,dist.bas=dist.bas,Zf=Zf)
  return(data)
})
output$plot.heatmap.1 <- renderPlot({
  data<-data.heatmap()
  if(input$Var.heatmap.1 != "id"){rownames(data)<-paste(dataX.mono.pre()[,input$Var.heatmap.1],dataX.mono.pre()[,"id"],sep=" , ")}
  heatmap(data[,rev(seq(dim(data)[2]))],Colv=NA)
  })
output$select.col.plot.heatmap.1<-renderUI({
  radioButtons("Var.heatmap.1","Choice of the variable for the heatmap plot",choices=colnames(dataX.mono.pre()))
})
output$plot.heatmap.2 <- renderD3heatmap({
  data<-data.heatmap()
  if(input$Var.heatmap.1 != "id"){rownames(data)<-paste(dataX.mono.pre()[,input$Var.heatmap.1],dataX.mono.pre()[,"id"],sep=" , ")}
  d3heatmap(data[,rev(seq(dim(data)[2]))],Colv=NA)
})

################# output$DPE.plot #################
output$DPEplot <- renderImage({
  outfile <- tempfile(fileext='.png')
  png(outfile, width=800, height=800)
  data <- data.mono.4()
  dataX <- dataX.mono.pre()
  eval(parse(text=input$DPEeditor))
  dev.off()
  list(src = outfile,
       alt = 'if you see this text, something went wrong')
},deleteFile=TRUE)

output$DPEprint <- renderPrint({
  data <- data.mono.4()
  dataX <- dataX.mono.pre()
  eval(parse(text=input$DPEeditor))
})

##### Train : Predictive Statistics ########

output$Train.column<-renderUI({
  radioButtons("Train.column","Choice of the variable",choices=colnames(dataX.mono.pre()))
})
output$Train.model.algo.info <- renderPrint({
  getModelInfo()[input$Train.model.algo]
})

Train.Ind <- reactive({
  data <- data.mono.4()
#   channel <- as.numeric(input$col.Pred)
#   validate(
#     need(length(channel) != 0, "At least one channel must be used")
#   )
#   hauteur<-input$hauteur.mono
#   dist.bas<-input$dist.bas.mono
#   Zf <- input$Zf.mono
#   data <- f.rebind(data=data,channel = channel,hauteur = hauteur,dist.bas=dist.bas,Zf=Zf)
  return(as.matrix(data))
})
Train.Dep <- reactive({
  data <- dataX.mono.pre()[,input$Train.column]
  if(input$Train.problem == 'classification'){
    data <- gsub(' ','_',data)
    data <- as.factor(data)
  }else{
    data <- as.numeric(data)
    validate(
      need(sum(!is.numeric(data)) == 0, "All the data are not numeric")
    )
  }
  data
})
Train.model.grid.pre <- reactive({
  grid <- getModelInfo(model = input$Train.model.algo)[[input$Train.model.algo]]$grid
  grid(Train.Ind(),Train.Dep(),len=input$Train.tunning.length)
})
output$Train.model.grid.edit <- renderTable({
  data <- Train.model.grid.pre()
  largeur <- ncol(data)
  longueur <- nrow(data)
  store <- matrix(rep(NA,largeur*longueur),ncol=largeur,nrow=longueur)
  for(i in seq(largeur)){
    store[,i] <- paste0("<input id='Train.model.grid.",i,'.', 1:longueur, "' class='shiny-bound-input' type='text' value='",data[,i],"'>")
  }
  colnames(store) <- colnames(data)
  return(store)
}, sanitize.text.function = function(y) y)
Train.model.grid.edit <- reactive({
  para <- getModelInfo(model = input$Train.model.algo)[[input$Train.model.algo]]$parameters$class
  data <- Train.model.grid.pre()
  largeur <- ncol(data)
  longueur <- nrow(data)
  store <- matrix(rep(NA,largeur*longueur),ncol=largeur,nrow=longueur)
  for(i in seq(largeur)){
    truc <- c();for(j in seq(longueur)){truc <- c(truc,input[[paste0("Train.model.grid.",i,".",j)]])}
    store[,i] <- as.numeric(truc)
  }
  store <- as.data.frame(store)
  colnames(store) <- colnames(data)
  return(store)
})

Train.model <- eventReactive(input$Train.go,{
  withProgress(message = "Work in Progress", value=0, {
    incProgress(0)
    data <- data.frame(Ind = Train.Ind(), Dep = Train.Dep())
    training <- data[Train.partition(),]
    set.seed(1)
    control <- trainControl(method = 'repeatedcv',
                            number=input$Train.tunning.CV,
                            repeats=input$Train.tunning.repeat,
                            allowParallel=T,verboseIter=T) # add ,classProbs = T for score but must add classmat2classvec and avoid regression
    set.seed(1)
    model <- train(Dep ~. , data = training,
                   method=input$Train.model.algo,
                   tuneGrid = Train.model.grid.edit(),
                   trControl = control
    )
  })
  return(model)
})

Train.prediction <- reactive({
  data <- data.frame(Ind = Train.Ind(), Dep = Train.Dep())
  predict(Train.model(),newdata=data)
})
output$Train.pred.table <- renderDataTable({
  cbind(dataX.mono.pre(),Prediction = Train.prediction(), Training = Train.partition())
})
output$Train.validation <- renderPrint({
  print(Train.model())
})
output$Train.valid.table <- renderTable({
  table(Train.Dep()[Train.partition() %in% input$Train.valid.table.use],
                  Train.prediction()[Train.partition() %in% input$Train.valid.table.use])
})
output$Train.valid.print <- renderPrint({
  confusionMatrix(Train.Dep()[Train.partition() %in% input$Train.valid.table.use],
                  Train.prediction()[Train.partition() %in% input$Train.valid.table.use])
})

output$Train.down.model <- downloadHandler(
  filename = function(preprocess=Preprocess.order(),color=input$col.Pred,model=input$Train.model.algo){
    color <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',color))));paste0(model,'_channel_',paste(color,collapse='-'),'_Preprocess_',paste(preprocess,collapse='-'),'.Rdata')
    },
  content = function(con) {
    assign('data',list(model = Train.model(),
                       origine.data = data.mono.2()[Train.partition(),,],
                       dataX = dataX.mono.pre(),
                       Preprocess.options = Preprocess.options(),
                       Preprocess.order = Preprocess.order(),
                       channel = selection.table(),
                       Vertical.dim = c(dim(data.mono.2())[2],input$hauteur.mono,input$Zf.mono,input$dist.bas.mono)
    ))
    save(list='data',file=con)
  }
) 

output$DPE.pred.plot <- renderImage({
  outfile <- tempfile(fileext='.png')
  png(outfile, width=800, height=800)
  data <- data.mono.4()
  dataX <- dataX.mono.pre()
  eval(parse(text=input$DPEeditorpred))
  dev.off()
  list(src = outfile,
       alt = 'if you see this text, something went wrong')
},deleteFile=TRUE)

output$DPE.pred.print <- renderPrint({
  data <- data.mono.4()
  dataX <- dataX.mono.pre()
  eval(parse(text=input$DPEeditorpred))
})

#### QC (or check point for the verticale dimension )####
checkpoint.vert.dim <- reactive({
  validate(
    need(input$checkpoint.1.upload != "", "Please upload your saved Rdata file")
  )
  inFile <- input$checkpoint.1.upload
  load(inFile$datapath)
  return(data$Vertical.dim)
})

Pred.upload.model <- reactive({
  validate(
    need(input$Pred.upload.model != "", "Please upload your model Rdata file")
  )
  inFile <- input$Pred.upload.model
  load(inFile$datapath)
  return(data)
})
Pred.prediction.data <- reactive({
  data <- data.mono.4()
#   data <- f.rebind(data=data,channel = as.numeric(Pred.upload.model()[[6]]),
#                    hauteur = Pred.upload.model()[[7]][2],dist.bas=Pred.upload.model()[[7]][4],Zf=Pred.upload.model()[[7]][3])
  # data <- t(apply(data.mono.4()[,,as.numeric(Pred.upload.model()[[6]])],c(1),cbind))
  data <- data.frame(Ind = data)
  predict(Pred.upload.model()[[1]],newdata=data)
})
output$table2 <- renderTable({
  validate(
    need(input$filedemouse == "QC", "Feature only available in 'Predict data - QC' mode")
  )
  cbind(dataX.mono.pre(),Pred.prediction.data())
})

###############mono renderUI################
## render a selectize input with the name of the columns as choice for the monovariate
# output$select.column.mono<-renderUI({
#   selectizeInput("column.mono","Choice of the variable to study",choices=colnames(dataX.mono.pre()))
# })
# output$mono.knitr.methode.choice<-renderUI({
#   if(is.null(input$mono.knitr.methode.file)){
#     selectizeInput("mono.knitr.methode.choice","Choice of the method",choices="None")
#   }else{
#     inFile<-input$mono.knitr.methode.file
#     data<-c("None",getSheets(loadWorkbook(inFile$datapath)))
#     selectizeInput("mono.knitr.methode.choice","Choice of the method",choices=data)
#   }
# })
Truc.mono<-reactive({ 
  validate(
    need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
  )
  data<-dataX.mono.pre.pre()
#   apply(data,1,function(x){paste0("Band ",sep="  , ")
  paste0("bande ",data[,1],"  , ",data[,2]," , ",data[,3]," , ",data[,4])
})
output$choice.band.mono.bef.1 <- renderUI({
  selectizeInput('name.band.mono.bef.1', 'Choice of the band 1', choices=Truc.mono()[!Not.Use()],width="1000px")
})
output$choice.band.mono.bef.2 <- renderUI({
  selectizeInput('name.band.mono.bef.2', 'Choice of the band 2', choices=Truc.mono()[!Not.Use()],width="1000px")
})
output$choice.band.m.comp.1 <- renderUI({
  selectizeInput('name.band.m.com.1', 'Choice of the bands to compare', choices=Truc.mono()[!Not.Use()],multiple=T)
})
output$choice.band.mono.aft.1 <- renderUI({
  selectizeInput('name.band.mono.aft.1', 'Choice of the band 1', choices=Truc.mono()[!Not.Use()],width="1000px")
})
output$choice.band.mono.aft.2 <- renderUI({
  selectizeInput('name.band.mono.aft.2', 'Choice of the band 2', choices=Truc.mono()[!Not.Use()],width="1000px")
})
output$choice.band.mono.bef.tot <- renderUI({
  selectizeInput('name.band.mono.bef.tot', 'Choice of the band to compare', choices=Truc.mono()[!Not.Use()],selected=NULL,
                 multiple=T,width='250%')
})
output$choice.band.mono.aft.tot <- renderUI({
  selectizeInput('name.band.mono.aft.tot', 'Choice of the band to compare', choices=Truc.mono()[!Not.Use()],selected=NULL,
                     multiple=T,width='250%')
})
# output$choice.band.mono.integration <- renderUI({
#   radioButtons('name.band.mono.integration', 'Choice of the band', choices=Truc.mono())
# })
output$mono.knitr.download = downloadHandler(
  filename = "HPTLC-report",
  content = function(file) {
#     send.mail(from = "dimitrifi@laposte.net",to = "dimitrifi@laposte.net",subject = "subject",body = "Body of the email",smtp = list(host.name = "smtp.laposte.net", port = 25, user.name = "dimitrifi", passwd = "Tigrou", ssl = TRUE, tls = TRUE),authenticate = TRUE,send = TRUE,attach.files=knit2pdf('inputMonoQuanti.Rnw', clean = TRUE))
    out = knit2pdf('inputMonoQuanti.Rnw', clean = TRUE)
    file.rename(out, file) # move pdf to file for downloading
  },
  contentType = 'application/pdf'
)

output$sessionInfo <- renderPrint({
  sessionInfo()
})

})

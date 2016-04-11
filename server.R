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



require("jpeg");require("png");require('tiff');require('caret');require('klaR');require('xlsx');
require("ChemometricsWithR");require("gplots");require("kohonen");require("devtools");
require("chemometrics");require("ggplot2");require("abind");require("plyr");require('dplyr');
require("prospectr");require("DiscriMiner");require("baseline");require("knitr");require('rmarkdown');
require("xtable");require("ptw");require("dtw");
require('d3heatmap');require('randomForest');require('kernlab');require('ipred');
require('extraTrees');require('evtree');require('FBN')

# require('EBImage')

# require('shinyRGL');require('rgl')

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

  output$manual.pdf <- downloadHandler(
    filename = "rTLC_manual.pdf",
    content = function(file) {
      file.copy('www/rTLC manual.pdf', file)
    }
  )
  output$help.global.pipeline <- renderImage({
    filename <- normalizePath(file.path('./www','Pipeline-total.jpg'))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = 'Alternate text')
  },deleteFile = F)
  output$help.predict.pipeline <- renderImage({
    filename <- normalizePath(file.path('./www','Pipeline-Prediction.jpg'))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = 'Alternate text')
  },deleteFile = F)

  output$checkpoint.1.download <- downloadHandler(
    filename = function(x){paste0(input$checkpoint.1.download.text,'.RData')},
    content = function(con) {
      assign("data",list(chrom = data.mono.2(),
                         batch = dataX.mono.pre(),
                         Vertical.dim = c(dim(data.mono.2())[2],input$hauteur.mono,input$Zf.mono,input$dist.bas.mono)
                         ))
      save(list="data", file=con)
    }
    )

  output$checkpoint.1.download.zip <- downloadHandler(
    filename = function(x){paste0(input$checkpoint.1.download.zip.text,'.zip')},
    content = function(file) {
      fs <- c()
      channel <- c(red=1,green=2,blue=3,grey=4)
      for(i in names(channel)){
        path <- paste0(i,'.csv')
        fs <- c(fs,path)
        write.csv(data.mono.2()[,dim(data.mono.2())[2]:1,channel[i]],file=path,row.names = F,col.names = F,sep=';')
      }
      path = paste0('batch','.csv')
      fs = c(fs,path)
      write.csv(dataX.mono.pre(),file=path)
      tempFile <- tempfile(fileext = ".zip")
      zip(zipfile=tempFile, files=fs)
      file.rename(tempFile, file)
    },
    contentType = "application/zip"
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
      if(colnames(data)[1] != 'id' & 'id' %in% colnames(data)){
        colnames(data)[which(colnames(data) == 'id')] <- 'id2'
      }
      if(!'id' %in% colnames(data)){
        data <- cbind(id=seq(nrow(data)),data)
      }
    }
    # data$id <- seq(nrow(data))
    rownames(data) <- seq(nrow(data))
    data
  })
  dataX.edited<-reactive({
    data <- dataX.editable()
    for(j in c(2:ncol(data))){
      truc <- c();for(i in seq(nrow(data))){truc <- c(truc,input[[paste0(colnames(data)[j],".",i)]])}
      data[,j] <- truc
    }
    validate(
      need(length(colnames(data)) >= 2, "Your batch must contain at least 1 columns"),
      need(colnames(data)[1] == "id", "The first column of your batch is not 'id'"),
      need(data[,1] == seq(1:nrow(data)) , "Your id column is not a sequence of number starting from 1")
    )
    data
  })
  dataX.mono.pre<-reactive({
    data<-dataX.edited()
    return(data[!Not.Use(),])
  })
  output$table1 <-renderTable({
      data <- dataX.editable()
      validate(
        need(length(colnames(data)) >= 2, "Your batch must contain at least 1 columns"),
        need(colnames(data)[1] == "id", "The first column of your batch is not 'id'"),
        need(data[,1] == seq(1:nrow(data)) , "Your id column is not a sequence of number starting from 1")
      )
    Not.Use <- paste0("<input id='Not.Use.", 1:nrow(data), "' class='shiny-bound-input' type='checkbox' value='1'>")
    for(i in c(2:ncol(data))){
      data[,i] <- paste0("<input id='",colnames(data)[i],'.', 1:nrow(data),"' class='shiny-bound-input' type='text' value='",data[,i],"'>")
    }
    data <- data.frame(cbind(Not.Use,data))
    return(data)
  }, sanitize.text.function = function(y) y)
  output$batch.Truc.mono <- renderUI({
    data <- colnames(dataX.edited())
    if(length(data) <= 4){
      checkboxGroupInput('batch.Truc.mono','Information to include in the track chromatograms plot',choices=data[2:length(data)],selected=data[2:length(data)],inline=T)
    }else{
      checkboxGroupInput('batch.Truc.mono','Information to include in the track chromatograms plot',choices=data[2:length(data)],selected=data[2:4],inline=T)
    }
  })
  output$batch.filter <- renderUI({
    data <- dataX.edited()
    truc <- tagList()
    for(i in c(2:ncol(data))){
      truc <- tagAppendChild(truc,
                             selectizeInput(paste0('batch.filter.',i),colnames(data)[i],multiple=T,choices=unique(as.character(data[,i])))
      )
    }
    truc
  })

  Not.Use <- reactive({
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    Not.Use <- c()
    for(i in seq(nrow(dataX.edited()))){
      Not.Use <- c(Not.Use, input[[paste0("Not.Use.",i)]])
    }
    data <- dataX.editable()
    for(i in c(2:ncol(data))){
      if(length(input[[paste0('batch.filter.',i)]]) != 0){
        Not.Use[!data[,i] %in% input[[paste0('batch.filter.',i)]]] <- T
      }
    }
    return(Not.Use)
  })


  ################ data.mono.1   ##########
  output$slider.subset.height<-renderUI({
    sliderInput('slider.subset.height','Subset to extract',min=0.01,max=input$hauteur.mono,value=c(0,input$hauteur.mono),
                step=0.01)
  })

  output$TableDimensionVerticale <-renderTable({
    if(input$filedemouse == 'QC'){Default <- Pred.upload.model()$Vertical.dim}
    if(input$filedemouse == 'checkpoint'){Default <- checkpoint.data()$Vertical.dim}
    if(input$filedemouse != 'QC' & input$filedemouse != 'checkpoint'){Default <- c(128,100,70,8)}
    data <- data.frame(Option = c('Pixel height','Plate height','Retention Front','Bottom distance'),
                       Value = c('redim.height','hauteur.mono','Zf.mono','dist.bas.mono'),
                       Default = Default
    )
    if(input$filedemouse == 'QC'| input$filedemouse == 'checkpoint'){
      data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='number' readonly='readonly' value='",data$Default,"'>")
      data[,c(1,2)]
    }else{
      data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='number'  value='",data$Default,"'>")
      data[,c(1,2)]
    }

  },include.rownames=F,include.colnames=F, sanitize.text.function = function(y) y)

#   output$TablePicturePreprocess.1 <-renderTable({
#     if(input$filedemouse == 'QC'){if('PicturePreprocess' %in% names(Pred.upload.model())){Default <- Pred.upload.model()$PicturePreprocess}else{Default <- c(1,1,0,0)}}
#     if(input$filedemouse == 'checkpoint'){if('PicturePreprocess' %in% names(checkpoint.data())){Default <- checkpoint.data()$PicturePreprocess}else{Default <- c(1,1,0,0)}}
#     if(input$filedemouse != 'QC' & input$filedemouse != 'checkpoint'){Default <- c(1,1,0,0)}
#     data <- data.frame(Option = c('Gamma','Medianfilter','Lowpass','Highpass'),
#                        Value = c('Picture.gamma','Picture.medianfilter','Picture.lowpass','Picture.highpass'),
#                        Default = Default
#     )
#     data <- data[1:2,]
#     if(input$filedemouse == 'QC'| input$filedemouse == 'checkpoint'){
#       data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='number' readonly='readonly'  value='",data$Default,"'>")
#       #       data[2,3] <- paste0("<input id='",data[2,3],"' class='shiny-bound-input' type='number' readonly='readonly' value='",gsub(1,'checked',data[3,3]),"'>")
# #       data[2,4] <- paste0("<input id='",data[2,4],"' class='shiny-bound-input' type='number' readonly='readonly' value='",gsub(1,'checked',data[3,4]),"'>")
#       data[,c(1,2)]
#     }else{
#       data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='number'  value='",data$Default,"'>")
# #       data$Value[3] <- paste0("<input id='",data$Value[3],"' class='shiny-bound-input' type='number'  value='",gsub(1,'checked',data$Default[3]),"'>")
# #       data$Value[4] <- paste0("<input id='",data$Value[4],"' class='shiny-bound-input' type='number'  value='",gsub(1,'checked',data$Default[4]),"'>")
#       data[,c(1,2)]
#     }
#
#   },include.rownames=F,include.colnames=F, sanitize.text.function = function(y) y)

#   output$TablePicturePreprocess.2 <-renderTable({
#     if(input$filedemouse == 'QC'){if('PicturePreprocess' %in% names(Pred.upload.model())){Default <- Pred.upload.model()$PicturePreprocess}else{Default <- c(1,1,0,0)}}
#     if(input$filedemouse == 'checkpoint'){if('PicturePreprocess' %in% names(checkpoint.data())){Default <- checkpoint.data()$PicturePreprocess}else{Default <- c(1,1,0,0)}}
#     if(input$filedemouse != 'QC' & input$filedemouse != 'checkpoint'){Default <- c(1,1,0,0)}
#     data <- data.frame(Option = c('Gamma','Medianfilter','Lowpass','Highpass'),
#                        Value = c('Picture.gamma','Picture.medianfilter','Picture.lowpass','Picture.highpass'),
#                        Default = Default
#     )
#     data <- data[3:4,]
#     data$Default <- gsub(T,'checked',data$Default)
#     if(input$filedemouse == 'QC' | input$filedemouse == 'checkpoint'){
#       data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='checkbox' readonly='readonly'  value='",data$Default,"'>")
#       data[,c(1,2)]
#     }else{
#       data$Value <- paste0("<input id='",data$Value,"' class='shiny-bound-input' type='checkbox'  value='",data$Default,"'>")
#       data[,c(1,2)]
#     }
#
#   },include.rownames=F,include.colnames=F, sanitize.text.function = function(y) y)

#   PicturePreprocess <- reactive({
#     c(input$Picture.gamma,input$Picture.medianfilter,input$Picture.lowpass,input$Picture.highpass)
#   })

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
    inFile <- input$TableDimensionUpload
    if(!is.null(inFile)){
      data.saved <- read.xlsx(as.character(inFile[1,4]),sheetIndex=1)
      validate(
        need(nrow(data.saved) == truc, "There is not the same number of row in your saved data than the number of pictures")
      )
      data <- data.saved
    }
    for(i in c(1:ncol(data))){
      data[,i] <- paste0("<input id='",colnames(data)[i],'.', 1:nrow(data),"' class='shiny-bound-input' type='number'  value='",data[,i],"'>")
    }
    data
  }, sanitize.text.function = function(y) y)

  output$TableDimensionSave <- downloadHandler(
    filename = function(x){paste0(input$TableDimensionSave.text,'.xlsx')},
    content = function(file) {
      write.xlsx(TableDimension(),file=file,row.names = F)
    }
  )

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

  output$TableDimensionPlot <- renderImage({
    outfile <- tempfile(fileext='.png')
    png(outfile, width=1000, height=500)

    plot(x=seq(200),y=seq(1,100,length.out = 200),type='n',yaxt='n',xlab='Horizontale Dimensions',ylab='',main='Illustration of the chromatograms extraction')
    text(x=50,y=85,labels='Plate width = 200 mm',cex=1,col='black')
    text(x=100,y=97,labels='Unnecessary cropping will result in false horizontal dimensions and reproductibility problems',cex=1.5,col='red')
    arrows(x0=0, y0=90, x1 =200,col='black',code=3)

    text(x=100,y=30,labels='LINOMAT CONVENTION',cex=1.5)
    text(x=100,y=25,labels='Calculation from the exterior of the band',cex=1.5)
    segments(x0=-10,x1=160,y0=40)
    segments(x0=-10,x1=160,y0=80)
    segments(x0=160,y0=0,y1=80)
    segments(x0=20,y0=8,y1=8,x1=28,lwd=5)
    segments(x0=38,y0=8,y1=8,x1=46,lwd=5)
    segments(x0=56,y0=8,y1=8,x1=64,lwd=5)
    segments(x0=74,y0=8,y1=8,x1=82,lwd=5)
    segments(x0=92,y0=8,y1=8,x1=100,lwd=5)
    segments(x0=110,y0=8,y1=8,x1=118,lwd=5)
    segments(x0=128,y0=8,y1=8,x1=136,lwd=5)
    segments(x0=146,y0=8,y1=8,x1=154,lwd=5)
    segments(x0=164,y0=8,y1=8,x1=172,lwd=5)
    text(x=20,y=5,labels='Left distance = 20 mm',cex=1,col='green')
    arrows(x0=0, y0=8, x1 =20,col='green',code=3,length=0.1)
    text(x=30,y=15,labels='Band width = 8 mm',cex=1,col='blue')
    arrows(x0=20, y0=10, x1 =28,col='blue',code=3,length=0.1)
    text(x=120,y=15,labels='Gap between band  = 10 mm',cex=1,col='red')
    arrows(x0=100, x1=110, y0 =10,col='red',code=3,length=0.1)
    text(x=100,y=70,labels='ATS-4 CONVENTION',cex=1.5)
    text(x=100,y=65,labels='Calculation from the middle of the band',cex=1.5)
    segments(x0=20,y0=48,y1=48,x1=28,lwd=5)
    segments(x0=38,y0=48,y1=48,x1=46,lwd=5)
    segments(x0=56,y0=48,y1=48,x1=64,lwd=5)
    segments(x0=74,y0=48,y1=48,x1=82,lwd=5)
    segments(x0=92,y0=48,y1=48,x1=100,lwd=5)
    segments(x0=110,y0=48,y1=48,x1=118,lwd=5)
    segments(x0=128,y0=48,y1=48,x1=136,lwd=5)
    segments(x0=146,y0=48,y1=48,x1=154,lwd=5)
    segments(x0=164,y0=48,y1=48,x1=172,lwd=5)
    text(x=30,y=42,labels='First Application Position = 24 mm',cex=1,col='green')
    arrows(x0=0, y0=45, x1 =24,col='green',code=3,length=0.1)
    text(x=30,y=55,labels='Band length = 8 mm',cex=1,col='blue')
    arrows(x0=20, y0=50, x1 =28,col='blue',code=3,length=0.1)
    text(x=120,y=55,labels='Distance between tracks  = 18 mm',cex=1,col='red')
    arrows(x0=96, x1=114, y0 =50,col='red',code=3,length=0.1)
    segments(x0=166,y0=0,x1=166,y1=80,col='red')
    segments(x0=170,y0=0,x1=170,y1=80,col='green')
    text(x=185,y=60,labels='The software will \nextract the mean pixels\n between each red\n and green vertical line\non each \'channel\'\n of the picture')
    text(x=185,y=20,labels='Tolerance = 2 mm\nRemove 2 mm from\n the exterior of the band')
    dev.off()
    list(src = outfile,
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  output$select.image.redim.mono<-renderUI({
    truc <- paste(seq(nrow(inFile.photo())),inFile.photo()$name,sep="  -  ")
    selectizeInput("select.image.redim.mono","Plate choice",choices=truc)
  })
  output$image.redim.mono <- renderImage({
    validate(
      need(input$Zf.mono != 0, "The retention front could not be 0 mm, please verify the Verticale dimension table")
    )
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
    png(outfile, width=600, height=300)
    par(mar=c(0,0,0,0))
    plot(c(0,largeur),c(0,input$hauteur.mono), type='n',ylab="",xlab="",bty='n')
    rasterImage(f.read.image(as.character(inFile[n.pic,4]),native=T,input$mono.Format.type,height=0),
                0 , 0, largeur, input$hauteur.mono)
    for(i in seq(nbr.band)){
      text(x=(dist.gauche+tolerance+(i-1)*(band+ecart)),y=input$hauteur.mono*0.9,labels=i,col="red",cex=1)
      abline(v=dist.gauche+tolerance+(i-1)*(band+ecart),col="red")
      abline(v=dist.gauche-tolerance+band+(i-1)*(band+ecart),col="green")

      abline(h=input$Zf.mono,col='white')
      abline(h=input$dist.bas.mono,col='white')
    }
    dev.off()
    list(src = outfile,
         contentType = 'image/png',
#          width = 600,
#          height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  output$select.image.reconstruct<-renderUI({
    truc <- paste(seq(nrow(inFile.photo())),inFile.photo()$name,sep="  -  ")
    tagList(
      selectizeInput("select.image.reconstruct","Plate choice",choices=truc),
      numericInput('select.image.reconstruct.track','band to compare with chromatogram',1)
      )
  })

  output$image.reconstruct <- renderImage({
    n.pic<-as.numeric(substr(input$select.image.reconstruct,1,3))
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
    png(outfile, width=800, height=1800)
    par(mar=c(5,4,0,0),mfrow=c(2,1))
    plot(c(0,largeur),c(0,input$hauteur.mono*2), type='n',ylab="",xlab="",bty='n')
    rasterImage(f.read.image(as.character(inFile[n.pic,4]),native=T,input$mono.Format.type,height=0),
                0 , 0, largeur, input$hauteur.mono)
    image <- f.read.image(as.character(inFile[n.pic,4]),native=F,input$mono.Format.type,height=input$redim.height)
    data <- f.eat.image(image,largeur,dist.gauche,band,ecart,tolerance)
    for(i in seq(nbr.band)){
      abline(v=dist.gauche+tolerance+(i-1)*(band+ecart),col="red")
      abline(v=dist.gauche-tolerance+band+(i-1)*(band+ecart),col="green")
      data2<-f.rebuilt(data[i,,1],data[i,,2],data[i,,3])
      rasterImage(data2,dist.gauche+tolerance+(i-1)*(band+ecart),input$hauteur.mono,dist.gauche-tolerance+band+(i-1)*(band+ecart),input$hauteur.mono*2)
    }
    id <- input$select.image.reconstruct.track
    f.plot.array(data,id=id,label=NULL,input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,ylim.raster = 1.6)
    a<-dim(image)
    rasterImage(aperm(image[a[1]:1,(a[2]/largeur*((dist.gauche+tolerance)+(id-1)*(band+ecart))):(a[2]/largeur*((dist.gauche+band-tolerance)+(id-1)*(band+ecart))),],c(2,1,3)),
                RF.min() , 1.4, RF.max(), 1.6)
    dev.off()
    list(src = outfile,
         contentType = 'image/png',
         #          width = 600,
         #          height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)

  ################ data.mono.2   ##########
  data.mono.1.1<-reactive({
    validate(
      need(input$Zf.mono != 0, "The retention front could not be 0 mm, please verify the Verticale dimension table")
    )
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
    dataX<-dataX.edited()
    validate(
      need(dim(data)[1] == nrow(dataX), "The number of chromatograms extracted do not match the number of row in your batch, please check your batch or your dimension table")
    )
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
  output$ptw.warp.ref <- renderUI({
    choices <- Truc.mono()[Train.partition()]
    selectizeInput('ptw.warp.ref','Select the track to use as reference',choices=choices)
  })
  output$ptw.warp.ref.bis <- renderUI({
    choices <- Truc.mono()[Train.partition()]
    selectizeInput('ptw.warp.ref','Select the track to use as reference',choices=choices)
  })
  Preprocess.options <- reactive({
    if(input$filedemouse != 'QC'){
      data <- data.mono.2()
      Smoothing <- list(window.size = input$window.size,poly.order=input$poly.order,diff.order=input$diff.order)
      if(input$warpmethod == 'ptw'){
        Warping <- list(warpmethod = input$warpmethod,
                        ptw.warp.ref = as.numeric(input$ptw.warp.ref)
        )
      }
      if(input$warpmethod == 'dtw'){
        Warping <- list(warpmethod = input$warpmethod,
                        dtw.warp.ref = as.numeric(input$ptw.warp.ref),
                        dtw.split = input$dtw.split
        )
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
      return(list(Smoothing=Smoothing,Warping=Warping,Baseline.correction=Baseline,
                  medianFilter=input$preprocess.medianfilter,gammaCorrection=input$preprocess.gammacorrection))
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
  output$VS_slider_1 <- renderUI({sliderInput("VS_slider_1", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_2 <- renderUI({sliderInput("VS_slider_2", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_3 <- renderUI({sliderInput("VS_slider_3", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_4 <- renderUI({sliderInput("VS_slider_4", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_5 <- renderUI({sliderInput("VS_slider_5", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_6 <- renderUI({sliderInput("VS_slider_6", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_7 <- renderUI({sliderInput("VS_slider_7", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_8 <- renderUI({sliderInput("VS_slider_8", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_9 <- renderUI({sliderInput("VS_slider_9", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_10 <- renderUI({sliderInput("VS_slider_10", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_11 <- renderUI({sliderInput("VS_slider_11", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_12 <- renderUI({sliderInput("VS_slider_12", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_13 <- renderUI({sliderInput("VS_slider_13", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_14 <- renderUI({sliderInput("VS_slider_14", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_15 <- renderUI({sliderInput("VS_slider_15", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_16 <- renderUI({sliderInput("VS_slider_16", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_17 <- renderUI({sliderInput("VS_slider_17", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_18 <- renderUI({sliderInput("VS_slider_18", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_19 <- renderUI({sliderInput("VS_slider_19", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_20 <- renderUI({sliderInput("VS_slider_20", label = NULL, min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  output$VS_slider_score.loading <- renderUI({sliderInput("VS_slider_score.loading", label = 'Not working yet', min=RF.min(),max=RF.max(),value=c(RF.min(),RF.max()),step = 0.001)})
  selection.table <- reactive({
    if(input$filedemouse != 'QC'){
      n <- 20
      validate(
        need(!is.null(input[[paste0('VS_slider_',1)]]), "Please visit the variable selection table")
      )
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
    plot(c(min,max),c(1,20),type='n',xlab = 'Rf',ylab = 'index',main='Scheme of the varaible selection')
    selection$channel <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',selection$channel))))

    for(i in seq(20)){
      if(selection[i,1] == T){
        arrows(x0=selection[i,3], y0=i, x1 =selection[i,4],col=selection[i,2],code=3,length=0.1)
      }
    }
    plot(x=seq(dim(data.mono.4())[2]),data.mono.4()[1,dim(data.mono.4())[2]:1],type='l',main='Result for the first sample',xlan='index',ylab='intensity')
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
    par(mgp=c(2,0.75,0),mar=c(5, 4, 2, 0.5))
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    data <- data.mono.2()
    n.band<-as.numeric(input$name.band.mono.bef.1)
    f.plot.array(data,n.band,names(Truc.mono()),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,inverse=MAC.inverse,cex=1.5)
    abline(v=input$z.min,col=5)
    abline(v=input$z.max,col=7)
  },height = 400,width=700)

  output$plot.v.mono.bef.2 <- renderPlot({
    par(mgp=c(2,0.75,0),mar=c(5, 4, 2, 0.5),cex.lab=1.5)
    validate(
      need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
    )
    data <- data.mono.2()
    n.band<-as.numeric(input$name.band.mono.bef.2)
    f.plot.array(data,n.band,names(Truc.mono()),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,inverse=MAC.inverse,cex=1.5)
    abline(v=input$z.min,col=5)
    abline(v=input$z.max,col=7)
  },height = 400,width=700)

  output$plot.v.mono.aft.1 <- renderPlot({
    par(mgp=c(2,0.75,0),mar=c(5, 4, 2, 0.5),cex.lab=1.5)
    data <- data.mono.3()
    n.band<-as.numeric(input$name.band.mono.aft.1)
    f.plot.array(data,n.band,names(Truc.mono()),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,reconstruct=F,cex=1.5)
    abline(h=0)
  },height = 400,width=700)
  output$plot.v.mono.aft.2 <- renderPlot({
    par(mgp=c(2,0.75,0),mar=c(5, 4, 2, 0.5),cex.lab=1.5)
    data <- data.mono.3()
    n.band<-as.numeric(input$name.band.mono.aft.2)
    f.plot.array(data,n.band,names(Truc.mono()),input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,reconstruct=F,cex=1.5)
    abline(h=0)
  },height = 400,width=700)

output$plot.v.mono.bef.tot <- renderPlot({
  validate(
    need(length(input$name.band.mono.bef.tot) > 1,"Select at least 2 band for comparaison")
    )
  n.band<-as.numeric(input$name.band.mono.bef.tot)
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  data <- data.mono.2()
  par(mar=c(5,4,4,1), mfrow=c(4,1))
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,1])),
          lty=1,type="l",main="Red channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,2])),
          lty=1,type="l",main="Green channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,3])),
          lty=1,type="l",main="Blue channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,4])),
          lty=1,type="l",main="Grey channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
},height = 1200,width=800)

output$plot.v.mono.aft.tot <- renderPlot({
  validate(
    need(length(input$name.band.mono.aft.tot) > 1,"Select at least 2 band for comparaison")
  )
  n.band<-as.numeric(input$name.band.mono.aft.tot)
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  data <- data.mono.3()
  par(mar=c(5,4,4,1), mfrow=c(4,1))
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,1])),
          lty=1,type="l",main="Red channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,2])),
          lty=1,type="l",main="Green channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,3])),
          lty=1,type="l",main="Blue channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
  matplot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=t(as.matrix(data[n.band,,4])),
          lty=1,type="l",main="Grey channel",xlab=expression("R"['F']),ylab="intensity", col = seq(length(n.band)))
  legend("topright", legend=names(Truc.mono()[n.band]) , col = seq(length(n.band)),pch="*")
},height = 1200,width=800)
output$image.comparaison.1 <- renderPlot({
  validate(
    need(length(input$name.band.m.com.1) > 1,"Select at least 2 band for comparaison")
  )
  data<-data.mono.2()
  band<-as.numeric(input$name.band.m.com.1)
  plot(c(0,length(band)),c(0,10), type='n',ylab="",xlab="",xaxt = "n",yaxt = "n")
  for(i in seq(band)){
    data2<-f.rebuilt(data[band[i],,1],data[band[i],,2],data[band[i],,3])
    rasterImage(data2,i-1,0,i,10)
    text(x=i-0.5,y=9,labels=band[i],col="red",cex=2)
    par(new=T)
  }
})
# ################ model pca ##########
model.pca<-reactive({
  data <- data.mono.4()
  PCA(as.matrix(data))
})
# ################# output$pca.plot.1 #################
pca.plot.1<-reactive({
  data<-model.pca()
#   label.color <- paste(input$col.pca,collapse=', ')
#   label.color <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',label.color))))
  xlabel<-paste0(input$PCA.comp.a,' (',round(data$var[as.numeric(substr(input$PCA.comp.a,3,3))]/data$totalvar*100,2),"%)")
  ylabel<-paste0(input$PCA.comp.b,' (',round(data$var[as.numeric(substr(input$PCA.comp.b,3,3))]/data$totalvar*100,2),"%)")
  data<-scores(data,npc=10)
  colnames(data)<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
  data<-data[,c(input$PCA.comp.a,input$PCA.comp.b)]
  colnames(data)<-c("PC1","PC2")
  data<-cbind(dataX.mono.pre(),data)
  plot<-ggplot()+geom_point(data=data,aes(x=PC1,y=PC2),size=as.numeric(input$cex.pca))+
    labs(x=xlabel, y=ylabel)
  if(input$shape.plot.pca != "None"){
    validate(
      need(length(unique(data[,input$shape.plot.pca])) < 6, "The number of factor in your shape variable must be lower than 6")
    )
  }
  if(input$shape.plot.pca != "None" & input$col.plot.pca == "None"){
    data$Shape<-data[,input$shape.plot.pca]
    plot<-ggplot()+geom_point(data=data,aes(x=PC1,y=PC2,shape=Shape),size=as.numeric(input$cex.pca))+
      labs(x=xlabel, y=ylabel)
  }
  if(input$col.plot.pca != "None" & input$shape.plot.pca == "None"){
    data$Color<-data[,input$col.plot.pca]
    plot<-ggplot()+geom_point(data=data,aes(x=PC1,y=PC2,col=Color),size=as.numeric(input$cex.pca))+
      labs(x=xlabel, y=ylabel)
  }
  if(input$col.plot.pca != "None" & input$shape.plot.pca != "None"){
    data$Color<-data[,input$col.plot.pca]
    data$Shape<-data[,input$shape.plot.pca]
    plot<-ggplot()+geom_point(data=data,aes(x=PC1,y=PC2,col=Color,shape=Shape),size=as.numeric(input$cex.pca))+
      labs(x=xlabel, y=ylabel)
  }
#   if(input$plotlyPCA==T){
#     p <- plotly(username=input$plot.ly.user, key=input$plot.ly.key)
#     p$ggplotly(plot)
#   }
  if(input$label.plot.pca != "None"){
    data$Label<-data[,input$label.plot.pca]
    plot<-plot+geom_text(data=data,aes(x=PC1,y=PC2,label=Label),hjust=as.numeric(input$hjust.pca),vjust=as.numeric(input$vjust.pca))
  }
  if(input$pca.ellipse == T){plot <- plot+ stat_ellipse(data=data,aes(x=PC1,y=PC2,col=Color),level=input$pca.ellipse.level)}
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
  colnames(data)<-c("PC1","PC2","PC3","PC4")
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
  RF <- colnames(data.mono.4())
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
  RF <- colnames(data.mono.4())
  print(RF[pick.peaks(data, input$pca.loading.local.maxima.span)])
})

output$pca.plot.score.loading <- renderPlot({
  dataX <- dataX.mono.pre()[,input$col.plot.pca]
  par(xpd=T)
  par(mfrow=c(4,3),mar=c(5,4,4,6),oma = c(0, 0, 3, 0))
  hauteur<-input$hauteur.mono
  dist.bas<-input$dist.bas.mono
  Zf <- input$Zf.mono
  maxi <- (hauteur-dist.bas)/(Zf-dist.bas)
  mini <- -dist.bas/(Zf-dist.bas)
  color <- c('red','green','blue','grey')
  for(i in seq(4)){
    data <- data.mono.3()[,,i]
    model <- PCA(data)
    scoreplot(model,col=factor(dataX),main=paste0(color[i],' channel'))
    legend("topright", inset=c(-0.5,0),legend=unique(factor(dataX)),pch=1,col=unique(factor(dataX)))
    data <- loadings.PCA(model)[,1]
    RF = seq(maxi,mini,length.out=length(data))
    plot(x=RF, xaxt = "n",
         y=as.matrix(data),type="l",main=paste0("Loading plot: PC1: ",round(model$var[1]/model$totalvar*100,1),'%'),xlab=expression("R"['F']),ylab="intensity")
    axis(side = 1, at = round(seq(maxi,mini,length.out=(maxi-mini)*10),2))
    data <- loadings.PCA(model)[,2]
    RF = seq(maxi,mini,length.out=length(data))
    plot(x=RF, xaxt = "n",
         y=as.matrix(data),type="l",main=paste0("Loading plot: PC2: ",round(model$var[2]/model$totalvar*100,1),'%'),xlab=expression("R"['F']),ylab="intensity")
    axis(side = 1, at = round(seq(maxi,mini,length.out=(maxi-mini)*10),2))
  }
  mtext(input$pca.plot.score.loading.title, outer = TRUE, cex = 1.5)
})
output$pca.plot.score.loading.title <- renderUI({
  textInput('pca.plot.score.loading.title','Title of the plot',paste0('Preprocess: \n',paste0(input$Preprocess.order,collapse='; ')))
})

# output$myWebGL.1 <- renderWebGL({ ## NOT WORKING
#   data<-model.pca()
#   data<-scores(data,npc=3)
#   colnames(data)<-c("PC1","PC2","PC3")
#   data<-cbind(dataX.mono.pre(),data)
#   # dataX <- dataX.mono.pre()
#   data$Color <- data$Drug
#   text3d(data$PC1, data$PC2,data$PC3,text=data$id,col=rainbow(length(levels(factor(data$Color))))[factor(data$Color)])
#   axes3d()
#   title3d(xlab="PC1",ylab="PC2",zlab="PC3")
# })


################# Outliers PCA #################
Moutlier.pca.1<-reactive({Moutlier(scores(model.pca(),npc=10)[,as.numeric(input$comp.outlier.pca.1)],quantile = input$quantile.outlier.pca.1, plot=F)})
output$quantile.outlier.pca.1<-renderPlot({
  par(mfrow=c(1,2))
  plot( x = as.numeric(dataX.mono.pre()[,1]),#rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$md,
        pch = '', xlab = 'Index of the observations',  ylab = 'Classical Mahalanobis distance' )
  text( x = as.numeric(dataX.mono.pre()[,1]),#rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$md,
        labels =  as.numeric(dataX.mono.pre()[,1])
        )
  abline(h=Moutlier.pca.1()$cutoff)

  plot( x = as.numeric(dataX.mono.pre()[,1]),#rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$rd,
        pch = '', xlab = 'Index of the observations',  ylab = 'Robust Mahalanobis distance' )
  text( x = as.numeric(dataX.mono.pre()[,1]),#rownames(scores(model.pca(),npc=2)),
        y = Moutlier.pca.1()$rd,
        labels = as.numeric(dataX.mono.pre()[,1])
        )
  abline(h=Moutlier.pca.1()$cutoff)
})
output$quantile.outlier.pca.2<-renderPrint({
  Moutlier(scores(model.pca(),npc=10)[,as.numeric(input$comp.outlier.pca.1)],quantile = input$quantile.outlier.pca.1)
})

################# Cluster #################
# Method 1 : Ward Hierarchical Clustering
data.cluster.1<-reactive({
  data <- data.mono.4()
  return(data)
})
plot.cluster.1.1 <- reactive({
  data<-data.cluster.1()
  if(length(input$Var.cluster.1) == 0){rownames(data)<-dataX.mono.pre()[,"id"]}
  if(length(input$Var.cluster.1) == 1){rownames(data)<-dataX.mono.pre()[,input$Var.cluster.1]}
  if(length(input$Var.cluster.1) > 1){rownames(data)<-apply(dataX.mono.pre()[,input$Var.cluster.1],1,paste0,collapse=" - ")}
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
  checkboxGroupInput("Var.cluster.1","Choice of the variable for the cluster plot",choices=colnames(dataX.mono.pre()),selected=colnames(dataX.mono.pre())[1])
})

################# output$heatmap #################
data.heatmap<-reactive({
  data <- data.mono.4()
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
  radioButtons("Train.column","Choice of the variable",choices=colnames(dataX.mono.pre())[2:length(colnames(dataX.mono.pre()))])
})
output$Train.model.algo.info <- renderPrint({
  getModelInfo()[input$Train.model.algo]
})

output$Train.model.algo.wiki <- renderUI({
  if(input$Train.model.algo == 'pls'){href <- 'https://en.wikipedia.org/wiki/Partial_least_squares_regression'}
  if(input$Train.model.algo == 'lda'){href <- 'https://en.wikipedia.org/wiki/Linear_discriminant_analysis'}
  if(input$Train.model.algo == 'rf'){href <- 'https://en.wikipedia.org/wiki/Random_forest'}
  if(input$Train.model.algo == 'pcr'){href <- 'https://en.wikipedia.org/wiki/Principal_component_regression'}
  if(input$Train.model.algo == 'rpart'){href <- 'https://en.wikipedia.org/wiki/Decision_tree_learning'}
  if(input$Train.model.algo == 'svmLinear2' | input$Train.model.algo == 'svmPoly'){href <- 'https://en.wikipedia.org/wiki/Support_vector_machine'}
  helpText(   a("Click Here to learn about this algorythm",target="_blank",
                href=href)
  )
})

Train.Ind <- reactive({
  data <- data.mono.4()
  return(as.matrix(data))
})
Train.Dep <- reactive({
  data <- dataX.mono.pre()[,input$Train.column]
  if(input$Trainproblem == 'classification'){
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
output$Train.metric.positive.class <- renderUI({
  h5(paste0('Positive class will be: ',dataX.mono.pre()[1,input$Train.column]))
})
output$Train.metric <- renderUI({
  if(input$Trainproblem == 'classification' & length(unique(Train.Dep())) == 2){
    truc <- c('Accuracy','Kappa','Specificity','Sensitivity','Pos_Pred_Value','Neg_Pred_Value','Detection_Rate','Balanced_Accuracy')
  }
  if(input$Trainproblem == 'classification'& length(unique(Train.Dep())) > 2){
    truc <- c('Accuracy','Kappa','Mean_Sensitivity','Mean_Specificity','Mean_Pos_Pred_Value','Mean_Neg_Pred_Value','Mean_Detection_Rate','Mean_Balanced_Accuracy')
    names(truc) <- c('Accuracy','Kappa','Specificity','Sensitivity','Pos_Pred_Value','Neg_Pred_Value','Detection_Rate','Balanced_Accuracy')
  }
  if(input$Trainproblem == 'regression'){
    truc <- c('RMSE','Rsquared')
  }
  selectizeInput('Train.metric','what summary metric will be used to select the optimal mode',choices=truc)
})
output$Train.model.algo <- renderUI({
  caret.table <- cbind(
    llply(getModelInfo(),function(l){l$label}),
    llply(getModelInfo(),function(l){l$library}),
    llply(getModelInfo(),function(l){l$prob}),
    llply(getModelInfo(),function(l){l$type})
  )
  Train.model.algo.choice <- names(caret.table[,1])
  names(Train.model.algo.choice) <- caret.table[,1]
  Train.model.algo.choice <- Train.model.algo.choice[names(caret.table[,1]) %in% c('rf','pls','lda','svmLinear2','svmPoly','rpart','pcr')]
  selectizeInput("Train.model.algo",'Choice of the algorythm',choices= Train.model.algo.choice,selected='rf')
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
#     eval(parse(text=paste0('control <- trainControl(method = input$Train.control.method,
#                             number=input$Train.tunning.CV,
#                             repeats=input$Train.tunning.repeat,
#                             savePredictions = "final",
#                             summaryFunction = ',input$Traincontrolsummaryfunction,',allowParallel=T,verboseIter=T)
#                            '
#     ))) #
    if(input$Trainproblem == 'classification'){
      control <- trainControl(method = input$Train.control.method,
                              number=input$Train.tunning.CV,
                              repeats=input$Train.tunning.repeat,
                              savePredictions = "final",
                              summaryFunction = multiClassSummary,
                              allowParallel=T,verboseIter=T,returnData=F)
    }
    if(input$Trainproblem == 'regression'){
      control <- trainControl(method = input$Train.control.method,
                              number=input$Train.tunning.CV,
                              repeats=input$Train.tunning.repeat,
                              savePredictions = "final",
                              summaryFunction = defaultSummary,
                              allowParallel=T,verboseIter=T,returnData=F)
    }
    set.seed(1)
    model <- train(Dep ~. , data = training,
                   method=input$Train.model.algo,
                   tuneGrid = Train.model.grid.edit(),
                   metric = input$Train.metric,
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

output$TrainValidMetricsClassTable <- renderTable({
  if(input$TrainValidMetricsUse == 'Cross-validation data'){
    x <- Train.model()$pred$obs
    y <- Train.model()$pred$pred
  }
  if(input$TrainValidMetricsUse == 'Training data'){
    x <- Train.Dep()[Train.partition() == T]
    y<-Train.prediction()[Train.partition() == T]
  }
  if(input$TrainValidMetricsUse == 'Test data'){
    x <- Train.Dep()[Train.partition() == F]
    y<-Train.prediction()[Train.partition() == F]
  }
  table(x,y)
})
output$TrainValidMetricsClassPrint <- renderPrint({
  if(input$TrainValidMetricsUse == 'Cross-validation data'){
    x <- Train.model()$pred$obs
    y <- Train.model()$pred$pred
  }
  if(input$TrainValidMetricsUse == 'Training data'){
    x <- Train.Dep()[Train.partition() == T]
    y<-Train.prediction()[Train.partition() == T]
  }
  if(input$TrainValidMetricsUse == 'Test data'){
    x <- Train.Dep()[Train.partition() == F]
    y<-Train.prediction()[Train.partition() == F]
  }
  confusionMatrix(x,y)
})
output$TrainValidMetricsRegPlot <- renderPlot({
  if(input$TrainValidMetricsUse == 'Cross-validation data'){
    x <- Train.model()$pred$obs
    y <- Train.model()$pred$pred
  }
  if(input$TrainValidMetricsUse == 'Training data'){
    x <- Train.Dep()[Train.partition() == T]
    y<-Train.prediction()[Train.partition() == T]
  }
  if(input$TrainValidMetricsUse == 'Test data'){
    x <- Train.Dep()[Train.partition() == F]
    y<-Train.prediction()[Train.partition() == F]
  }
  plot(x=x,y=y,xlab='Observation',ylab='Prediction',
       main=paste0('Regression Curve: ',input$TrainValidMetricsUse,'\n','R2 = ',cor(x,y)^2,' - RMSE = ',RMSE(x,y)))
})


output$Train.tunning.plot <- renderPlot({
  print(plot(Train.model()))
})
output$Train.down.model.text <- renderUI({
  value <- paste0(input$Train.model.algo,paste(Preprocess.order(),collapse='-'))
  textInput('Train.down.model.text','filename',value)
})

output$Train.down.model <- downloadHandler(
  filename = function(x){paste0(input$Train.down.model.text,'.Rdata')},
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
checkpoint.data <- reactive({
  validate(
    need(input$checkpoint.1.upload != "", "Please upload your saved Rdata file")
  )
  inFile <- input$checkpoint.1.upload
  load(inFile$datapath)
  return(data)
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
Truc.mono<-reactive({
  validate(
    need(input$Not.Use.1 != "", "Please visit the batch tab in Data Input to choose the data you want to Use")
  )
  data<-dataX.mono.pre()[,c('id',input$batch.Truc.mono)]
  truc <- seq(nrow(data))
  names(truc) <- paste0("track ",apply(data,1,paste0,collapse='  - '))
  truc
})
output$choice.band.mono.bef.1 <- renderUI({
  selectizeInput('name.band.mono.bef.1', 'Choice of the band 1', choices=Truc.mono(),width="1000px")
})
output$choice.band.mono.bef.2 <- renderUI({
  selectizeInput('name.band.mono.bef.2', 'Choice of the band 2', choices=Truc.mono(),width="1000px")
})
output$choice.band.m.comp.1 <- renderUI({
  selectizeInput('name.band.m.com.1', 'Choice of the bands to compare', choices=Truc.mono(),multiple=T)
})
output$choice.band.mono.aft.1 <- renderUI({
  selectizeInput('name.band.mono.aft.1', 'Choice of the band 1', choices=Truc.mono(),width="1000px")
})
output$choice.band.mono.aft.2 <- renderUI({
  selectizeInput('name.band.mono.aft.2', 'Choice of the band 2', choices=Truc.mono(),width="1000px")
})
output$choice.band.mono.bef.tot <- renderUI({
  selectizeInput('name.band.mono.bef.tot', 'Choice of the band to compare', choices=Truc.mono(),selected=NULL,
                 multiple=T,width='250%')
})
output$choice.band.mono.aft.tot <- renderUI({
  selectizeInput('name.band.mono.aft.tot', 'Choice of the band to compare', choices=Truc.mono(),selected=NULL,
                     multiple=T,width='250%')
})

output$mono.knitr.download = downloadHandler(
  filename = function(x){paste0(input$mono.knitr.download.text,'.pdf')},
  content = function(file) {
    out = knit2pdf('inputMonoQuanti.Rnw', clean = TRUE)
    file.rename(out, file) # move pdf to file for downloading
  },
  contentType = 'application/pdf'
)

output$downloadReport <- downloadHandler(
  filename = function() {
    paste(input$mono.knitr.download.text, sep = '.', switch(
      input$reportformat, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },

  content = function(file) {
    src <- normalizePath('report.Rmd')

    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    # owd <- setwd(tempdir())
    # on.exit(setwd(owd))
    # file.copy(src, 'report.Rmd')

    library(rmarkdown)
    out <- render('report.Rmd', switch(
      input$reportformat,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)

output$sessionInfo <- renderPrint({
  sessionInfo()
})



})

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
f.area<-function(data,start,stop){
  return(abs(sum(data[start:stop])))
}
f.height<-function(data,start,stop){
  return(abs(max(data[start:stop])))
}
f.read.image<-function(source,native,format,height=0){
#   gamma=PicturePreprocess[1];medianfilter=PicturePreprocess[2];lowpass=PicturePreprocess[3];highpass=PicturePreprocess[4]
  if(format == "tiff"){data<-readTIFF(source,native=native)}
  if(format == "jpeg"){data<-readJPEG(source=source,native=native)}
  if(format == "png"){data<-readPNG(source=source,native=native)}
#   if(format == 'bmp'){data<-read.bmp(source)}
#   if(format == 'EBIimage'){data<-readImage(source,type=format)@.Data;data<-aperm(data,c(2,1,3))}
#   data<-readImage(source,type=format)
#   if(gamma!=1){data <- data ^ gamma}
#   if(medianfilter!=1){data <- medianFilter(data,medianfilter)}
#   if(lowpass==T){
#     fLow <- makeBrush(21, shape= 'disc', step=FALSE)^2
#     fLow <- fLow/sum(fLow)
#     data <- filter2(data, fLow)
#     }
#   if(highpass==T){
#     fHigh <- matrix(1, nc = 3, nr = 3)
#     fHigh[2, 2] <- -8
#     data <- filter2(data, fHigh)
#     }
#   data <- normalize(data)
#   data <- data@.Data
#   data<-aperm(data,c(2,1,3))
  if(height != 0){
    data.1 <- redim(data[,,1],height,dim(data)[2])
    data.2 <- redim(data[,,2],height,dim(data)[2])
    data.3 <- redim(data[,,3],height,dim(data)[2])
    data <- abind(data.1,data.2,data.3,along=3)
  }
  return(data)
}

f.plot.and.calib<-function(dataX,data,column.mono=input$column.mono,
                           signal.type=input$monovariate.signal.type,model.type=input$monovariate.model.type,
                           hauteur=input$hauteur.mono,dist.bas=input$dist.bas.mono,Zf=input$Zf.mono,z.min=input$z.min,z.max=input$z.max,
                           type.return="table"){
  pix.min<-z.min
  pix.max<-z.max
  area<-apply(data,1,f.area,start=pix.min,stop=pix.max)
  height<-apply(data,1,f.height,start=pix.min,stop=pix.max)
  data.1<-cbind(dataX,area,height)
  colnames(data.1)[colnames(data.1)==column.mono] <- "masse"
  if(signal.type=="area"){
    if(model.type=="linear"){
      fit<-lm(masse~area,data=data.1,Use==T)
    }else{fit<-lm(masse~area+I(area^2)+0,data=data.1,Use==T)}
  }
  if(signal.type=="height"){
    if(model.type=="linear"){
      fit<-lm(masse~height,data=data.1,Use==T)
    }else{fit<-lm(masse~height+I(height^2)+0,data=data.1,Use==T)}
  }
  masse.pred<-predict(fit,newdata=data.1)
  data<-cbind(data.1,masse.pred)
  data$teneur<-data$masse.pred/data[,9]*100
  if(type.return=="table"){return(data)}
  if(type.return=="model"){return(fit)}
  data.plot.cal<-data[data$Use==T,]
  data.plot.unknow<-data[data$Use!=T,]
  if(signal.type=="area"){plot<-ggplot()+
                            geom_text(data=data.plot.cal,aes(y=masse,x=area,label=id),colour="red")+
                            geom_point(data=data.plot.cal,aes(y=masse,x=area),shape=21,size=9,colour="red")
                          if(model.type=="linear"){
                            plot<-plot+stat_smooth(data=data.plot.cal,aes(y=masse,x=area),method=lm,se=F,formula= y~x,colour="red")
                          }else{
                            plot<-plot+stat_smooth(data=data.plot.cal,aes(y=masse,x=area),method=lm,se=F,formula= y~x +I(x^2)+0,colour="red")
                          }
                          plot<-plot+geom_text(data=data.plot.unknow,aes(y=masse.pred,x=area,label=id),colour="blue")+
                            geom_point(data=data.plot.unknow,aes(y=masse.pred,x=area),shape=21,size=9,colour="blue")+
                            coord_flip()                            
  }
  if(signal.type=="height"){plot<-ggplot()+
                              geom_text(data=data.plot.cal,aes(y=masse,x=height,label=id),colour="red")+
                              geom_point(data=data.plot.cal,aes(y=masse,x=height),shape=21,size=9,colour="red")
                            if(model.type=="linear"){
                              plot<-plot+stat_smooth(data=data.plot.cal,aes(y=masse,x=height),method=lm,se=F,formula= y~x,colour="red")
                            }else{
                              plot<-plot+stat_smooth(data=data.plot.cal,aes(y=masse,x=height),method=lm,se=F,formula= y~x +I(x^2)+0,colour="red")
                            }
                            plot<-plot+geom_text(data=data.plot.unknow,aes(y=masse.pred,x=height,label=id),colour="blue")+
                              geom_point(data=data.plot.unknow,aes(y=masse.pred,x=height),shape=21,size=9,colour="blue")+
                              coord_flip()
  }
  plot
}

redim = function(im, w.out, h.out) {
  w.in = nrow(im)
  h.in = ncol(im)
  im.out = matrix(rep(0,w.out*h.out), nrow =w.out, ncol=h.out )
  w_ratio = w.in/w.out
  h_ratio = h.in/h.out
  im.out <- im[ floor(w_ratio* 1:w.out), floor(h_ratio* 1:h.out)] 
  return(im.out)
}
f.rebuilt<-function(a,b,c){
  data<-abind(cbind(a,a),
              cbind(b,b),
              cbind(c,c),
              along=3)
  return(data)
}
f.rebuilt.vector<-function(a){
  data<-as.vector(a)
  l<-length(data)
  data<-array(data,dim=c(l/3,1,3))
  return(data)
}
f.image.tot<-function(data,vec){
  plot(c(0,nrow(data)),c(0,10), type='n',ylab="",xlab="",xaxt = "n",yaxt = "n")
  ### photo rebuilt ####
  for(i in c(0:(nrow(data)-1))){
    data2<-f.rebuilt.vector(data[i+1,])
    rasterImage(data2,i,0,i+1,10)
    text(x=i+0.5,y=9+sin(i*pi/2),labels=vec[i+1],col="red",cex=2)
    par(new=T)
  }
  par(new=F)
}
f.eat.image<-function(image,largeur,dist.gauche,band,ecart,tolerance){
  a<-dim(image)
  nbr.band<-round((largeur-2*dist.gauche)/(band+ecart))
  data<-c()
  for(i in c(0:(nbr.band-1))){
    truc<-apply(image[,(a[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))):(a[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),1],1,mean)
    data<-rbind(data,truc)
  }
  data2.1<-as.data.frame(data)
  data<-c()
  for(i in c(0:(nbr.band-1))){
    truc<-apply(image[,(a[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))):(a[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),2],1,mean)
    data<-rbind(data,truc)
  }
  data2.2<-as.data.frame(data)
  data<-c()
  for(i in c(0:(nbr.band-1))){
    truc<-apply(image[,(a[2]/largeur*((dist.gauche+tolerance)+i*(band+ecart))):(a[2]/largeur*((dist.gauche+band-tolerance)+i*(band+ecart))),3],1,mean)
    data<-rbind(data,truc)
  }
  data2.3<-as.data.frame(data)
  data<-abind(data2.1,data2.2,data2.3,along=3)
  data <- abind(data,apply(data,1:2,mean),along=3)
  return(data)
}
f.plot.array<-function(data,id,label,hauteur,Zf,dist.bas,reconstruct=T,xlim=c(-dist.bas/(Zf-dist.bas),(hauteur-dist.bas)/(Zf-dist.bas)),inverse=F,ylim.raster=1.3){
  if(reconstruct==T){
    if(is.null(label)){
      plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
           ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab="",
           type="n")
      par(new=T)
    }else{
      plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
           ylim=c(0,ylim.raster),xlim=xlim,main=label[id],xlab='',ylab="",
           type="l",col="red")
      par(new=T)
    }
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab="",
         type="l",col="red")
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,2]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',
         type="l",col="green")
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,3]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',
         type="l",col="blue")
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,4]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',
         type="l",col="black")
    if(inverse==F){
      data.plot<-round(array(data[id,,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
      rasterImage(data.plot,(hauteur-dist.bas)/(Zf-dist.bas) , 1.1, -dist.bas/(Zf-dist.bas), 1.3)
    }else{
      data.plot<-round(array(data[id,dim(data)[2]:1,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
      rasterImage(data.plot, (hauteur-dist.bas)/(Zf-dist.bas) , 1.1, -dist.bas/(Zf-dist.bas), 1.3)
    }
  }else{
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
         ylim=c(min(data),max(data)),xlim=xlim,main=label[id],xlab='',ylab='',
         type="l",col="red")
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,2]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',
         type="l",col="green")
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,3]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',
         type="l",col="blue")
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,4]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',
         type="l",col="black")
  }
  mtext(side = 1, expression("R"['F']), line = 2)
  mtext(side = 2, "Intensity", line = 2)
}

do.VPdtw <- function(data,ref,dataX,maxshift=50,Reference.type=c("random","median","mean","trimmed")){
  ref<-as.character(ref)
  if(ref != "0"){
  data.a<-VPdtw(reference = data[ref,,1],data[,,1],maxshift=maxshift)$warpedQuery
  data.b<-VPdtw(reference = data[ref,,2],data[,,2],maxshift=maxshift)$warpedQuery
  data.c<-VPdtw(reference = data[ref,,3],data[,,3],maxshift=maxshift)$warpedQuery
  data.d<-VPdtw(reference = data[ref,,4],data[,,4],maxshift=maxshift)$warpedQuery
  }else{
    data.a<-VPdtw(reference = NULL,data[,,1],maxshift=maxshift,Reference.type=Reference.type)$warpedQuery
    data.b<-VPdtw(reference = NULL,data[,,2],maxshift=maxshift,Reference.type=Reference.type)$warpedQuery
    data.c<-VPdtw(reference = NULL,data[,,3],maxshift=maxshift,Reference.type=Reference.type)$warpedQuery
    data.d<-VPdtw(reference = NULL,data[,,4],maxshift=maxshift,Reference.type=Reference.type)$warpedQuery
    
  }
  data<-abind(data.a,data.b,data.c,data.d,along=3)
  print(dim(data))
  rownames(data)<-dataX$id
  data[is.na(data)] <- 0
  return(data)
}

f.rebind <- function(data,channel,hauteur=10,dist.bas=1,Zf=7){
  Rf <- round(seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),3)
  data <- t(apply(data[,,channel],c(1),cbind))
  channel <- gsub(1,'red',gsub(2,'green',gsub(3,'blue',gsub(4,'grey',channel))))
  new.rf <- c()
  for(i in channel){
    new.rf <- c(new.rf,paste(i,Rf,sep='_'))
  }
  colnames(data) <- new.rf
  return(data)
}
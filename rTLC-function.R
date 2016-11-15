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
f.read.image<-function(source,native,format,height=0){
  if(format == "tiff"){data<-readTIFF(source,native=native)}
  if(format == "jpeg"){data<-readJPEG(source=source,native=native)}
  if(format == "png"){data<-readPNG(source=source,native=native)}
  if(height != 0){
    data.1 <- redim(data[,,1],height,dim(data)[2])
    data.2 <- redim(data[,,2],height,dim(data)[2])
    data.3 <- redim(data[,,3],height,dim(data)[2])
    data <- abind(data.1,data.2,data.3,along=3)
  }
  return(data)
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
f.plot.array<-function(data,id,label,hauteur,Zf,dist.bas,reconstruct=T,xlim=c(-dist.bas/(Zf-dist.bas),(hauteur-dist.bas)/(Zf-dist.bas)),inverse=F,ylim.raster=1.3,...){
  if(reconstruct==T){
    if(is.null(label)){
      plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
           ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab="",xaxt="n",
           type="n")
      par(new=T)
    }else{
      plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
           ylim=c(0,ylim.raster),xlim=xlim,main=label[id],xlab='',ylab="",xaxt="n",
           type="l",col="red",...)
      par(new=T)
    }
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab="",xaxt="n",
         type="l",col="red",...)
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,2]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="green",...)
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,3]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="blue",...)
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,4]),
         ylim=c(0,ylim.raster),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="black",...)
    if(inverse==F){
      data.plot<-round(array(data[id,,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
      rasterImage(data.plot,(hauteur-dist.bas)/(Zf-dist.bas) , 1.1, -dist.bas/(Zf-dist.bas), 1.3)
    }else{
      data.plot<-round(array(data[id,dim(data)[2]:1,c(1,2,3)],dim=c(1,dim(data)[2],3))*256)/256
      rasterImage(data.plot, (hauteur-dist.bas)/(Zf-dist.bas) , 1.1, -dist.bas/(Zf-dist.bas), 1.3)
    }
  }else{
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,1]),
         ylim=c(min(data),max(data)),xlim=xlim,main=label[id],xlab='',ylab='',xaxt="n",
         type="l",col="red",...)
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,2]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="green",...)
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,3]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="blue",...)
    par(new=T)
    plot(x=seq((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas),length.out=dim(data)[2]),y=as.vector(data[id,,4]),
         ylim=c(min(data),max(data)),xlim=xlim,xlab='',ylab='',xaxt="n",yaxt="n",
         type="l",col="black",...)
  }
  mtext(side = 1, expression(italic(R)['F']), line = 2.5,cex.axis=0.9,...)
  mtext(side = 2, "Pixel intensity (AU)", line = 2.5,cex.axis=0.9,...)
  axis(side=1,at=seq(0,1,length.out = 11),labels=seq(0,1,length.out = 11))
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



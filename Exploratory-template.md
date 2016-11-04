Exploratory Templates
===========

Those lines of code could be copy and paste in the editor tab to produce differents plots and print output.
Use it to produce publications ready figures, they still need to be rescaled with an other software unfortunatly.

## Line plot comparison
Works with the propolis dataset.

Need a pixel dimension of 1063*1063. Single column figure for Elsevier.

```r
hauteur<-input$hauteur.mono
dist.bas<-input$dist.bas.mono
Zf <- input$Zf.mono

layout(rbind(c(1,2),c(3,3),c(4,4)))
par(cex.axis=1.5,cex.main=2.5,cex.lab=2,mgp = c(3, 1, 0),mar=c(5,5,4,1))
data <- data.mono.2()[c(37,42),,]
n.band<-37
label = c("Blue type of propolis","Orange type of propolis")
f.plot.array(data,1,label,input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,inverse=F,cex=1.5)
mtext("A", side = 3, line = 1, outer = F, at = c(-0.175),
      adj = NA, padj = 0.5, cex = 3, col =NA, font = NA)
n.band<-42
f.plot.array(data,2,label,input$hauteur.mono,input$Zf.mono,input$dist.bas.mono,inverse=F,cex=1.5)

par(mar=c(1,6,1,0), xaxs="i", yaxs="i",mgp=c(2.5,1,0))
data<-data.mono.2()
band<-c(2,7,8,16,18,19,35,36,39,51,60,61,62,63,103)
plot(c(0,length(band)),c((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas)), type='n',ylab=expression(italic(R)['F']),xlab="",xaxt = "n", bty='n',cex.lab=2.5)
for(i in seq(band)){
  data2<-f.rebuilt(data[band[i],,1],data[band[i],,2],data[band[i],,3])
  rasterImage(data2,i-1,-dist.bas/(Zf-dist.bas),i,(hauteur-dist.bas)/(Zf-dist.bas))
  text(x=i-0.5,y=0.9,labels=band[i],col="red",cex=2)
  par(new=T)
}
mtext("B", side = 3, line = 1, outer = FALSE, at = c(-0.6),
      adj = NA, padj = 1.2, cex = 3, col =NA, font = NA)
par(new=F)
data<-data.mono.2()
band<-c(5,6,11,17,41,42,43,44,54,58,59,64,66,72,75)
plot(c(0,length(band)),c((hauteur-dist.bas)/(Zf-dist.bas),-dist.bas/(Zf-dist.bas)), type='n',ylab=expression(italic(R)['F']),xlab="",xaxt = "n", bty='n',cex.lab=2.5)
for(i in seq(band)){
  data2<-f.rebuilt(data[band[i],,1],data[band[i],,2],data[band[i],,3])
  rasterImage(data2,i-1,-dist.bas/(Zf-dist.bas),i,(hauteur-dist.bas)/(Zf-dist.bas))
  text(x=i-0.5,y=0.9,labels=band[i],col="red",cex=2)
  par(new=T)
}

mtext("C", side = 3, line = 1, outer = FALSE, at = c(-0.6),
      adj = NA, padj = 1.2, cex = 3, col =NA, font = NA)

```

## PCA scatterplot
Works with the propolis dataset

Need a pixel dimension of 2244 * 1000. Double column figure for Elsevier.
```r
library(grid)
library(gridExtra) 

Rf <- round(seq(RF.max(),RF.min(),length.out=dim(data.mono.3())[2]),3) ## extract the Rf to be able to subset the dataset
data<-data.mono.3()[,Rf >= 0 & Rf <= 1,3] ## do the variable selection, only Rf between 0 and 1, and only the third element of the dimension 3 of the preprocessed array, i.e. blue channel
model = PCA(as.matrix(data))
data<-as.data.frame(scores(model,npc=2))
colnames(data)<-c("PC1","PC2")
str(data)
data$Color<-dataX.mono.pre()[,"Class.2"]
xlabel<-paste0("PC1",' (',round(model$var[1]/model$totalvar*100,2),"%)")
ylabel<-paste0("PC2",' (',round(model$var[2]/model$totalvar*100,2),"%)")
plot_blue<-ggplot()+geom_point(data=data,aes(x=PC1,y=PC2,col=Color),size=3)+
  labs(x=xlabel, y=ylabel)+ stat_ellipse(data=data,aes(x=PC1,y=PC2,col=Color),level=0.95)+
  theme(legend.justification=c(1,1), legend.position=c(1,1),legend.text = element_text(size = 22),legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  labs(col="")+
  ggtitle("Blue channel")+
  theme(plot.title = element_text(size=34),axis.title = element_text(size=26),axis.text  =element_text(size=22))+
  theme(plot.margin = unit(c(0.5,0.5,0,0.5), "in")) 


data<-data.mono.3()[,Rf >= 0 & Rf <= 1,4] ## do the variable selection, only Rf between 0 and 1, and only the third element of the dimension 3 of the preprocessed array, i.e. blue channel
model = PCA(as.matrix(data))
data<-as.data.frame(scores(model,npc=2))
colnames(data)<-c("PC1","PC2")
str(data)
data$Color<-dataX.mono.pre()[,"Class.2"]
xlabel<-paste0("PC1",' (',round(model$var[1]/model$totalvar*100,2),"%)")
ylabel<-paste0("PC2",' (',round(model$var[2]/model$totalvar*100,2),"%)")
plot_gray<-ggplot()+geom_point(data=data,aes(x=PC1,y=PC2,col=Color),size=3)+
  labs(x=xlabel, y=ylabel)+ stat_ellipse(data=data,aes(x=PC1,y=PC2,col=Color),level=0.95)+
  theme(legend.justification=c(1,1), legend.position=c(1,1),legend.text = element_text(size = 22),legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
  labs(col="")+
  ggtitle("Grayscale")+
  theme(plot.title = element_text(size=34),axis.title = element_text(size=26),axis.text  =element_text(size=22))+
  theme(plot.margin = unit(c(0.5,0.5,0,0.5), "in")) 

grid.arrange(plot_blue, plot_gray, ncol = 2)
```

## PCA loadingplot
Works with the propolis dataset

Need a pixel dimension of 2244 * 1000. Double column figure for Elsevier.
```r
par(mfcol=c(2,2),cex=1.5,lwd=2,cex.lab=1.5,cex.main=1.5,mgp=c(2,0.5,0))

Rf <- round(seq(RF.max(),RF.min(),length.out=dim(data.mono.3())[2]),3) ## extract the Rf to be able to subset the dataset
data<-data.mono.3()[,Rf >= 0 & Rf <= 1,3] ## do the variable selection, only Rf between 0 and 1, and only the third element of the dimension 3 of the preprocessed array, i.e. blue channel
RF = seq(1,0,length.out=length(data))
model = PCA(as.matrix(data))
data <- loadings.PCA(model)[,1]
RF = seq(1,0,length.out=length(data))
# par(xaxp  = c(min(RF), max(RF), 0.1))
plot(x=RF, xaxt = "n",
   y=as.matrix(data),type="l",main=paste0("Loading plot PC",1),xlab=expression(italic(R)['F']),ylab="Intensity (AU)")
axis(side = 1, at = round(seq(1,0,length.out=(1-0)*10),2))
abline(v = RF[pick.peaks(data, 15)], col = "blue")
abline(v = RF[pick.peaks(-data, 15)], col = "red")
data <- loadings.PCA(model)[,2]
# par(xaxp  = c(min(RF), max(RF), 0.1))
plot(x=RF, xaxt = "n",
   y=as.matrix(data),type="l",main=paste0("Loading plot PC",2),xlab=expression(italic(R)['F']),ylab="Intensity (AU)")
axis(side = 1, at = round(seq(1,0,length.out=(1-0)*10),2))
abline(v = RF[pick.peaks(data, 15)], col = "blue")
abline(v = RF[pick.peaks(-data, 15)], col = "red")  


data<-data.mono.3()[,Rf >= 0 & Rf <= 1,4] ## do the variable selection, only Rf between 0 and 1, and only the third element of the dimension 3 of the preprocessed array, i.e. blue channel
model = PCA(as.matrix(data))
data <- loadings.PCA(model)[,1]
RF = seq(1,0,length.out=length(data))
# par(xaxp  = c(min(RF), max(RF), 0.1))
plot(x=RF, xaxt = "n",
   y=as.matrix(data),type="l",main=paste0("Loading plot PC",1),xlab=expression(italic(R)['F']),ylab="Intensity (AU)")
axis(side = 1, at = round(seq(1,0,length.out=(1-0)*10),2))
abline(v = RF[pick.peaks(data, 15)], col = "blue")
abline(v = RF[pick.peaks(-data, 15)], col = "red")
data <- loadings.PCA(model)[,2]
# par(xaxp  = c(min(RF), max(RF), 0.1))
plot(x=RF, xaxt = "n",
   y=as.matrix(data),type="l",main=paste0("Loading plot PC",2),xlab=expression(italic(R)['F']),ylab="Intensity (AU)")
axis(side = 1, at = round(seq(1,0,length.out=(1-0)*10),2))
abline(v = RF[pick.peaks(data, 15)], col = "blue")
abline(v = RF[pick.peaks(-data, 15)], col = "red")  
```


## Hierarchical plot

Works with the propolis dataset

Need a pixel dimension of 2244 * 1063. Double column figure for Elsevier.
```r
par(mfrow=c(1,2),cex.lab=2,cex.axis=2,cex.main=3,mar=c(1,5,5,0.2)) ## define graphics options

Rf <- round(seq(RF.max(),RF.min(),length.out=dim(data.mono.3())[2]),3) ## extract the Rf to be able to subset the dataset
data<-data.mono.3()[,Rf >= 0 & Rf <= 1,3] ## do the variable selection, only Rf between 0 and 1, and only the third element of the dimension 3 of the preprocessed array, i.e. blue channel
rownames(data)<-dataX.mono.pre()[,"Class.2"] ## define the row names of the dataset
d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") ## fit the cluster
plot(fit,xlab="",sub="",main="Blue channel",lwd=1.5,ylab="Distance") # display dendogram
rect.hclust(fit, k=3, border="red") ## define the cluster separations
mtext("A", side = 3, line = 1, outer = F, at = c(-8),
      adj = NA, padj = 0, cex = 5, col =NA, font = NA)

Rf <- round(seq(RF.max(),RF.min(),length.out=dim(data.mono.3())[2]),3)
data<-data.mono.3()[,Rf >= 0 & Rf <= 1,4]
rownames(data)<-dataX.mono.pre()[,"Class.2"]
d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit,xlab="",sub="",main="Grayscale",lwd=1.5,ylab="Distance") # display dendogram
rect.hclust(fit, k=2, border="red")
mtext("B", side = 3, line = 1, outer = F, at = c(-8),
      adj = NA, padj = 0, cex = 5, col =NA, font = NA)

```
Prediction Templates
===========

Those lines of code could be copy and paste in the editor tab to produce differents plots and print output

## Variable importance for random forest

```r
varImpPlot(model$finalModel,main='Variable importance for random forest')
```

## Variable importances for random forest and plot of the first 3 variables

```r
# First: extract the variable and reorder them
truc <- importance(model$finalModel)[order(importance(model$finalModel),decreasing=T),] 
names(truc) <- gsub('Ind.','',names(truc))

# Function that take the rank indice of importance to plot
f.plot <- function(vec = c(1,2)){ 
  plot(x=Ind[,names(truc)[vec[1]]],y=Ind[,names(truc)[vec[2]]],col=as.factor(Dep),
       xlab=names(truc)[vec[1]],ylab=names(truc)[vec[2]]
       )
}
par(mfrow=c(2,2)) # split the plot in 4 part
varImpPlot(model$finalModel)
# Change the number in the funciton to plot a different set of variable
f.plot(c(1,2))
f.plot(c(1,3))
f.plot(c(2,3))
```

## Score plot for pls

```r
data <- as.data.frame(scores(model$finalModel)[,1:2])
colnames(data) <- c('Comp1','Comp2')
Dep <- Dep[Train.partition()]
print(qplot(Comp1,Comp2,data=data,col=Dep))
```



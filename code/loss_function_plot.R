### code to make the loss functions all on one figure for the paper. 

x<-seq(0,1,by=0.01)
l1<- -log(1-2*abs(.5-x))
l2<- - log(1-abs(.5-x))
l3<- - log(1-abs(1-x))
l4<- - log(1-abs(0-x))
l5<- -log(1-x)


pdf(file='loss_function_plot.pdf')
plot(x,l1,type='l', xlab='p', ylab='score')
lines(x,l2,col='green')
lines(x,l3,col='red')
lines(x,l4,col='blue')
lines(x,l5,col='purple') 
legend(.5,3.5,c('','','','',''), lty=c(1,1), lwd=c(2.5,2.5),col=c("black","green",'red','blue','purple'))
dev.off()
### code to make the loss functions all on one figure for the paper. 
getwd()
setwd('/Users/rlucas7/NCAA/figures')
x<-seq(0,1,by=0.01)
#l1<- -log(1-2*abs(.5-x))
#l2<- - log(1-abs(.5-x))
l3<- - log(1-abs(1-x))
l4<- - log(1-abs(0-x))
l5<- -log(1-2*abs(0.5-x))
# l5<- -log(1-x) # this equals, l4, the case of eqn (8) with y_i==0
# l6<- -abs(.5-x)*log(abs(.5-x)) # don't use this one, 
#plot the function versus p to see why

pdf(file='loss_function_plot_col.pdf')
plot(x,l3,type='l', xlab='p', ylab='score',ylim=c(0,5))
#abline(h=0)
#lines(x,l2,col='green')
lines(x,l4,col='red')
lines(x,l5,col='blue')
#lines(x,l5,col='purple') 
legend(.5,3.5,c('Part 1','Part 2','Addition Proposed'), lty=c(1,1), lwd=c(2.5,2.5),col=c("black",'red','blue'))
dev.off()


pdf(file='loss_function_plot_bw.pdf')
plot(x,l3,type='l', xlab='p', ylab='score',ylim=c(0,5))
#abline(h=0)
#lines(x,l2,col='green')
#lines(x,l4,col='red')
lines(x,l4,lty=2)
lines(x,l5,lty=3) 
legend(.5,3.5,c('Part 1','Part 2',expression(tau == 1/2)), lty=c(1,2,3), lwd=c(2.5,2.5))
dev.off()


# # pdf(file='loss_function_plot_bw.pdf')
# plot(x,l1,type='l', xlab='p', ylab='score')
# lines(x,l2,lty=2)
# lines(x,l3,lty=3)
# lines(x,l4,lty=4)
# #lines(x,l6,lty=5) 
# #legend(.5,3.5,c('(6)','(7)','(8a)','(8b)','(alt)'), lty=c(1,2,3,4,5), lwd=c(2.5,2.5))
# legend(.5,3.5,c('(6)','(7)','(8a)','(8b)'), lty=c(1,2,3,4), lwd=c(2.5,2.5))
# dev.off()
set.sed(12345)
#generate beta
beta<-runif(n = 5,min = 1,max = 3)
beta
upper.ci<-beta+0.25
upper.ci
lower.ci<-beta-0.25
samp.n<-rep(x = 10,5)
samp.n
data<-data.frame(samp.n,beta,lower.ci,upper.ci)
data
data$group<-rownames(data)<-paste0("drug",1:5)
data$group<-factor(data$group)
str(data)
format(data,digits=3) # 2 decimals 
library(ggplot2)
plot.b<-ggplot(data,aes(x=group,y=beta))
plot.b1<-plot.b+geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci))
plot.b1
#prettier
plot.c<-ggplot(data,aes(x=group,y=beta,colour=group)) #colour based on group
plot.c1<-plot.c+geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci),width=0.3) #width = width of end of bars              
plot.c1

#make points bigger and nicer
point.c<-geom_point(size=3,shape=21,fill='white')
#different shape
point.c.sp<-geom_point(aes(shape=group),size=5)

#c1 and point
plot.c2<-plot.c1+point.c
plot.c2

#redo
plot.c1+geom_point()
plot.c1+geom_point(aes(shape=group))

#or
plot.c1+point.c.sp

#reposition (need reworking) - not working , unless make the x axis in numerical 
pd<-position_dodge(0.5)
ggplot(data,aes(x=group,y=beta,colour=group)) +  
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci),width=0.3,position=pd) +
  geom_point(aes(shape=group),size=5,position=pd)

#so 
data$xaxis<-as.numeric(seq(from=1,to=5))
pd<-position_dodge(0.5)
pd1<-ggplot(data,aes(x=xaxis,y=beta,colour=group)) 
pd2<-  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci),width=0.3,position=pd) 
pd3<-  geom_point(size=3,shape=21,position=pd)
pd4<-pd1+pd2
pd4
pd5<-pd4+pd3
pd5
  #excellent ref = http://docs.ggplot2.org/0.9.3.1/geom_point.html, 
#http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  
#nice
var1<-ggplot(data,aes(x=group,y=beta,colour=group))
var.er.b<-var1+geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci),width=0.3,colour='black')
var.pnt<-var.er.b+geom_point(size=3,shape=21,fill='white')
var.lab.tit<-var.pnt+xlab('Drugs')+ylab('beta coefficients')+ggtitle('Drugs and regression coefficients')
var.lab.tit.sc.th<-var.lab.tit + scale_y_continuous(limits=c(0.75,max(3.0)),breaks=0:15*0.25)+theme_bw()
var.legend<-var.lab.tit.sc.th+theme(legend.justification=c(1,1), legend.position=c(.85,1))
var.legend
#increase text
var.legend.txt<-var.legend+ theme(legend.title=element_text(size=24),legend.text=element_text(size=24))
var.legend.txt

#####
var1<-ggplot(data,aes(x=group,y=beta))
var.er.b<-var1+geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci),width=0.3,colour='black')
var.pnt<-var.er.b+geom_point(size=10,shape=21,fill='white')
var.lab.tit<-var.pnt+xlab('Drugs')+ylab('beta coefficients')+ggtitle('Drugs and regression coefficients')
var.lab.tit.sc.th<-var.lab.tit + scale_y_continuous(limits=c(0.75,max(3.0)),breaks=0:15*0.25)+theme_bw()
var.legend<-var.lab.tit.sc.th+theme(legend.justification=c(1,1), legend.position=c(.85,1))
var.legend
#increase text
var.legend.txt<-var.legend+ theme(legend.title=element_text(size=24),legend.text=element_text(size=24))
var.legend.txt

###
var.pnt<-var.er.b+geom_point(aes(shape=group),fill='white')
var.lab.tit<-var.pnt+xlab('Drugs')+ylab('beta coefficients')+ggtitle('Drugs and regression coefficients')
var.lab.tit.sc.th<-var.lab.tit + scale_y_continuous(limits=c(0.75,max(3.0)),breaks=0:15*0.25)+theme_bw()
var.legend<-var.lab.tit.sc.th+theme(legend.justification=c(1,1), legend.position=c(.85,1))
var.legend
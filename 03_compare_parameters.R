#-------------------------------------------------------
rm(list=ls())
model_name=c('null')

load(paste('./data/',model_name,'_recovered_parameters3.rdata',sep=""))
load(paste('./data/',model_name,'_true_parameters.Rdata',sep=""))


library(ggplot2)
library(ggpubr)

#population level parameters
p1= ggplot(data.frame(x=pars$phi),aes(x=x))+geom_density(alpha = .5,fill="yellow")+ 
    geom_vline(xintercept = 0.3#mean(true.parameters[,2])
               , linetype="dotted",color = "blue", size=1.5)+
    xlim(0,0.6)+
    xlab(expression(Phi))+ theme_classic()

p2= ggplot(data.frame(x=exp(pars$population_location)),aes(x=x))+geom_density(alpha = .5,fill="pink")+ 
    geom_vline(xintercept = 3#(mean(true.parameters[,3]))
               , linetype="dotted",color = "blue", size=1.5)+
    xlim(2,5)+
    xlab(expression(beta['mean']))+ theme_classic()


#compare individual level parameters
p3=ggplot(data.frame(x =true.parameters[,'alpha'], y =apply(pars$alpha, 2, mean)),aes(x=x,y=y))+geom_point()+
    labs(title='',
         subtitle = paste('r=',round(cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean)),2)),
         x=expression(alpha['true']),
         y=expression(alpha['recovered']))+ 
    xlim(0,1)+ylim(0,1)+
    theme_classic()
    
p4=ggplot(data.frame(x =true.parameters[,'beta'], y =apply(pars$beta, 2, mean)),aes(x=x,y=y))+geom_point()+
    labs(title='',
         subtitle = paste('r=',round(cor(true.parameters[,'alpha'], apply(pars$alpha, 2, mean)),2)),
         x=expression(beta['true']),
         y=expression(beta['recovered']))+ 
    xlim(0,10)+ylim(0,10)+
    theme_classic()

ggarrange(p1,p3,p2,p4,nrow=2,ncol=3)

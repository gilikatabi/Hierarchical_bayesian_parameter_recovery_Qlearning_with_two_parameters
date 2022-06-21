#This code plot recovered parameters against the true parameters


rm(list=ls())
model_name=c('different_alphas')
load(paste('./data/',model_name,'_recovered_parameters.rdata',sep=""))
load(paste('./data/','true_parameters.Rdata',sep=""))


library(ggplot2)
library(ggpubr)
library(bayestestR)
#-------------------------------------------------------------------------------------------------------------
# #population level parameters
source('./functions/my_posteriorplot.R')


p1 = my_posteriorplot(x       = pars$population_locations[,1],
                     myxlim  = c(0,6),
                     my_vline= 4, 
                     myxlab  = expression(beta['location']),
                     mycolor = "pink")

p2 = my_posteriorplot(x       = plogis(pars$population_locations[,2]),
                     myxlim  = c(0,1.5),
                     my_vline= 0.5, 
                     myxlab  = expression(alpha_1['location']),
                     mycolor = "pink")


p3 = my_posteriorplot(x       = plogis(pars$population_locations[,3]),
                     myxlim  = c(0,1.5),
                     my_vline= 0.3, 
                     myxlab  = expression(alpha_2['location']),
                     mycolor = "pink")

p4 = my_posteriorplot(x       = plogis(pars$population_locations[,4]),
                     myxlim  = c(0,1.5),
                     my_vline= 0.1, 
                     myxlab  = expression(alpha_3['location']),
                     mycolor = "pink")

p5 = my_posteriorplot(x       = plogis(pars$population_locations[,5]),
                     myxlim  = c(0,1.5),
                     my_vline= 0.5, 
                     myxlab  = expression(alpha_4['location']),
                     mycolor = "pink")

p6 = my_posteriorplot(x       = plogis(pars$population_locations[,6]),
                     myxlim  = c(0,1.5),
                     my_vline= 0.3, 
                     myxlab  = expression(alpha_5['location']),
                     mycolor = "pink")

p7= my_posteriorplot(x       = plogis(pars$population_locations[,7]),
                     myxlim  = c(0,1.5),
                     my_vline= 0.7, 
                     myxlab  = expression(alpha_6['location']),
                     mycolor = "pink")
#####################

p8= my_posteriorplot(x       = pars$population_scales[,1],
                     myxlim  = c(0,2),
                     my_vline= 1.5, 
                     myxlab  = expression(beta['scale']),
                     mycolor = "yellow")

p9= my_posteriorplot(x       = pars$population_scales[,2],
                     myxlim  = c(0,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha_1['scale']),
                     mycolor = "yellow")

p10= my_posteriorplot(x       = pars$population_scales[,3],
                     myxlim  = c(0,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha_2['scale']),
                     mycolor = "yellow")

p11= my_posteriorplot(x       = pars$population_scales[,4],
                     myxlim  = c(0,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha_3['scale']),
                     mycolor = "yellow")

p12= my_posteriorplot(x       = pars$population_scales[,5],
                     myxlim  = c(0,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha_4['scale']),
                     mycolor = "yellow")

p13= my_posteriorplot(x       = pars$population_scales[,6],
                     myxlim  = c(0,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha_5['scale']),
                     mycolor = "yellow")

p14= my_posteriorplot(x       = pars$population_scales[,7],
                     myxlim  = c(0,1.5),
                     my_vline= 1, 
                     myxlab  = expression(alpha_6['scale']),
                     mycolor = "yellow")

annotate_figure(ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,nrow=2,ncol=7), 
                top = text_grob("Population Level Parameters (fixed effects)", color = "black", face = "bold", size = 10))

#-------------------------------------------------------------------------------------------------------------
# individual level parameters

p1=ggplot(data.frame(x =true.parameters[,'beta'], y =apply(pars$beta, 2, mean)),aes(x=x,y=y))+geom_point()+
  labs(title='',
       subtitle = paste('r=',round(cor(true.parameters[,'beta'], apply(pars$beta, 2, mean)),2)),
       x=expression(beta['true']),
       y=expression(beta['recovered']))+ 
  xlim(0,10)+ylim(0,10)+
  theme_classic()

p2=ggplot(data.frame(x =true.parameters[,'alpha_1'], y =apply(pars$alpha_1, 2, mean)),aes(x=x,y=y))+geom_point()+
    labs(title='',
         subtitle = paste('r=',round(cor(true.parameters[,'alpha_1'], apply(pars$alpha_1, 2, mean)),2)),
         x=expression(alpha_1['true']),
         y=expression(alpha_1['recovered']))+ 
    xlim(0,1)+ylim(0,1)+
    theme_classic()
    

p3=ggplot(data.frame(x =true.parameters[,'alpha_2'], y =apply(pars$alpha_2, 2, mean)),aes(x=x,y=y))+geom_point()+
  labs(title='',
       subtitle = paste('r=',round(cor(true.parameters[,'alpha_2'], apply(pars$alpha_2, 2, mean)),2)),
       x=expression(alpha_2['true']),
       y=expression(alpha_2['recovered']))+ 
  xlim(0,1)+ylim(0,1)+
  theme_classic()

p4=ggplot(data.frame(x =true.parameters[,'alpha_3'], y =apply(pars$alpha_3, 2, mean)),aes(x=x,y=y))+geom_point()+
  labs(title='',
       subtitle = paste('r=',round(cor(true.parameters[,'alpha_3'], apply(pars$alpha_3, 2, mean)),2)),
       x=expression(alpha_3['true']),
       y=expression(alpha_3['recovered']))+ 
  xlim(0,1)+ylim(0,1)+
  theme_classic()

p5=ggplot(data.frame(x =true.parameters[,'alpha_4'], y =apply(pars$alpha_4, 2, mean)),aes(x=x,y=y))+geom_point()+
  labs(title='',
       subtitle = paste('r=',round(cor(true.parameters[,'alpha_4'], apply(pars$alpha_4, 2, mean)),2)),
       x=expression(alpha_4['true']),
       y=expression(alpha_4['recovered']))+ 
  xlim(0,1)+ylim(0,1)+
  theme_classic()

p6=ggplot(data.frame(x =true.parameters[,'alpha_5'], y =apply(pars$alpha_5, 2, mean)),aes(x=x,y=y))+geom_point()+
  labs(title='',
       subtitle = paste('r=',round(cor(true.parameters[,'alpha_5'], apply(pars$alpha_5, 2, mean)),2)),
       x=expression(alpha_5['true']),
       y=expression(alpha_5['recovered']))+ 
  xlim(0,1)+ylim(0,1)+
  theme_classic()

p7=ggplot(data.frame(x =true.parameters[,'alpha_6'], y =apply(pars$alpha_6, 2, mean)),aes(x=x,y=y))+geom_point()+
  labs(title='',
       subtitle = paste('r=',round(cor(true.parameters[,'alpha_6'], apply(pars$alpha_6, 2, mean)),2)),
       x=expression(alpha_6['true']),
       y=expression(alpha_6['recovered']))+ 
  xlim(0,1)+ylim(0,1)+
  theme_classic()


annotate_figure(ggarrange(p1,p2,p3,p4,p5,p6,p7,nrow=1,ncol=7), 
                top = text_grob("Individual Level Parameters (random effects)", color = "black", face = "bold", size = 10))


# Aim: Run some basic sanity checks using linear / logistic regression

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(effects)
load('./data/simdata.Rdata')

#sanity check 1: plot mean reward vs expected value. This has to be positive (higher excpected value - higer mean reward)
model<-glmer(reward ~ expval_ch+(1| subject),data = df, family = binomial)
plot(effect('expval_ch',model))


#sanity check 2: pStay model-agnostic analysis.
#Show a tendency to stay with the same bandit after reward, and switch after unrewarded trials
df=df%>%mutate(stay=(action==lag(action,default=0))*1,
               reward_oneback=lag(reward,default=0))

model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))


#sanity check 3: plot mean reward vs parameters
model<-glm(reward ~ poly(alpha,2)+beta, 
           data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
           family = binomial)
plot(effect('alpha',model)) #(should get some hyperbolic function, mostly when beta is fixed across all subjects)
plot(effect('beta',model))  #(should see some positive linear trend)

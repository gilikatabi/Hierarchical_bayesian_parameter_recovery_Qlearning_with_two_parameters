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
#Show a tendency to stay with the same first choice after reward, and switch after unrewarded trials
df=df%>%mutate(stay=(choice1==lag(choice1,default=0))*1,
               reward_oneback=lag(reward,default=0))

model<-glmer(stay ~ reward_oneback+(reward_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

#sanity check 3: pStay model-agnostic analysis.
#Show a tendency to stay with the same first and second choices after reward, and switch after unrewarded trials
df=df%>%mutate(stay_2=(choice1==lag(choice1,default=0) & choice2==lag(choice2,default=0))*1)

model<-glmer(stay_2 ~ reward_oneback+(reward_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

#sanity check 4: pStay model-agnostic analysis.
#Show a tendency to stay with the same choices (1,2,3) after reward, and switch after unrewarded trials
df=df%>%mutate(stay_3=(choice1==lag(choice1,default=0) & choice2==lag(choice2,default=0) & choice3==lag(choice3,default=0))*1)

model<-glmer(stay_3 ~ reward_oneback+(reward_oneback| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

#sanity check 5: pStay model-agnostic analysis.
#Show a tendency to stay with the same second choice (if you stay first choice) after reward, and switch after unrewarded trials
df=df%>%mutate(stay_2=(choice1==lag(choice1,default=0) & choice2==lag(choice2,default=0))*1)

model<-glmer(stay_2 ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter(stay==1), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

#sanity check 6: pStay model-agnostic analysis.
#Show a tendency to stay with the same third choice (if you stay first and second choices) after reward, and switch after unrewarded trials
df=df%>%mutate(stay_2=(choice1==lag(choice1,default=0) & choice2==lag(choice2,default=0) & choice3==lag(choice3,default=0))*1)

model<-glmer(stay_2 ~ reward_oneback+(reward_oneback| subject), 
             data = df%>%filter((stay==1) & (stay_2==1)), 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
plot(effect('reward_oneback',model))

#sanity check 7: plot mean reward vs parameters
model<-glm(reward ~ poly(alpha,2)+beta, 
           data = merge(df,as.data.frame(true.parameters),by=c('subject')), 
           family = binomial)
plot(effect('alpha',model)) #(should get some hyperbolic function, mostly when beta is fixed across all subjects)
plot(effect('beta',model))  #(should see some positive linear trend)

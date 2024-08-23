# Libraries that you will need
library(this.path) # to get path to the current directory
library(ggplot2) # for data visualization
library(tidyverse)

setwd(this.dir()) # set this folder as a working directory
# We will save all generated data files to this directory

############################################
### 14) PSYCHOMETRIC FUNCTION FOR THE PONZO ILLUSION
#GENERATIVE PARAMETERS of the psychometric function
mu=12 # mu (aka PSE)
sd=8
lapse=0.12

#Test stimuli
stim=seq(-50,50,10) # 0 = objectively equal to the reference
#From GENERATIVE MODEL:
prob=(1-lapse)*pnorm(stim,mu,sd)+0.5*lapse+
  rnorm(stim,0,0.04) #plus variation
prob=ifelse(prob<0,0,ifelse(prob>1,1,prob)) #check if values go below 0 and 1 and replace with 0 and 1

#Visualization
ggplot()+geom_point(aes(stim,prob))

#data generation
n=100 #number of trials per stimulus condition

#Generate responses
responses=matrix(nrow=n, ncol=length(stim))
for (j in 1:length(stim)) {
  freq=round(n*prob[j],0)
  responses[,j]=sample(c(rep(1,freq), rep(0,n-freq)),n)
}

#RECORD DATA
stim.long=as.vector(t(replicate(n,stim))) #long format of stimuli*trials
resp=as.vector(responses) #responses in a long format
df14=data.frame(stim.long,resp)
colnames(df14)=c("diff", "response")

df14_10 <- df14

df<- rbind(df14_1, df14_2, df14_3, df14_4, df14_5, df14_6, df14_7, df14_8, df14_9, df14_10)
df$ppt <- rep(1:10, each = 1100)

write.csv(df, "Case14.csv") #save the generated data as .csv to this folder

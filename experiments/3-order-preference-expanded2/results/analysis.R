library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
#library(tidyr)

#setwd("~/Documents/git/spanish_adjectives/experiments/3-order-preference-expanded2/Submiterator-master")
setwd("~/git/spanish_adjectives/experiments/3-order-preference-expanded2/Submiterator-master")

#### first run of experiment
num_round_dirs = 14
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    '../../2-order-preference-expanded/Submiterator-master/round', i, '/spanish-order-expanded.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df1$workerid = paste("vi.",df1$workerid)

d1 = subset(df1, select=c("workerid","noun","gender","nounclass","slide_number", "predicate1", "predicate2", "class1","class2","response","language","school","age","assess","education","lived","level","family","years","describe","classes"))

#### second run of experiment
num_round_dirs = 22
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/spanish-order-expanded.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","noun","gender","nounclass","slide_number", "predicate1", "predicate2", "class1","class2","response","language","school","age","assess","education","lived","level","family","years","describe","classes"))

d = rbind(d1,d)

# re-factorize
d[] <- lapply( d, factor) 

t = d[d$describe=="SpanSpan",]

# only look at "both8" for lived
t = t[t$lived=="both8",]

# only look at "español" as the native language
t = t[t$language!="English"&t$language!="english"&!is.na(t$language)&t$language!=""&t$language!="gbhj",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

# t = d[d$language=="Espanol"|d$language=="espanol"|d$language=="espanol "|
#         d$language==" Español"|d$language=="Española"|d$language=="spanish"|d$language=="Castellano"|
#         d$language=="Español, de España"|d$language=="SPANISH"|d$language=="castellano"|
#         d$language=="Español, Catalan"|d$language=="espanol, vasco"|d$language=="Español e italiano"|
#         d$language=="ESPAÑOL E ITALIANO",]

t$response = as.numeric(as.character(t$response))

summary(t) # 48 indicated "spanish" as native language

#write.csv(t,"~/git/spanish_adjectives/experiments/3-order-preference-expanded2/results/order-preference-spanish-only.csv")

#####
## duplicate observations by first predicate
#####

library(tidyr)

o <- t
o$rightpredicate1 = o$predicate2
o$rightpredicate2 = o$predicate1
o$rightresponse = 1-o$response
agr = o %>% 
        select(predicate1,rightpredicate1,response,rightresponse,workerid,noun,nounclass,class1,class2) %>%
        gather(predicateposition,predicate,predicate1:rightpredicate1,-workerid,-noun,-nounclass,-class1,-class2)
agr$correctresponse = agr$response
agr[agr$predicateposition == "rightpredicate1",]$correctresponse = agr[agr$predicateposition == "rightpredicate1",]$rightresponse
agr$correctclass = agr$class1
agr[agr$predicateposition == "rightpredicate1",]$correctclass = agr[agr$predicateposition == "rightpredicate1",]$class2
head(agr[agr$predicateposition == "rightpredicate1",])
agr$response = NULL
agr$rightresponse = NULL
agr$class1 = NULL
agr$class2 = NULL
nrow(agr) #2860
#write.csv(agr,"~/git/spanish_adjectives/experiments/3-order-preference-expanded2/results/naturalness-duplicated.csv")

adj_agr = aggregate(correctresponse~predicate*correctclass,FUN=mean,data=agr)
adj_agr

class_agr = aggregate(correctresponse~correctclass,FUN=mean,data=agr)

source("../results/helpers.r")

class_s = bootsSummary(data=agr, measurevar="correctresponse", groupvars=c("correctclass"))

ggplot(data=class_s,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/class_distance.pdf",height=3)


# class plot with adjectives
ggplot(data=class_s,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse))+
  geom_bar(stat="identity",fill="white",color="grey")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5)+
  geom_jitter(data=adj_agr,aes(y=correctresponse),alpha=.25,color="red") +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#ggsave("../results/class_distance_jitter.pdf",height=3)

# adjectives plot
adj_s = bootsSummary(data=agr, measurevar="correctresponse", groupvars=c("correctclass","predicate"))
ggplot(data=adj_s,aes(x=reorder(predicate,-correctresponse,mean),y=correctresponse))+
  geom_bar(stat = "summary", fun.y = "mean",fill="white",color="grey")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(predicate,-correctresponse,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective")+
  ylab("distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/adjective_distance.pdf",height=5)

# full violin plot with data points
ggplot(data=agr,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse))+
  geom_violin()+
  geom_jitter(data=agr,aes(y=correctresponse),alpha=.25,color="red") +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  #ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()
#ggsave("../results/class_violin_jitter.pdf",height=3)



#### comparison with faultless disgareement

f = read.csv("../../4-faultless-disagreement/results/pred-subjectivity.csv",header=T)

adj_agr$subjectivity = f$response[match(adj_agr$predicate,f$predicate)]

gof(adj_agr$correctresponse,adj_agr$subjectivity)
# r = -0.07, r2 = 0.01
results <- boot(data=adj_agr, statistic=rsq, R=10000, formula=correctresponse~subjectivity)
boot.ci(results, type="bca") 
# 95%   ( 0.0000,  0.0619 ) 

ggplot(adj_agr, aes(x=subjectivity,y=correctresponse)) +
  geom_point() +
  #geom_smooth()+
  stat_smooth(method="lm")+
  geom_text(aes(label=predicate),size=2.5,vjust=1.5)+
  ylab("naturalness\n")+
  xlab("\nsubjectivity")+
  theme_bw()
#ggsave("../results/naturalness-subjectivity.pdf",height=3,width=4)

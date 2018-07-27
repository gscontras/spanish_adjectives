library(ggplot2)
library(reshape2)
library(lme4)

#setwd("~/Documents/git/cocolab/adjective_ordering/experiments/8-order-preference-and/Submiterator-master")
setwd("~/git/spanish_adjectives/experiments/8-order-preference-and/Submiterator-master")

d = read.table("order-preference-trials.tsv",sep="\t",header=T)
head(d)


s = read.table("order-preference-subject_information.tsv",sep="\t",header=T)
head(s)

d$language = s$language[match(d$workerid,s$workerid)]

d = d[d$language!="SPANISH",]
length(unique(d$workerid)) ## 59 native English

o <- d
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
nrow(agr) #3600

eng_conj_agr <- agr


adj_agr = aggregate(correctresponse~predicate*correctclass,FUN=mean,data=agr)
adj_agr

class_agr = aggregate(correctresponse~correctclass,FUN=mean,data=agr)

#source("../results/helpers.r")

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








old_d = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/3-order-preference/Submiterator-master/order-preference-trials.tsv",sep="\t",header=T)
old_d$modification = "original\nexperiment"
head(old_d)

summary(d)

d = rbind(d,old_d)

d$configuration = paste(d$class1,d$class2)

agg = aggregate(response~configuration*modification,data=d,mean)

ggplot(d, aes(x=response,color=modification)) +
  #geom_histogram(alpha=0.5,aes(y = ..density..)) + 
  geom_density(alpha=0.5) +
  xlab("acceptability rating")
ggsave("../results/density_plot.pdf")



## get faultles disagreement ratings

f = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/2-faultless-disagreement/Submiterator-master/faultless-disagreement-2-trials.tsv",sep="\t",header=T)
head(f)
f_agr = aggregate(response~class,data=f,mean)
p_agr = aggregate(response~predicate,data=f,mean)

d$class1_f = f_agr$response[match(d$class1,f_agr$class)]
d$class2_f = f_agr$response[match(d$class2,f_agr$class)]

d$pred1_f = p_agr$response[match(d$predicate1,p_agr$predicate)]
d$pred2_f = p_agr$response[match(d$predicate2,p_agr$predicate)]

d$f_ratio = (d$class1_f/d$class2_f)
d$f_diff = (d$class1_f-d$class2_f)
d$p_ratio = (d$pred1_f/d$pred2_f)
d$p_diff = (d$pred1_f-d$pred2_f)
d$sentence = paste(d$predicate1,d$predicate2,d$noun)

## by class plot

#d_s = bootsSummary(data=d, measurevar="response", groupvars=c("f_diff","modification"))

d_s = aggregate(response~f_diff+modification,data=d,mean)
#d_s = aggregate(response~f_diff*configuration,data=d,mean)

ggplot(d_s, aes(x=configuration,y=response,color=modification)) +
  geom_point() +
  geom_smooth() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("faultless disagreement") +
  ggtitle("by-class plot")
ggsave("../results/class_plot.pdf")



s = read.table("~/Documents/git/cocolab/adjective_ordering/experiments/6-subjectivity/Submiterator-master/subjectivity-trials.tsv",sep="\t",header=T)
head(s)
s_agr = aggregate(response~class,data=s,mean)
p_agr = aggregate(response~predicate,data=s,mean)

d$class1_s = s_agr$response[match(d$class1,s_agr$class)]
d$class2_s = s_agr$response[match(d$class2,s_agr$class)]

d$pred1_s = p_agr$response[match(d$predicate1,p_agr$predicate)]
d$pred2_s = p_agr$response[match(d$predicate2,p_agr$predicate)]

d$s_ratio = (d$class1_s/d$class2_s)
d$s_diff = (d$class1_s-d$class2_s)
d$p_ratio = (d$pred1_s/d$pred2_s)
d$p_diff = (d$pred1_s-d$pred2_s)
d$sentence = paste(d$predicate1,d$predicate2,d$noun)

## by class plot

#d_s = bootsSummary(data=d, measurevar="response", groupvars=c("s_diff"))

d_s = aggregate(response~s_diff,data=d,mean)
d_s = aggregate(response~f_diff*configuration,data=d,mean)

ggplot(d_s, aes(x=s_diff,y=response)) +
  geom_point() +
  #geom_text(aes(label=configuration))+
  ylab("acceptability") +
  xlab("subjectivity") +
  ggtitle("by-class plot")
ggsave("../results/class_plot_subjectivity.pdf")


## correlations

head(d)
cor(d$response,d$f_diff) # 0.69
cor(d$response,d$s_diff) # 0.68



#ggplot(d, aes(x=f_diff,y=response)) +
 #        geom_point() +
  #geom_smooth()


## by predicate plot

#ggplot(d, aes(x=p_diff,y=response)) +
#  geom_point() +
#  geom_smooth()

p_s = bootsSummary(data=d, measurevar="response", groupvars=c("p_diff"))

p_s = aggregate(response~p_diff*sentence,data=d,mean)

ggplot(p_s, aes(x=p_diff,y=response)) +
  geom_point(alpha=0.25) +
  ylab("acceptability") +
  xlab("faultless disagreement") +
  geom_text(size=2,alpha=0.75,aes(label=sentence),angle=45)+
  ggtitle("by-predicate plot")
ggsave("../results/pred_plot.pdf",width=12,height=10)

library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
#library(tidyr)

setwd("~/git/spanish_adjectives/experiments/5-order-preference-no-conjunction/Submiterator-master")

d = read.csv("spanish-order-no-conjunction-trials.csv",header=T)
s = read.csv("spanish-order-no-conjunction-subject_information.csv",header=T)

d$language = s$language[match(d$workerid,s$workerid)]
d$describe = s$describe[match(d$workerid,s$workerid)]
d$lived = s$lived[match(d$workerid,s$workerid)]
d$age = s$age[match(d$workerid,s$workerid)]


## find native speakers

t = d[d$describe=="SpanSpan",]

# only look at "both8" for lived
t = t[t$lived=="both8",]

# only look at "español" as the native language
#t = t[t$language!="English"&t$language!="english"&!is.na(t$language)&t$language!=""&t$language!="gbhj",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

t$age = as.numeric(as.character(t$age))
mean(t[!is.na(t$age),]$age)
#summary(t) 

length(unique(t$workerid))# 22 indicated "spanish" as native language (180)


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

agr$response = 1-agr$correctresponse

spanish_agr <- agr

adj_agr = aggregate(response~predicate*correctclass,FUN=mean,data=agr)
adj_agr

class_agr = aggregate(response~correctclass,FUN=mean,data=agr)

source("../results/helpers.r")

class_s = bootsSummary(data=agr, measurevar="response", groupvars=c("correctclass"))

ggplot(data=class_s,aes(x=reorder(correctclass,-response,mean),y=response))+
  geom_bar(stat="identity",fill="lightgray",color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-response,mean), width=0.1))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("\nadjective class")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/class_distance.pdf",height=3)
#ggsave("../results/spanish-no-conjunction-ordering.pdf",height=2.5,width=7)

# class plot with adjectives
class_s$correctclass = factor(class_s$correctclass,levels=c("size","quality","texture","age","shape","color","nationality"))
ggplot(data=class_s,aes(x=reorder(correctclass,-response,mean),y=response))+
  geom_bar(stat="identity",fill="lightgrey",color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-response,mean), width=0.1),alpha=1)+
  geom_jitter(data=adj_agr,aes(y=response),alpha=.75,color="red") +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5)+
  geom_hline(yintercept=0.5,linetype="dashed") +
  xlab("\nadjective class")+
  ylab("preferred distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#ggsave("../results/class_distance_jitter.png",height=2.7)

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


#### comparison with English results
eng_conj_agr = read.csv("~/git/spanish_adjectives/experiments/8-order-preference-and/results/eng_conj_agr.csv")
# eng_agr = read.csv("~/git/adjective_ordering/experiments/analysis/naturalness-duplicated.csv")
tagalog = read.csv("~/git/tagalog_adjectives/experiments/2-tagalog-preference/results/naturalness-duplicated.csv")
tagalog$modification = NA
eng_conj_agr$response = eng_conj_agr$correctresponse
eng_conj_agr$expt = "English conjunction" #n = 59
eng_conj_agr[eng_conj_agr$modification=="hierarchical",]$expt = "English baseline" #n = 59
# eng_agr$response = eng_agr$correctresponse
# eng_agr$expt = "English (Scontras et al., 2017)" # n= 45
spanish_agr$X = "NA"
spanish_agr$modification = "NA"
spanish_agr$expt = "Spanish" # n = 48
tagalog$expt = "Tagalog"
tagalog$response = tagalog$correctresponse

d_all = rbind(eng_agr,eng_conj_agr,spanish_agr)
#d_all = rbind(eng_agr,spanish_agr)

d_all = na.omit(d_all)

d_all$class = as.character(d_all$correctclass)

d_all[d_all$correctclass=="nationality"|d_all$correctclass=="material",]$class <- "nationality/\nmaterial"

#summary(lmer(response~class*expt+(1|workerid)+(1|noun),data=d_all))


## Spanish, English, and English conjunction
d_all = rbind(eng_conj_agr,spanish_agr)
d_all = na.omit(d_all)
d_all$class = as.character(d_all$correctclass)
d_all[d_all$correctclass=="nationality"|d_all$correctclass=="material",]$class <- "nationality/\nmaterial"

class_s = bootsSummary(data=d_all, measurevar="response", groupvars=c("class","expt"))
class_s$class = factor(class_s$class,levels=c("size","texture","quality","age","shape","color","nationality/\nmaterial"))

ggplot(data=class_s,aes(x=class,y=response,fill=expt))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=class, width=0.2), position=position_dodge(width=0.9))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("adjective class")+
  ylab("preferred\ndistance from noun\n")+
  ylim(0,1)+
  labs(fill="experiment")+
  # scale_fill_manual(values=c("#7376FE", "#FC726F"))+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/LSA-class-distance.png",height=3)
#ggsave("../results/frankfurt-class-distance.png",height=3,width=6.5)


## Spanish, Tagalog, English, and English conjunction
d_all = rbind(eng_agr,eng_conj_agr,spanish_agr,tagalog)
d_all = na.omit(d_all)
d_all$class = as.character(d_all$correctclass)
d_all[d_all$correctclass=="nationality"|d_all$correctclass=="material",]$class <- "nationality/\nmaterial"
class_s = bootsSummary(data=d_all, measurevar="response", groupvars=c("class","expt"))

class_s$expt = factor(class_s$expt,levels=c("English (Scontras et al., 2017)",
                                            "English conjunction",
                                            "Tagalog",
                                            "Spanish"))

ggplot(data=class_s,aes(x=reorder(class,-response,mean),y=response,fill=expt))+
  geom_bar(stat="identity",position=position_dodge(),color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.2), position=position_dodge(width=0.9))+
  geom_hline(yintercept=0.5,linetype="dashed") + 
  xlab("adjective class")+
  ylab("preferred\ndistance from noun\n")+
  ylim(0,1)+
  labs(fill="experiment")+
  # scale_fill_manual(values=c("#7376FE", "#FC726F"))+
  theme_bw()#+
#theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/SALT-class-distance.png",height=2,width=7.5)






#### comparison with faultless disgareement

f = read.csv("../../4-faultless-disagreement/results/pred-subjectivity.csv",header=T)

adj_agr$subjectivity = f$response[match(adj_agr$predicate,f$predicate)]

gof(adj_agr$response,adj_agr$subjectivity)
# r = 0.75, r2 = 0.56
results <- boot(data=adj_agr, statistic=rsq, R=10000, formula=response~subjectivity)
boot.ci(results, type="bca") 
# 95%   ( 0.2305,  0.7272 )  

ggplot(adj_agr, aes(x=subjectivity,y=response)) +
  geom_point() +
  #geom_smooth()+
  stat_smooth(method="lm",color="black")+
  #geom_text(aes(label=predicate),size=2.5,vjust=1.5)+
  ylab("preferred distance from noun\n")+
  xlab("\nsubjectivity score")+
  ylim(0,1)+
  #geom_text(label=adj_agr$predicate) +
  # xlim(0.2,0.8)+
  theme_bw()
#ggsave("../results/naturalness-subjectivity-spanish-no-conjunction.png",height=3,width=3.5)
####ggsave("../results/naturalness-subjectivity-spanish-LSA-proceedings.png",height=2.7,width=3)
#ggsave("../results/spanish-no-conjunction-scatter.pdf",height=2.75,width=3.15)


ggplot(adj_agr, aes(x=subjectivity,y=response)) +
  geom_point() +
  #geom_smooth()+
  stat_smooth(method="lm",color="black")+
  #geom_text(aes(label=predicate),size=2.5,vjust=1.5)+
  ylab("preferred\ndistance from noun\n")+
  # ylab("")+
  xlab("\nsubjectivity score")+
  ggtitle("Spanish")+
  ylim(0.2,0.8)+
  # xlim(0.2,0.8)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("../results/naturalness-subjectivity-spanish-SALT.png",height=2,width=2.5)

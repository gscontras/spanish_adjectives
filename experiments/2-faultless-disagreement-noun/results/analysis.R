library(ggplot2)
library(reshape2)
library(lme4)
library(hydroGOF)

setwd("~/Documents/git/cocolab/adjective_ordering/experiments/11-faultless-disagreement-noun/Submiterator-master")

#source("../results/splithalf.R")
source("../results/helpers.R") # for bootsSummary() and boot


d = read.table("faultless-disagreement-noun-trials.tsv",sep="\t",header=T)
head(d)
s = read.table("faultless-disagreement-noun-subject_information.tsv",sep="\t",header=T)
head(s)
summary(d)
d$language = s$language[match(d$workerid,s$workerid)]
unique(d$language)
all <- d
# only native English speakers (n=45)
d = d[d$language!="Mandarin",]
unique(d$workerid)
summary(d)

#d$class <- factor(d$class,levels=c("quality","size","age","texture","color","shape","material"))

table(d$class,d$nounclass)

## class plot
d_s = bootsSummary(data=d, measurevar="response", groupvars=c("class"))
#d_s = aggregate(response~class,data=d,mean)
# save data for aggregate plot
#write.csv(d_s,"~/Documents/git/cocolab/adjective_ordering/presentations/DGfS/plots/faultless.csv")

class_plot <- ggplot(d_s, aes(x=reorder(class,-response,mean),y=response)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1),position=position_dodge(width=0.9))+
  ylab("faultless disagreement\n")+
  xlab("\nadjective class") +
  ylim(0,1) +
  theme_bw()
class_plot
#ggsave("../results/class_plot.pdf",height=3)

f <- d

model.7 = lm(response~predicate, data=f)
model.11 = lm(response~predicate+noun:predicate, data=f)
anova(model.7,model.11)
summary(model.7)
summary(model.11)

f_agr_pred = aggregate(response~predicate,data=f,mean)
f_agr_class = aggregate(response~class,data=f,mean)

#load in order preference
o = read.csv("~/Documents/git/cocolab/adjective_ordering/experiments/10-order-preference-noun/Submiterator-master/naturalness-duplicated.csv",header=T)
head(o)
o_agr_pred = aggregate(correctresponse~predicate*correctclass,data=o,mean)
o_agr_class = aggregate(correctresponse~correctclass,data=o,mean)
head(o_agr_pred)
# explainable variance
o$workerID = o$workerid + 1
o$response = o$correctresponse
o$class = o$correctclass
#library(dplyr)
# needs to not have plyr loaded/cannot use bootsSummary()
#prophet(splithalf_class(o, 100), 2) # 0.99 class configuration
#prophet(splithalf_pred(o, 100), 2) # 0.98 predicate configuration
#f$workerID = f$workerid
#prophet(splithalf_pred(f, 100), 2) # 

## FAULTLESS
# PREDICATE
o_agr_pred$faultless = f_agr_pred$response[match(o_agr_pred$predicate,f_agr_pred$predicate)]
gof(o_agr_pred$correctresponse,o_agr_pred$faultless) # r = .91, r2 = .84
results <- boot(data=o_agr_pred, statistic=rsq, R=10000, formula=correctresponse~faultless)
boot.ci(results, type="bca") # 95%   ( 0.6392,  0.9120 ) 
# CLASS
o_agr_class$faultless = f_agr_class$response[match(o_agr_class$correctclass,f_agr_class$class)]
gof(o_agr_class$correctresponse,o_agr_class$faultless) # r = .92, r2 = .85
results <- boot(data=o_agr_class, statistic=rsq, R=10000, formula=correctresponse~faultless)
boot.ci(results, type="bca") # 95%   ( 0.1281,  0.9546 )   


# plot order preference against faultless
ggplot(o_agr_pred, aes(x=faultless,y=correctresponse)) +
  geom_point() +
  geom_smooth(method=lm,color="black") +
  xlab("\nfaultless disagreement rating")+
  ylab("naturalness rating\n")+
  #ylim(0,1)+
  #scale_y_continuous(breaks=c(.25,.50,.75))+
  theme_bw()
#ggsave("../results/naturalness-faultless-new-nouns.png",height=3,width=3.5)

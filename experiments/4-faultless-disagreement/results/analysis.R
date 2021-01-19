library(ggplot2)
library(reshape2)
library(lme4)
library(dplyr)

setwd("~/git/spanish_adjectives/experiments/4-faultless-disagreement/Submiterator-master/")

source("../results/helpers.r")

num_round_dirs = 12
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/faultless-disagreement.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","firstutterance","noun","nounclass","slide_number", "predicate",  "class","response","language","school","age","assess","education","lived","level","family","years","describe","classes","gender.1"))

# re-factorize
d[] <- lapply( d, factor) 

t = d[d$describe=="SpanSpan",]

# only look at "both8" for lived
t = t[t$lived=="both8",]

# only look at "español" as the native language
t = t[t$language!="English"&t$language!="english"&!is.na(t$language)&t$language!=""&t$language!="gbhj"&t$language!="Ingles"&t$language!="ingles"&t$language!="tamil"&t$language!="TAMIL"&t$language!="yes"&t$language!="N"&t$language!="YES"&t$language!="170"&t$language!="NO",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

# t = d[d$language=="Espanol"|d$language=="espanol"|d$language=="espanol "|
#         d$language==" Español"|d$language=="Española"|d$language=="spanish"|d$language=="Castellano"|
#         d$language=="Español, de España"|d$language=="SPANISH"|d$language=="castellano"|
#         d$language=="Español, Catalan"|d$language=="espanol, vasco"|d$language=="Español e italiano"|
#         d$language=="ESPAÑOL E ITALIANO",]

t$response = as.numeric(as.character(t$response))
t$age = as.numeric(as.character(t$age))

summary(t) 
length(unique(t$workerid)) # 21 indicated "spanish" as native language
#write.csv(t,"../results/spanish-faultless.csv")

t$class <- factor(t$class,levels=c("quality","size","age","texture","color","shape","nationality"))

table(t$class,t$nounclass)

## class plot
d_s = bootsSummary(data=t, measurevar="response", groupvars=c("class"))
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

agr_pred = aggregate(response~predicate*class,data=t,mean)

#write.csv(agr_pred,"../results/pred-subjectivity.csv")





adj_agr = aggregate(response~predicate*class,FUN=mean,data=t)
adj_agr
class_s = bootsSummary(data=t, measurevar="response", groupvars=c("class"))

# class plot with adjectives
class_s$class = factor(class_s$class,levels=c("size","quality","texture","age","shape","color","nationality"))
ggplot(data=class_s,aes(x=reorder(class,-response,mean),y=response))+
  geom_bar(stat="identity",fill="lightgrey",color="black")+
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class,-response,mean), width=0.1),alpha=1)+
  geom_jitter(data=adj_agr,aes(y=response),alpha=.75,color="red") +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(correctclass,-correctresponse,mean), width=0.1),alpha=0.5)+
  #geom_hline(yintercept=0.5,linetype="dashed") +
  xlab("\nadjective class")+
  ylab("perceived subjectivity\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
#ggsave("../results/class_subjectivity_jitter.png",height=2.7)
library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
#library(tidyr)

setwd("~/Documents/git/spanish_adjectives/experiments/1-order-preference/Submiterator-master")
setwd("~/git/spanish_adjectives/experiments/1-order-preference/Submiterator-master")

num_round_dirs = 4
df = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/spanish-order.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))

d = subset(df, select=c("workerid","noun","gender","nounclass","slide_number", "predicate1", "predicate2", "class1","class2","response","language","school","age","assess","education","lived","level","family","years","describe","classes"))

# re-factorize
d[] <- lapply( d, factor) 


# only look at "español" as the native language
t = d[d$language=="Espanol"|d$language=="espanol"|d$language=="espanol ",]

# only look at "both8" for lived
t = t[t$lived=="both8",]

t$response = as.numeric(as.character(t$response))

summary(t) # 11 indicated "spanish" as native language

#write.csv(t,"~/Documents/git/spanish_adjectives/experiments/1-order-preference/results/order-preference-spanish-only.csv")

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
nrow(agr) #416
#write.csv(agr,"~/Documents/git/spanish_adjectives/experiments/1-order-preference/results/naturalness-duplicated.csv")

adj_agr = aggregate(correctresponse~predicate*correctclass,FUN=mean,data=agr)
adj_agr

class_agr = aggregate(correctresponse~correctclass,FUN=mean,data=agr)

ggplot(data=class_agr,aes(x=reorder(correctclass,-correctresponse,mean),y=correctresponse))+
  geom_bar(stat="identity")+
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=reorder(class1,-correctresponse,mean), width=0.1),alpha=0.5)+
  xlab("\nadjective class")+
  ylab("distance from noun\n")+
  ylim(0,1)+
  #labs("order\npreference")+
  theme_bw()#+
  #theme(axis.text.x=element_text(angle=90,vjust=0.35,hjust=1))
#ggsave("../results/class_distance.pdf",height=3)






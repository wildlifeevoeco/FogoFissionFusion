# Description of fusion

#where are fusion events, where are fusion0?
library(ggplot2)
library(data.table)
library(gridExtra)

fusion=readRDS('output/07-dyads.Rds')

Fusion=subset(fusion, fusion0==TRUE)

#mosaic graph

#extract what I need: landcover at a dyad centroid and dyadLC

#Fusion[,.(dyadLC,dyadID)]
Fusion=as.data.table(Fusion)
A=ggplot(Fusion,aes(x = fusion0,fill = dyadLC)) + 
  geom_bar(position = "fill") + xlab('Fusion site') + labs(fill='Landcover type')+ ylab('Proportion')+ggtitle('Where do fusion events happen?')

#compare to where they are when they are in a dyad

inadyad=subset(fusion, min2==TRUE)

B=ggplot(inadyad,aes(x = min2,fill = dyadLC)) + 
  geom_bar(position = "fill") + xlab('hanging site') + labs(fill='Landcover type')+ ylab('Proportion')+ggtitle('Where do they stay when in a dyad?')

#Where are they when thay split?


split=subset(fusion, end==TRUE)
C=ggplot(split,aes(x = end,fill = dyadLC)) + 
  geom_bar(position = "fill") + xlab('Splitting site') + labs(fill='Landcover type')+ ylab('Proportion')+ggtitle('Where do splitting event happen?')


grid.arrange (A,B,C, ncol=3)

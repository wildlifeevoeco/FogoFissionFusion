# Description of fusion

#where are fusion events, where are fusion0?
library(ggplot2)
library(data.table)
library(gridExtra)

fusion=readRDS('output/07-dyads.Rds')

Fusion=subset(fusion, fusion0==TRUE)

fusion[, count_fuse := length(fusion0), by = .(fusion0)]


## color palette
cbPalette <- c("#999999", "red", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E69F00")

## function to re-order bars on figure
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = T)))
}

## delete NAs from dyad habitat type
fusion <- fusion[!is.na(dyadLC)]

## change name of false/true to fission/fusion
fusion$fusion0[fusion$fusion0 == FALSE] <- "fused"
fusion$fusion0[fusion$fusion0 == TRUE] <- "fusion"

## change name of anthro
fusion$dyadLC[fusion$dyadLC == "Anthropogenic and disturbance"] <- "Anthropogenic"


a1 <- ggplot(fusion[fusion0 == "fusion"], aes(reorder_size(dyadLC), count_fuse, fill=dyadLC)) +
  stat_summary(fun = "sum", geom="bar") +
  xlab('') +
  ylab('Frequency') +
  ggtitle('a) dyad fusion') +
  ylim(0,5000) +
  scale_fill_manual(values=cbPalette) + 
  theme(legend.position = 'none',
        axis.text.x=element_text(size=12, color = "black", angle = 45, hjust = 1),
        axis.text.y=element_text(size=14, color = "black"),
        axis.title.y=element_text(size=14),
        strip.text = element_text(size=14,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
b1 <- ggplot(fusion[fusion0 == "fused"], aes(reorder_size(dyadLC), count_fuse, fill=dyadLC)) +
  stat_summary(fun = "sum", geom="bar") +
  xlab('') +
  ylab('Frequency') +
  ggtitle('b) dyads fused for >1 time step') +
  ylim(0,5000) +
  scale_fill_manual(values=cbPalette) + 
  theme(legend.position = 'none',
        axis.text.x=element_text(size=12, color = "black", angle = 45, hjust = 1),
        axis.text.y=element_text(size=14, color = "black"),
        axis.title.y=element_text(size=14),
        strip.text = element_text(size=14,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
grid.arrange(a1, b1, nrow = 1)



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

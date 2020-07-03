# Description of fusion

#where are fusion events, where are fusion0?
library(ggplot2)
library(data.table)
library(gridExtra)
library(patchwork)

fusion=readRDS('output/07-dyads.Rds')

#takes only the time step in between fission and fusion events = fused
fusion[ , start:= ifelse(start==TRUE,1,0)]
fusion[ , end:= ifelse(end==TRUE,1,0)]
fusion[ , fused:= rowSums(fusion[,23:24])]
fusion=fusion[ min2==TRUE]

fusion[, count_fuse := length(fused), by = .(fused)]
fusion[ , count_fusion := length(start), by=.(start)]

## color palette
cbPalette <- c("#EF6C00", "#d90000", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#ffab91", "#CC79A7","#E69F00")


## delete NAs from dyad habitat type
fusion <- fusion[!is.na(dyadLC)]
fusion[ , .N, by=dyadLC]

## change name of anthro
fusion$dyadLC[fusion$dyadLC == "Anthropogenic and disturbance"] <- "Anthropogenic"
fusion$dyadLC=as.factor(fusion$dyadLC)

#set order manually
table(fusion$dyadLC, fusion$start)
fusion$dyadLC <- factor(fusion$dyadLC,levels = c("Lichen and Heath", "Wetland", "Rocky Barren", "Conifer Scrub",
                                                 "Water", "Anthropogenic","Conifer Forest",
                                                 "Broadleaf" ))

A <- ggplot(fusion[start== 1], aes(reorder(dyadLC, -count_fusion), count_fusion, fill=dyadLC)) +
  stat_summary(fun = "sum", geom="bar") +
  xlab('') +
  ylab('Number of points') +
  ylim(0,600)+
  ggtitle('a) Dyad fusion event') +
  scale_fill_manual(values=cbPalette) + 
  theme(legend.position = 'none',
        axis.text.x=element_text(size=12, color = "black", angle = 45, hjust = 1),
        axis.text.y=element_text(size=14, color = "black"),
        axis.title.y=element_text(size=14),
        strip.text = element_text(size=14,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

#set order manually
table(fusion$dyadLC, fusion$fused)
fusion$dyadLC <- factor(fusion$dyadLC,levels = c("Lichen and Heath", "Wetland", "Rocky Barren", "Conifer Scrub",
                                                 "Water", "Anthropogenic","Mixed Wood ","Conifer Forest",
                                                 "Broadleaf" ))

B <- ggplot(fusion[fused == 0], aes(x=reorder(dyadLC, -count_fuse), y=count_fuse, fill=dyadLC)) +
  stat_summary(fun = "sum", geom="bar") +
  geom_bar(stat="identity")+
  xlab('') +
  ylab('') +
  ggtitle('b) Dyad walk') +
  ylim(0,2000) +
  scale_fill_manual(values=cbPalette) + 
  theme(legend.position = 'none',
        axis.text.x=element_text(size=12, color = "black", angle = 45, hjust = 1),
        axis.text.y=element_blank(),
        axis.title.y=element_text(size=14),
        strip.text = element_text(size=14,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

A+B






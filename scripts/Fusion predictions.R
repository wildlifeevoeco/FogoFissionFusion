
# Packages ----------------------------------------------------------------
libs <- c('data.table', 'raster','ggplot2','patchwork')
lapply(libs, require, character.only = TRUE)

# Input data -------------------------------------------------------------------
legend <- fread('../nl-landcover/input/FINAL_PRODUCT/FINAL_RC_legend.csv')
landcover <- raster('../nl-landcover/output/fogo_lc.tif')


# Random nb of data points on Fogo
random=sampleRandom(landcover, size=1000, na.rm=TRUE, xy=TRUE)

#merge legend value and LC
random=as.data.table(random)
colnames(random)[3] <-"Value"
 
random=merge(random,legend, by='Value')
random=random[ ,.(Landcover)]
random
random$Landcover[random$Landcover == "Anthropogenic and disturbance"] <- "Anthropogenic"



fusion <- readRDS('output/07-dyads.Rds')
# SUBSET ONLY FUSION

fusion=fusion[ start==TRUE]
fusion[ ,.N, by =dyadrun]
fusion<-fusion[!is.na(dyadLC)]
fusion$dyadLC[fusion$dyadLC == "Anthropogenic and disturbance"] <- "Anthropogenic"

# sample random fusion locations x 1000
samplefusion=fusion[sample(nrow(fusion), 1000), ] 
samplefusion=samplefusion[ , .(dyadLC)]
samplefusion[,Landcover:=dyadLC]
samplefusion[,dyadLC:=NULL]

samplefusion

# compare the two
dataR=table(random$Landcover)
dataF=table(samplefusion$Landcover)
dataR=as.data.table(dataR)
dataF=as.data.table(dataF)


# add row, broadleaf=0 if à in the sample
#dataF=rbind(data.frame(V1='Broadleaf', N=0),dataF)
MAT <- matrix(c(dataR$N,dataF$N), ncol=2)

# Chi2
chisq.test(MAT)

## color palette
cbPalette <- c("#EF6C00", "#d90000", "#56B4E9", "#009E73", 
               "#F0E442", "#0072B2", "#ffab91", "#CC79A7","#E69F00")

## function to re-order bars on figure
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = T)))
}

A=ggplot(dataF, aes(reorder(V1, -N), N, fill=V1)) +
  stat_summary(fun = "sum", geom="bar") +
  xlab('') +
  ylab('Number of points') +
  ggtitle('a) Observed fusion sites') +
  ylim(0,500) +
  scale_fill_manual(values=cbPalette) + 
  theme(legend.position = 'none',
        axis.text.x=element_text(size=12, color = "black", angle = 45, hjust = 1),
        axis.text.y=element_text(size=14, color = "black"),
        axis.title.y=element_text(size=14),
        strip.text = element_text(size=14,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
        
B=ggplot(dataR, aes(reorder(V1,-N), N, fill=V1)) +
  stat_summary(fun = "sum", geom="bar") +
  xlab('') +
  ylab('') +
  ggtitle('b) Random fusion sites') +
  ylim(0,500) +
  scale_fill_manual(values=cbPalette) + 
  theme(legend.position = 'none',
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=12, color = "black", angle = 45, hjust = 1),
        strip.text = element_text(size=14,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

A+B


#TODO : why not the same number on the graph but same nb in reality?
#water disappeared!! BECAUSE IT IS RANDOM MORON!
#TODO: why not the same scale for Y
dataF[ ,sum:=sum(N)]
dataR[ ,sum:=sum(N)]


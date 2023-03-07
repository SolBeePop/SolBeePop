### SolBeePop ####################################################
### Cross-species simulations
### Script for plotting simulation outputs
### Author: Amelie Schmolke / Colleen Roy
### Date: December 2022
###################################################################

rm(list = ls()) # clean up workspace
library(plyr)
library(reshape)
library(ggplot2)
library(gridExtra)

## Inputs ---------------------------------------------------------------------
ofp = 'C:\\SolBeePop\\Cross_Species_Simulations\\'  # output file path

## Set Up ---------------------------------------------------------------------
setwd(ofp)
sls = c(
  'O. bicornis' = 'osmia_bicornis.csv',
  'M. rotundata' = 'megachile_rotundata.csv',
  'N. melanderi' = 'nomia_melanderi.csv',
  'E. pruinosa' = 'eucera_pruinosa.csv')
ols = c('count.turtles', 'bees.emerged.yr', 'f.emerged.yr', 'm.emerged.yr', 
        'bees.nesting', 'bees.nesting.today',
        'sum.cells.today', 'sum.f.cells.today', 'sum.m.cells.today',
        'sum.cells', 'sum.f.cells', 'sum.m.cells',
        'mean.cells.today', 'mean.f.cells.today', 'mean.m.cells.today',
        'mean.cells', 'mean.f.cells', 'mean.m.cells')

## Read simulation outputs -----------------------------------------------------------------
df = data.frame(matrix(nrow = 0, ncol = length(ols) + 5))
colnames(df) = c('sp', 'X.step.', 'year', 'doy', 'DateREP', ols)

for(i in names(sls)){
  tdf = read.csv(sls[[i]], stringsAsFactors = FALSE, skip = 6)
  tdf$sp = i
  df = rbind(df, tdf[colnames(df)])
  rm(tdf)
}
rm(i)

# add offspring female sex ratio
df$sex.ratio = df$sum.f.cells / df$sum.cells

## aggregate by day
df_mean = ddply(df, .(sp,X.step., year, doy,DateREP), summarize,
                avg.total.bees=mean(count.turtles),
                min.total.bees=min(count.turtles),
                max.total.bees=max(count.turtles),
                avg.emerged.yr=mean(bees.emerged.yr),
                min.emerged.yr=min(bees.emerged.yr),
                max.emerged.yr=max(bees.emerged.yr),
                avg.f.emerged.yr=mean(f.emerged.yr),
                min.f.emerged.yr=min(f.emerged.yr),
                max.f.emerged.yr=max(f.emerged.yr),
                avg.m.emerged.yr=mean(m.emerged.yr),
                min.m.emerged.yr=min(m.emerged.yr),
                max.m.emerged.yr=max(m.emerged.yr),
                avg.bees.nesting=mean(bees.nesting),
                min.bees.nesting=min(bees.nesting),
                max.bees.nesting=max(bees.nesting),
                avg.bees.nesting.today=mean(bees.nesting.today),
                min.bees.nesting.today=min(bees.nesting.today),
                max.bees.nesting.today=max(bees.nesting.today),
                avg.sum.cells=mean(sum.cells),
                min.sum.cells=min(sum.cells),
                max.sum.cells=max(sum.cells),
                avg.sum.f.cells=mean(sum.f.cells),
                min.sum.f.cells=min(sum.f.cells),
                max.sum.f.cells=max(sum.f.cells),
                avg.sum.m.cells=mean(sum.m.cells),
                min.sum.m.cells=min(sum.m.cells),
                max.sum.m.cells=max(sum.m.cells),
                avg.mean.cells.today=mean(mean.cells.today),
                min.mean.cells.today=min(mean.cells.today),
                max.mean.cells.today=max(mean.cells.today),
                avg.mean.cells=mean(mean.cells),
                min.mean.cells=min(mean.cells),
                max.mean.cells=max(mean.cells),
                avg.sex.ratio=mean(sex.ratio),
                min.sex.ratio=min(sex.ratio),
                max.sex.ratio=max(sex.ratio))

write.csv(df_mean, 'csp_allspecies_mean.csv', row.names = FALSE)
#df_mean <- read.csv('csp_allspecies_mean.csv')

## Plot Single Variables - EOY ------------------------------------------------
tdf = ddply(df_mean, .(sp, year), transform, eoy = max(doy))
tdf = tdf[tdf$eoy == tdf$doy,]

# generate all plots, no range shown
for(i in ols){
  tdf$tmp = tdf[,i]
  ymax = ceiling(max(tdf$tmp))
  plt = ggplot(tdf) +
    geom_point(aes(x = year, y = tmp, color = sp)) +
    geom_line(aes(x = year, y = tmp, color = sp)) +
    labs(y = i, x = 'Simulation Year') + ylim(0, ymax) +
    theme_bw() +
    theme(legend.title = element_blank(), legend.position = 'bottom') +
    guides(color = guide_legend(nrow = 2))
  ggsave(paste0('plots//', i, '.png'),
    plt, width = 4, height = 4)
}

## Plot Stacked Variables - EOY -----------------------------------------------
tdf = melt(tdf, id.vars = c('sp', 'year'),
  measure.vars = c('avg.sum.f.cells', 'avg.sum.m.cells'))
tdf$variable = factor(tdf$variable, levels = c('avg.sum.m.cells', 'avg.sum.f.cells'))

plt = ggplot(tdf) +
  geom_bar(aes(x = year, y = value, fill = variable), stat = 'identity') +
  scale_fill_manual(values = c('skyblue', 'orange')) +
  labs(y = i, x = 'End of Year') +
  facet_wrap(~sp) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = 'bottom')
ggsave(paste0('plots//sum.m+f.cells_yr.png'),
  plt, width = 4, height = 4)

## plot for MS1: bee number at the end of year, average and range
ymax_beenum = ceiling(max(tdf$max.total.bees))
plt_beenum = ggplot(tdf) +
  geom_ribbon(aes(x = year, ymin=min.total.bees, ymax=max.total.bees, fill=sp), alpha = 0.2) +
  geom_line(aes(x = year, y = avg.total.bees, color = sp)) +
  labs(y = "Bee number (end of year)", x = 'Simulation year') + ylim(0, ymax_beenum) +
  theme(axis.text=element_text(size=24)) + theme(axis.title=element_text(size=24)) +
  theme_bw() +
  theme(legend.position='none')+
  guides(color = guide_legend(nrow = 2)) 

ymax_meancells = ceiling(max(tdf$max.mean.cells))
tdf_1 <- subset(tdf,tdf$year > 0)
plt_meancells = ggplot(tdf_1) +
  geom_ribbon(aes(x = year, ymin=min.mean.cells, ymax=max.mean.cells, fill=sp), alpha = 0.2) +
  geom_line(aes(x = year, y = avg.mean.cells, color = sp)) +
  labs(y = "Brood cells per female", x = 'Simulation year') + ylim(0, ymax_meancells) +
  theme(axis.text=element_text(size=24)) + theme(axis.title=element_text(size=24)) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = 'bottom', legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(nrow = 2)) 

plt<- plot_grid(plt_beenum,plt_meancells,labels = "AUTO", ncol=1, align = 'v', rel_heights = c(1, 1.15))
ggsave("MS_Fig_csp_Feb2023_New.jpg", plt, device="jpeg", width=5,height=7, dpi=300)
ggsave("MS_Fig_csp_Feb2023_New.pdf", plt, device="pdf", width=5,height=7, dpi=300)

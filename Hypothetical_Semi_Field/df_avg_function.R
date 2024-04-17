## Support function for plotting of absolute effects 
## Project: SolBeePop_ecotox
## Author: Amelie Schmolke 
## Last Edited: 19 Dec 2023

f_df_avg <- function(df){
  df_avg <- df %>% 
    group_by(date_corr) %>% # for Osmia semi-field calibration and validation
#    group_by(date) %>% # for case simulations (of hypothetical semi-field studies)
    summarize(avg.f.prenesting.today = mean(f.prenesting),
              min.f.prenesting.today = min(f.prenesting),
              max.f.prenesting.today = max(f.prenesting),
              avg.bees.nesting.today = mean(bees.nesting.today), 
              min.bees.nesting.today = min(bees.nesting.today), 
              max.bees.nesting.today = max(bees.nesting.today), 
              avg.f.postem.today = mean(f.prenesting + bees.nesting.today),
              min.f.postem.today = min(f.prenesting + bees.nesting.today),
              max.f.postem.today = max(f.prenesting + bees.nesting.today),
              avg.sum.cells.today = mean(sum.cells.today),
              min.sum.cells.today = min(sum.cells.today),
              max.sum.cells.today = max(sum.cells.today),
              avg.sum.f.cells.today = mean(sum.f.cells.today),
              min.sum.f.cells.today = min(sum.f.cells.today),
              max.sum.f.cells.today = max(sum.f.cells.today),
              avg.sum.m.cells.today = mean(sum.m.cells.today),
              min.sum.m.cells.today = min(sum.m.cells.today),
              max.sum.m.cells.today = max(sum.m.cells.today),
              avg.sum.cells = mean(sum.cells), 
              min.sum.cells = min(sum.cells), 
              max.sum.cells = max(sum.cells), 
              avg.sum.f.cells = mean(sum.f.cells), 
              min.sum.f.cells = min(sum.f.cells), 
              max.sum.f.cells = max(sum.f.cells), 
              avg.sum.m.cells = mean(sum.m.cells),
              min.sum.m.cells = min(sum.m.cells),
              max.sum.m.cells = max(sum.m.cells),
              avg.bees.emerged.yr = mean(bees.emerged.yr),
              min.bees.emerged.yr = min(bees.emerged.yr),
              max.bees.emerged.yr = max(bees.emerged.yr),
              avg.f.emerged.yr = mean(f.emerged.yr), 
              min.f.emerged.yr = min(f.emerged.yr), 
              max.f.emerged.yr = max(f.emerged.yr), 
              avg.m.emerged.yr = mean(m.emerged.yr), 
              min.m.emerged.yr = min(m.emerged.yr), 
              max.m.emerged.yr = max(m.emerged.yr)) 
  
    return(df_avg)
}

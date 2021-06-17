library(here)
library(readr)
library(ggplot2)

# Read iNEXT results

i.ptero = read_rds(here('data/pteropus_viruses.rds'))
i.mac = read_rds(here('data/macaque_viruses.rds'))

# Extract the data from the iNEXT object
# Pteropus
z = i.ptero[[2]][[1]] %>%
  mutate(type = replace(method, method == "observed", "interpolated"),
         species = 'Pteropus'
         ) 

# Macaque
y = i.mac[[2]][[1]] %>%
  mutate(type = replace(method, method == "observed", "interpolated"),
         species = 'Macaque'
  ) 

# get specific coverage thresholds and observed points
z2 = z %>% filter(method == 'observed')
z50 = filter(z, t == 12)
z99 = filter(z, t == 2075)

y2 = y %>% filter(method == 'observed')
y50 = filter(y, t == 31)
y99 = filter(y, t == 1925)

# Plot
viral_div = 
ggplot(mapping = aes(x = t, y = qD)) +
  geom_ribbon(data = z, aes(ymin = qD.LCL, ymax = qD.UCL), fill = "#F8766D", alpha = 0.2) +
  geom_ribbon(data = y, aes(ymin = qD.LCL, ymax = qD.UCL), fill = "#00BA38", alpha = 0.2) +
  geom_line(data = z,  aes(linetype = rev(type)), color = '#F8766D') +
  geom_line(data = y, aes(linetype = rev(type)), color = '#00BA38') +
  geom_point(data = z2, aes(x = t, y = qD), colour = "#F8766D", size = 5) +
  geom_point(data = z50, aes(x = t, y = qD), colour = "#00BFC4", size = 5) +
  geom_point(data = z99, aes(x = t, y = qD), size = 5) +
  geom_point(data = y2, aes(x = t, y = qD), colour = '#00BA38', size = 5) +
  geom_point(data = y50, aes(x = t, y = qD), colour = "#00BFC4", size = 5) +
  geom_point(data = y99, aes(x = t, y = qD), size = 5) +
  theme_bw() +
  theme(text = element_text(size = 18, face = 'bold', family = 'Helvetica'), 
        legend.position = 'none',
        legend.title = element_blank()
        ) +
  xlab('Samples') +
  ylab('Viral species richness') +
  #scale_shape_manual(values = 19) +
  annotate(geom = "text", x = c(500, 2000), y = c(175, 275), 
           label = c("B" , "C"), family = 'Helvetica', fontface = 'bold', size = 6) +
  annotate(geom = "text", x = c(1100, 2100), y = c(55, 60), 
           label = c("E", "F"), family = 'Helvetica', fontface = 'bold', size = 6) +
  annotate(geom = "text", x = c(120, 120), y = c(50, 10), 
           label = c("A", "D"), family = 'Helvetica', fontface = 'bold', size = 6)

# Save on disk
ggsave(here("figures/viral_diversity.png"), viral_div, 
       device = 'png', width = 2.25, height = 2.25, dpi = 300, scale = 4, units = 'in')


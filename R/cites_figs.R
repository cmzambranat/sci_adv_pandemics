library(tidyverse)
library(here)
library(readxl)
library(scales)
library(lemon)
library(patchwork)
library(rlang)

# Defining some functions for data wranling and plotting

# Data wrangling
cites_data = function(excel_file, importer_country = c('CN', 'US', 'ALL')) {
  if ( {{importer_country}} == 'ALL') {
    read_xlsx( {{excel_file}} ) %>%
      janitor::clean_names() %>%
      group_by(year, app) %>%
      summarise(n_transactions = n() ) %>%
      pivot_wider(names_from = app, values_from = n_transactions) %>%
      janitor::clean_names() %>%
      mutate(app_iii_ii = sum(iii, ii, na.rm = T),
             app_iii_ii_i = sum(iii, ii, i, na.rm = T),
             app_iii = iii) %>%
      select(year, app_iii, app_iii_ii, app_iii_ii_i) %>%
      pivot_longer(-year, names_to = "appendix", values_to = "counts")
  } else {
    read_xlsx( {{excel_file}} )  %>%
      janitor::clean_names() %>%
      filter(importer == {{ importer_country }}) %>%
      group_by(year, app) %>%
      summarise(n_transactions = n() ) %>%
      pivot_wider(names_from = app, values_from = n_transactions) %>%
      janitor::clean_names() %>%
      mutate(app_iii_ii = sum(iii, ii, na.rm = T),
             app_iii_ii_i = sum(iii, ii, i, na.rm = T),
             app_iii = iii) %>%
      select(year, app_iii, app_iii_ii, app_iii_ii_i) %>%
      pivot_longer(-year, names_to = "appendix", values_to = "counts")
  }
}

# Plotting
cites_plot = function(cites_data, year, counts, ylabel, y_scale_limit, legend_label){
  ggplot2::ggplot(data = cites_data, aes(x = {{ year }}, {{ counts }} )) +
    geom_pointpath(aes(color = appendix), linesize = 1) +
    theme_bw() +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x)
        10 ^ x),
      labels = trans_format("log10", math_format(10 ^ .x)),
      limits = c(1, {{y_scale_limit}})
    ) +
    scale_color_brewer(
      type = "qual",
      palette = 'Dark2',
      #name = "CITES Appendix",
      labels = {{legend_label}}
    ) +
    xlab(bquote("Year")) +
    ylab( {{ylabel}} ) +
    theme(text = element_text(size = 12, face = 'bold', family = 'Helvetica'),
          legend.position = c(0.2, 0.8),
          legend.background = element_blank(),
          legend.title = element_blank())
}

## Log
# Subsetting exports from Singapore (SG) to China (CN) and  to USA (US)

SG_all_live = cites_data(excel_file = here('data/CITES US CN animals.xlsx'), 
                         importer_country = 'ALL')

SG_to_CN_live = cites_data(excel_file = here('data/CITES US CN animals.xlsx'), 
                           importer_country = 'CN') 
SG_to_US_live = cites_data(excel_file = here('data/CITES US CN animals.xlsx'), 
                           importer_country = 'US')


# Plots removing Appendix III
# China
p_SG_to_CN_live_log_no_iii = cites_plot((SG_to_CN_live %>%
                                           filter(appendix != 'app_iii')), year, counts, 
                                        ylabel = 'Total CITES transactions to China',
                                        y_scale_limit = 200, 
                                        legend_label = c("Appendix III + II", "Appendix III + II + I"))

# US
p_SG_to_US_live_log_no_iii = cites_plot((SG_to_US_live %>%
                                           filter(appendix != 'app_iii')), year, counts, 
                                        ylabel = 'Total CITES transactions to USA',
                                        y_scale_limit = 150,
                                        legend_label = c("Appendix III + II", "Appendix III + II + I"))

# Patch plots
singapore_trade_log_no_iii = p_SG_to_US_live_log_no_iii + p_SG_to_CN_live_log_no_iii +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Save on disk
ggsave(here("figures/singapore_trade_log_no_iii.png"), singapore_trade_log_no_iii, 
       device = 'png', width = 2.25, height = 2.25, dpi = 300, scale = 4, units = 'in')


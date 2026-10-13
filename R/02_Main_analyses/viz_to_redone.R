library(cowplot)

radek_1 <- 
plot_grid(
  plot_case_studies_year_region_2,
  NULL, 
  nrow = 1,
  rel_widths = c(2, 3)
) +
  ggview::canvas(
    width = 1961 ,
    height = 500,
    units = "px"
    
  )


radek_2 <-
plot_grid(
  legend_plot,
  map_continents,
  NULL, 
  NULL,
  nrow = 1, 
  rel_widths = c(1.5, 4, 2, 2)
) +
  ggview::canvas(
    width = 1961,
    height = 400,
    units = "px"
    
  )


figure1 <- 
plot_grid(
  radek_1,
  radek_2,
  nrow = 2,
  rel_heights = c(500, 400)
  
) +
  ggview::canvas(
    width = 1961 ,
    height = 1200,
    units = "px"
    
  )

figure1

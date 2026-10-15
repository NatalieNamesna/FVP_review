#----------------------------------------------------------#
#
#
#                 The FVP review project
#
#                   Run figure scripts
#
#           N. Namesna, S. Flantua, O. Mottl
#
#                         2026
#
#----------------------------------------------------------#

#  Builds and saves all composite figures to Outputs/Figures/Composites.
#    Each script is run in its own environment so that no object leaks
#    from one figure to the next.

c(
  "Figure_01.R",
  "Figure_02.R",
  "Figure_03.R",
  "Figure_04.R"
) |>
  purrr::walk(
    .f = ~ base::source(
      file = here::here("R/03_Figures", .x),
      local = base::new.env()
    )
  )

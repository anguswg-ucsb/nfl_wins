year = 2022
week = 1
wins <- nflpredictr::predict_games(year = 2022, week = 1)
install.packages("hexSticker")
library(hexSticker)
tmp_gg <-
  wins %>%
  dplyr::mutate(
    size = dplyr::case_when(
      .pred_class == "1" ~ (.pred_1),
      .pred_class == "0" ~ (.pred_0)
    )
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_col(aes(x = home_team, y = .pred_1, fill = .pred_class), alpha = 1) +
  ggplot2::scale_fill_manual(values = c("red3", "forestgreen")) +
  ggplot2::scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, 1), labels = scales::percent) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggplot2::theme_void() +
  hexSticker::theme_transparent() +
  ggplot2::theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = -90, size = 8, color = "white", face = "bold", vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(size = 8, color = "white",  face = "bold", vjust = -0.4),
    panel.background = element_rect(fill = 'black', color = 'black')
  )
tmp_gg

s <- sticker(
  tmp_gg,
  package  = "nflpredictr",
  p_size   = 28,
  s_x      = 1,
  s_y      = 0.8,
  s_width  = 1.2,
  s_height = 0.65, 
  h_fill   = "black",
  h_color  = "white",
  filename = "inst/figures/nflpredictr.png",
  dpi = 500
)

plot(s)
print(s)

# football_svg <- "C:/Users/angus/Downloads/football-solid.svg"
# football_svg <- system.file("C:/Users/angus/Downloads/football-solid.svg")
logo_png <- "C:/Users/angus/OneDrive - Lynker Technologies/Desktop/football_crystal_ball_purple2.png"
# football_svg <- system.file("figures/cat.png", package="hexSticker")
s <- sticker(logo_png, package="nflpredictr", 
        p_size=20,
        s_x=1, s_y=.75, s_width=0.35, s_height = 0.4,
        h_fill   = "black",
        h_color  = "white",
        filename = "inst/figures/nflpredictr.png",
        dpi = 500
)
plot(s)


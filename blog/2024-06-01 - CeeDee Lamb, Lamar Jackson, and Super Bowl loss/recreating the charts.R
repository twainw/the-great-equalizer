library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
library(nflplotR)

# custom theme
gt_theme_f5 <- function(gt_object, ...) {
  
  gt_object %>%
    gt_theme_538() |> 
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(
        font = google_font("Roboto"),
        weight = 700
      )
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(
        font = google_font("Roboto"),
        color = "gray65",
        weight = 400
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "top", color = "black", weight = px(0)
        ),
        cell_text(
          font = google_font("Roboto"),
          #transform = "uppercase",
          v_align = "bottom",
          size = px(14),
          weight = 'bold'
        )
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_stubhead()
      )
    ) %>%
    tab_options(
      data_row.padding = px(7.5),
      heading.border.bottom.style = "none",
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold", 
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 16,
      heading.align = "left",
      ...
    )
}

custom_palette <- as.character(paletteer::paletteer_d("Redmonder::dPBIRdGn"))[3:9]

# CeeDee Lamb over the years chart

lamb_table <- data.frame(
  Season = c("2023", "2022", "2021", "2020"),
  # Drop %, ESPN Open Score, Target Share
  drop = c(34, 43, 112, 113),
  espn_open = c(2, 8, 47, 91),
  tgt_share = c(4, 6, 33, 45),
  # Total EPA, YAC Per Reception, Yards Per Route
  epa = c(1, 5, 47, 75),
  yac = c(56, 67, 45, 83),
  yprr = c(5, 9, 34, 44)
)

lamb_table |> 
  arrange(Season) |> 
  gt() |> 
  cols_label(
    drop = md("Drop<br>%"),
    espn_open = md("ESPN<br>Open<br>Score"),
    tgt_share = md("Target<br>Share"),
    epa = md("Total<br>EPA"),
    yac = md("YAC/Rec"),
    yprr = "YPRR",
    Season = ""
  ) |> 
  tab_header(title = "CeeDee Lamb's Growth Over The Years",
             subtitle = "Regular season rank in each metric [Lower Value = Better]") %>%
  gt_color_rows(-c("Season"), palette = custom_palette, direction = -1, domain = c(1, 115)) |> 
  gt_theme_f5() |> 
  tab_options(table_body.border.bottom.width = '1px', 
              table_body.border.bottom.color = "gray90", 
              data_row.padding = '1px', 
              stub.background.color = 'floralwhite') |> 
  cols_width(Season ~  px(75), 
             everything() ~ px(65)) |> 
  gtsave("lamb_over_the_years.png")


# CeeDee Lamb target share chart

data <- data.frame(
  Player = c("Davante Adams", "Tyreek Hill", "AJ Brown", "CeeDee Lamb", "Garrett Wilson",
             "Stefon Diggs", "Puka Nacua", "DeAndre Hopkins", "Michael Pittman",
             "Amon-Ra St. Brown", "DJ Moore", "Adam Thielen", "Mike Evans", 
             "Ja'Marr Chase", "Keenan Allen"),
  TargetShare = c(33.0, 31.1, 30.1, 29.9, 29.8, 29.4, 28.8, 28.7, 28.6, 28.6, 28.5, 25.7, 24.7, 24.5, 24.4)/100,
  team = c("LV", "MIA", "PHI", "DAL", "NYJ", "BUF", "LAR", "TEN", 
           "IND", "DET", "CHI", "CAR", "TB", "CIN", "LAC")
) |> 
  left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr"))

ggplot(data, aes(x = reorder(Player, TargetShare), y = TargetShare, fill = team)) +
  geom_col(aes(color = team, fill = team), width = 0.7) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.05, hjust = -0.10) +
  geom_text(aes(label = scales::percent(TargetShare, accuracy = 0.1)),
            hjust = 1.5,  
            size = 4, 
            color = 'white', fontface = 'bold') +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  coord_flip(clip = "off") +
  labs(
    title = "CeeDee Lamb is the Cowboys' Engine",
    subtitle = "Top-15 NFL receivers in Target-Share [2023 Regular Season]",
    x = NULL,
    y = "Target Share (%)",
    caption = "Source: #nflverse | Players with min 50 targets | Chart: @twain_w"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Rockwell"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5, margin = margin(b = 7.5)),
    plot.title.position = 'plot',
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10), face = 'bold'),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      size = 12, 
      color = "#505050", 
      hjust = 1,
      margin = margin(r = -12)
    ),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, color = "#808080"),
    legend.position = 'none'
  )

ggsave("tgt_share.png", width = 6.87, height = 6.84, dpi = 300, bg = 'white', type = "cairo")

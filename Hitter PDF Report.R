# Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(gtable)
library(grid)
library(GeomMLBStadiums)
library(ggrepel)

# Working Directory and CSV Data
setwd("~/Downloads/UConnCSV")
data <- read.csv("UConnSeason.csv")

pitcher_data <- data %>% filter(Batter == "Lane, Connor")
hitter_name <- unique(pitcher_data$Batter)[1]
name_split <- strsplit(hitter_name, ", ")[[1]]
formatted_name <- paste(name_split[2], name_split[1])
make_zone_by_side <- function(hitter, pitcher_throws) {
  result <- data %>%
    filter(Batter == hitter, PitcherThrows == pitcher_throws,
           PitchCall %in% c("InPlay", "StrikeSwinging", "StrikeCalled"))
  
  x_breaks <- seq(-1.05, 1.05, length.out = 4)
  y_breaks <- seq(1.6, 3.3, length.out = 4)
  
  tiles <- expand.grid(
    x = x_breaks[-length(x_breaks)] + diff(x_breaks) / 2,
    y = y_breaks[-length(y_breaks)] + diff(y_breaks) / 2
  )
  
  pitches_by_zone <- result %>%
    mutate(
      x_bin = cut(-PlateLocSide, breaks = x_breaks, labels = FALSE, include.lowest = TRUE),
      y_bin = cut(PlateLocHeight, breaks = y_breaks, labels = FALSE, include.lowest = TRUE)
    ) %>%
    filter(!is.na(x_bin) & !is.na(y_bin)) %>%
    group_by(x_bin, y_bin) %>%
    summarise(TotalPitches = n(), .groups = 'drop') %>%
    mutate(Percentage = round(TotalPitches / sum(TotalPitches) * 100, 1))
  
  pitches_by_zone <- pitches_by_zone %>%
    mutate(
      x = (x_breaks[x_bin] + x_breaks[x_bin + 1]) / 2,
      y = (y_breaks[y_bin] + y_breaks[y_bin + 1]) / 2
    )
  
  tiles <- merge(tiles, pitches_by_zone[, c("x", "y", "Percentage")], by = c("x", "y"), all.x = TRUE)
  tiles$Percentage[is.na(tiles$Percentage)] <- 0
  
  zone <- ggplot() +
    geom_tile(data = tiles, aes(x = x, y = y, fill = Percentage), color = "black") +
    scale_fill_gradient2(low = "#3661ad", mid = "white", high = "#d82129", midpoint = mean(tiles$Percentage, na.rm = TRUE)) +
    geom_point(data = result, aes(x = -PlateLocSide, y = PlateLocHeight), color = "gray", size = 1, alpha = 0.6) +
    geom_text(data = tiles, aes(x = x, y = y, label = paste0(Percentage, "%")), color = "black") +
    geom_rect(xmin = -1.05, xmax = 1.05, ymin = 1.6, ymax = 3.3, fill = "transparent", color = "black") +
    ylim(0.75, 3.75) + xlim(-2, 2) +
    theme_classic() +
    xlab("") + ylab("") +
    labs(title = paste("Pitch Percentage per Zone Vs", pitcher_throws, "Handed Pitcher")) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    coord_fixed(ratio = 1.3)
  
  return(zone)
}

# Generate Heatmaps using actual values
heatmap_left <- make_zone_by_side(hitter_name, "Left")
heatmap_right <- make_zone_by_side(hitter_name, "Right")

# Hit Chart Code
hit_chart <- ggplot(
  pitcher_data %>%
    filter(
      (!PlayResult %in% c("CaughtStealing", "StolenBase", "Undefined")) |
        (PitchCall %in% c("StrikeSwinging", "FoulBallNotFieldable") & !PlayResult %in% c("Undefined"))
    ),
  aes(
    x = PlateLocSide,
    y = PlateLocHeight,
    color = PlayResult,
    shape = TaggedPitchType
  )
) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "yellow", "black", "cyan", "magenta", "brown")) +
  labs(
    title = "Swing Chart",
    x = "Horizontal Pitch Location",
    y = "Vertical Pitch Location",
    color = "Swing Result",
    shape = "Pitch Type"
  ) +
  theme_minimal() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4), fill = NA, color = "black", size = 1) +
  xlim(-1.8, 1.8) +
  ylim(1, 4)

# Spray chart
spray_data <- data %>%
  filter(Batter == hitter_name, !PlayResult %in% c("Undefined", "FieldersChoice", "StolenBase", "CaughtStealing", "Sacrifice", "Error")) %>%
  mutate(hc_x = sin(Bearing * pi / 180) * Distance,
         hc_y = cos(Bearing * pi / 180) * Distance)

spray_chart <- ggplot(spray_data, aes(x = hc_x, y = hc_y)) +
  geom_mlb_stadium(stadium_ids = 'dodgers', stadium_transform_coords = TRUE, stadium_segments = 'all', linewidth = 0.5, color = 'black') +
  theme_void() +
  geom_point(aes(fill = PlayResult), shape = 21, colour = 'black', stroke = 0.5, size = 3, alpha = 0.8) +
  scale_fill_manual(values = c("Single" = "blue", "Double" = "red", "Triple" = "yellow", "HomeRun" = "green", "Out" = "black")) +
  coord_fixed() +
  labs(title = "Spray Chart", fill = 'Hit Type') +
  geom_text_repel(aes(label = paste(round(ExitSpeed, 1))), colour = "black", size = 3)


combined_hit_and_spray_chart <- arrangeGrob(
  hit_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  spray_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  ncol = 2
)


# Pitch Performance Statistics
data_filtered <- pitcher_data %>%
  mutate(
    InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
    Swing = PitchCall %in% c("FoulBallNotFieldable", "StrikeSwinging", "InPlay"),
    Chase = ifelse(!InStrikeZone & Swing, 1, 0),
    ZSwing = ifelse(InStrikeZone & Swing, 1, 0),
    HardHitCheck = ifelse(!is.na(ExitSpeed) & ExitSpeed >= 95 & ExitSpeed <= 120 & PitchCall == "InPlay", 1, 0)
  )

statistics <- data_filtered %>%
  group_by(TaggedPitchType) %>%
  summarise(
    Pitches = n(),
    PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
    AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
    H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
    `1B` = sum(PlayResult == "Single"),
    `2B` = sum(PlayResult == "Double"),
    `3B` = sum(PlayResult == "Triple"),
    HR = sum(PlayResult == "HomeRun"),
    SO = sum(KorBB == "Strikeout"),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    AVG = round(sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
    OBP = round((sum(H, na.rm = TRUE) + sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE)) / sum(PA, na.rm = TRUE), 3),
    SLG = round((sum(`1B`, na.rm = TRUE) + 2*sum(`2B`, na.rm = TRUE) + 3*sum(`3B`, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE)) / sum(AB, na.rm = TRUE), 3),
    OPS = (OBP + SLG),
    "Swing%" = round(sum(Swing) / Pitches * 100, 1),
    "Whiff%" = round(sum(PitchCall == "StrikeSwinging") / sum(Swing) * 100, 1),
    "Chase%" = round(sum(Chase) / sum(Swing) * 100, 1),
    "Strikeout%" = round(sum(SO) / sum(AB) * 100, 1),
    "Walk%" = round(sum(BB) / sum(AB) * 100, 1),
    "GroundBall%" = round(sum(TaggedHitType == "GroundBall") / sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")) * 100, 1),
    "FlyBall%" = round(sum(TaggedHitType == "FlyBall") / sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")) * 100, 1),
    "LineDrive%" = round(sum(TaggedHitType == "LineDrive") / sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")) * 100, 1),
    AvgExitVelocity = round(mean(ExitSpeed, na.rm = TRUE), 1),
    MaxExitVelocity = round(max(ExitSpeed, na.rm = TRUE), 1),
    "HardHit%" = round(sum(HardHitCheck, na.rm = TRUE) / sum(PitchCall == "InPlay", na.rm = TRUE) * 100, 1)
  ) %>%
  select(TaggedPitchType, Pitches, `Swing%`, `Whiff%`, `Chase%`, `GroundBall%`, `FlyBall%`, `LineDrive%`, `AvgExitVelocity`, `MaxExitVelocity`, `HardHit%`, `AVG`) %>%
  arrange(desc(Pitches))

# Row for "All"
all_statistics <- data_filtered %>%
  summarise(
    TaggedPitchType = "All",
    Pitches = n(),
    PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
    AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
    H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
    `1B` = sum(PlayResult == "Single"),
    `2B` = sum(PlayResult == "Double"),
    `3B` = sum(PlayResult == "Triple"),
    HR = sum(PlayResult == "HomeRun"),
    SO = sum(KorBB == "Strikeout"),
    BB = sum(KorBB == "Walk"),
    HBP = sum(PitchCall == "HitByPitch"),
    AVG = round(sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
    OBP = round((sum(H, na.rm = TRUE) + sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE)) / sum(PA, na.rm = TRUE), 3),
    SLG = round((sum(`1B`, na.rm = TRUE) + 2*sum(`2B`, na.rm = TRUE) + 3*sum(`3B`, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE)) / sum(AB, na.rm = TRUE), 3),
    OPS = (OBP + SLG),
    "Swing%" = round(sum(Swing) / Pitches * 100, 1),
    "Whiff%" = round(sum(PitchCall == "StrikeSwinging") / sum(Swing) * 100, 1),
    "Chase%" = round(sum(Chase) / sum(Swing) * 100, 1),
    "Strikeout%" = round(sum(SO) / sum(AB) * 100, 1),
    "Walk%" = round(sum(BB) / sum(AB) * 100, 1),
    "GroundBall%" = round(sum(TaggedHitType == "GroundBall") / sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")) * 100, 1),
    "FlyBall%" = round(sum(TaggedHitType == "FlyBall") / sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")) * 100, 1),
    "LineDrive%" = round(sum(TaggedHitType == "LineDrive") / sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")) * 100, 1),
    AvgExitVelocity = round(mean(ExitSpeed, na.rm = TRUE), 1),
    MaxExitVelocity = round(max(ExitSpeed, na.rm = TRUE), 1),
    "HardHit%" = round(sum(HardHitCheck, na.rm = TRUE) / sum(PitchCall == "InPlay", na.rm = TRUE) * 100, 1)
  ) %>%
  select(TaggedPitchType, Pitches, `Swing%`, `Whiff%`, `Chase%`, `GroundBall%`, `FlyBall%`, `LineDrive%`, `AvgExitVelocity`, `MaxExitVelocity`, `HardHit%`, `AVG`) %>%
  arrange(desc(Pitches))

# Combine pitch-specific statistics with the "All" row
combined_statistics <- bind_rows(statistics, all_statistics)

performance_title <- textGrob(
  "Stats By Pitch Type",
  gp = gpar(fontsize = 14),
  just = "center",
  y = 0.5, 
  x = 0.5,
  vjust = 5
)


# Performance table
performance_grob <- tableGrob(
  combined_statistics,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.6)),
    colhead = list(fg_params = list(cex = 0.7))
  )
)

# Combine the title and the performance table into a single grob
performance_grob_with_title <- grid.arrange(
  performance_title,
  performance_grob,
  ncol = 1,
  heights = c(0.025, 0.975) 
)


# Calculate Statistics
calculateStatistics <- function(data_filtered) {
  data_filtered <- data_filtered %>%
    mutate(
      InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
      Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
      Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0)
    )
  
  first_pitch_strikes <- data_filtered %>%
    filter(PitchofPA == 1 & PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "StrikeCalled", "StrikeSwinging"))
  
  first_pitch_strikes_count <- nrow(first_pitch_strikes)
  first_pitch_strike_percentage <- round(first_pitch_strikes_count / nrow(data_filtered %>% filter(PitchofPA == 1)) * 100, 1)
  
  earned_runs <- sum((data_filtered %>% filter(PlayResult != "Error"))$RunsScored, na.rm = TRUE)
  
  statistics <- data_filtered %>%
    summarise(
      PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
      AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !PlayResult %in% c("Sacrifice")),
      H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
      `1B` = sum(PlayResult == "Single"),
      `2B` = sum(PlayResult == "Double"),
      `3B` = sum(PlayResult == "Triple"),
      HR = sum(PlayResult == "HomeRun"),
      SO = sum(KorBB == "Strikeout"),
      BB = sum(KorBB == "Walk"),
      HBP = sum(PitchCall == "HitByPitch"),
      AVG = round(sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), 3),
      OBP = round((sum(H, na.rm = TRUE) + sum(BB, na.rm = TRUE) + sum(HBP, na.rm = TRUE)) / sum(PA, na.rm = TRUE), 3),
      SLG = round((sum(`1B`, na.rm = TRUE) + 2*sum(`2B`, na.rm = TRUE) + 3*sum(`3B`, na.rm = TRUE) + 4*sum(HR, na.rm = TRUE)) / sum(AB, na.rm = TRUE), 3),
      OPS = OBP + SLG
    )
  
  return(statistics)
}

statistics_table <- calculateStatistics(pitcher_data)

# Statistics Table
statistics_title <- textGrob(
  "Game Statistics",
  gp = gpar(fontsize = 14),
  just = "center",
  y = 0.5
)

# Convert Statistics Table to Grob
statistics_grob <- tableGrob(
  statistics_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(cex = 0.6)),
    colhead = list(fg_params = list(cex = 0.7))
  )
)

# Combine the title and the statistics table into a single grob
statistics_grob_with_title <- grid.arrange(
  statistics_title,
  statistics_grob,
  ncol = 1,
  heights = c(0.025, 0.975) 
)

# Title grob using formatted name
title_grob <- textGrob(
  paste0(formatted_name, " Hitting Report"),
  x = 0.5, y = 0.5, just = "center",
  gp = gpar(fontsize = 20, fontface = "bold")
)

# Define Fancy Plot Theme
fancy_plot_theme <- theme(
  plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
  legend.title = element_text(face = "bold", size = 10),
  legend.text = element_text(size = 9)
)

# Define Enhanced Table Theme with Borders
fancy_table_theme <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 8, fontface = "plain", fontfamily = "sans"),
    bg_params = list(fill = "white", col = "black")
  ),
  colhead = list(
    fg_params = list(fontsize = 9, fontface = "bold", col = "white"),
    bg_params = list(fill = "navyblue", col = "black")
  )
)

# Redefine styled titles
performance_title <- textGrob(
  "Stats By Pitch Type",
  gp = gpar(fontsize = 14, fontface = "bold.italic", col = "black", fontfamily = "sans"),
  just = "center", y = 0.5, x = 0.5, vjust = 5
)

statistics_title <- textGrob(
  "Game Statistics",
  gp = gpar(fontsize = 14, fontface = "bold.italic", col = "black", fontfamily = "sans"),
  just = "center", y = 0.5
)

# Apply theme to plots
hit_chart <- hit_chart + fancy_plot_theme
spray_chart <- spray_chart + fancy_plot_theme
heatmap_left <- heatmap_left + fancy_plot_theme
heatmap_right <- heatmap_right + fancy_plot_theme

# Update tables with theme
performance_grob <- tableGrob(combined_statistics, rows = NULL, theme = fancy_table_theme)
statistics_grob <- tableGrob(statistics_table, rows = NULL, theme = fancy_table_theme)

# Combine the title and the performance table into a single grob
performance_grob_with_title <- grid.arrange(performance_title, performance_grob, ncol = 1, heights = c(0.025, 0.975))
statistics_grob_with_title <- grid.arrange(statistics_title, statistics_grob, ncol = 1, heights = c(0.025, 0.975))

# Combine Tables
combined_tables_grob <- arrangeGrob(
  grobs = list(statistics_grob_with_title, performance_grob_with_title),
  nrow = 2,
  heights = c(0.05, 0.22)
)

# Apply theme to combined chart
combined_hit_and_spray_chart <- arrangeGrob(
  hit_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  spray_chart + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
  ncol = 2
)

# Final layout
report_layout <- arrangeGrob(
  grobs = list(
    title_grob,
    combined_tables_grob,
    arrangeGrob(
      heatmap_left + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
      heatmap_right + theme(plot.margin = unit(c(1, 1, 1, 1), "cm")),
      ncol = 2
    ),
    combined_hit_and_spray_chart
  ),
  nrow = 4,
  heights = c(0.4, 1.2, 1.2, 1.2)
)

# Save the report
filename_safe <- gsub(", ", "_", hitter_name)
pdf_filename <- paste0(formatted_name, " Hitting Report.pdf")
pdf(pdf_filename, width = 10, height = 14)
grid.draw(report_layout)
dev.off()

cat("PDF saved as '", pdf_filename, "'\n", sep = "")










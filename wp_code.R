library(tidyverse)
library(stringr)

# Research Statement
# Correlation between pitcher platoon advantage (LL, RR) and strikeout rate from 2015 - 2019

# Upload data sets
atbats <- read.csv("atbats.csv")
atbats_2019 <- read.csv("2019_atbats.csv")
atbats_total <- rbind(atbats, atbats_2019)

# LHB vs. LHP
left_left <- filter(atbats_total, stand == "L" & p_throws == "L")

left_left$is.strikeout <- if_else(left_left$event == "Strikeout", 1, 0)
left_left$is.flyout <- if_else(left_left$event == "Flyout", 1, 0)
left_left$is.groundout <- if_else(left_left$event == "Groundout", 1, 0)
left_left$is.lineout <- if_else(left_left$event == "Lineout", 1, 0)
left_left$is.popout <- if_else(left_left$event == "Pop Out", 1, 0)

left_left$is.single <- if_else(left_left$event == "Single", 1, 0)
left_left$is.double <- if_else(left_left$event == "Double", 1, 0)
left_left$is.triple <- if_else(left_left$event == "Triple", 1, 0)
left_left$is.homerun <- if_else(left_left$event == "Home Run", 1, 0)

# RHB vs. RHP
right_right <- filter(atbats_total, stand == "R" & p_throws == "R")

right_right$is.strikeout <- if_else(right_right$event == "Strikeout", 1, 0)
right_right$is.flyout <- if_else(right_right$event == "Flyout", 1, 0)
right_right$is.groundout <- if_else(right_right$event == "Groundout", 1, 0)
right_right$is.lineout <- if_else(right_right$event == "Lineout", 1, 0)
right_right$is.popout <- if_else(right_right$event == "Pop Out", 1, 0)

right_right$is.single <- if_else(right_right$event == "Single", 1, 0)
right_right$is.double <- if_else(right_right$event == "Double", 1, 0)
right_right$is.triple <- if_else(right_right$event == "Triple", 1, 0)
right_right$is.homerun <- if_else(right_right$event == "Home Run", 1, 0)

# LHB vs. RHP
left_right <- filter(atbats_total, stand == "L" & p_throws == "R")

left_right$is.strikeout <- if_else(left_right$event == "Strikeout", 1, 0)
left_right$is.flyout <- if_else(left_right$event == "Flyout", 1, 0)
left_right$is.groundout <- if_else(left_right$event == "Groundout", 1, 0)
left_right$is.lineout <- if_else(left_right$event == "Lineout", 1, 0)
left_right$is.popout <- if_else(left_right$event == "Pop Out", 1, 0)

left_right$is.single <- if_else(left_right$event == "Single", 1, 0)
left_right$is.double <- if_else(left_right$event == "Double", 1, 0)
left_right$is.triple <- if_else(left_right$event == "Triple", 1, 0)
left_right$is.homerun <- if_else(left_right$event == "Home Run", 1, 0)

# RHB vs. LHP
right_left <- filter(atbats_total, stand == "R" & p_throws == "L")

right_left$is.strikeout <- if_else(right_left$event == "Strikeout", 1, 0)
right_left$is.flyout <- if_else(right_left$event == "Flyout", 1, 0)
right_left$is.groundout <- if_else(right_left$event == "Groundout", 1, 0)
right_left$is.lineout <- if_else(right_left$event == "Lineout", 1, 0)
right_left$is.popout <- if_else(right_left$event == "Pop Out", 1, 0)

right_left$is.single <- if_else(right_left$event == "Single", 1, 0)
right_left$is.double <- if_else(right_left$event == "Double", 1, 0)
right_left$is.triple <- if_else(right_left$event == "Triple", 1, 0)
right_left$is.homerun <- if_else(right_left$event == "Home Run", 1, 0)

# Out Events by Batter-Pitcher Situation

df_strikeout <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.strikeout), mean(right_right$is.strikeout),
             mean(left_right$is.strikeout), mean(right_left$is.strikeout)),
  "Event" = rep("Strikeout", 4)
)

df_flyout <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.flyout), mean(right_right$is.flyout),
             mean(left_right$is.flyout), mean(right_left$is.flyout)),
  "Event" = rep("Fly Out", 4)
)

df_groundout <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.groundout), mean(right_right$is.groundout),
             mean(left_right$is.groundout), mean(right_left$is.groundout)),
  "Event" = rep("Ground Out", 4)
)

df_lineout <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.lineout), mean(right_right$is.lineout),
             mean(left_right$is.lineout), mean(right_left$is.lineout)),
  "Event" = rep("Lineout", 4)
)

df_popout <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.popout), mean(right_right$is.popout),
             mean(left_right$is.popout), mean(right_left$is.popout)),
  "Event" = rep("Pop Out", 4)
)

df_outs <- rbind(df_strikeout, df_flyout, df_groundout, df_lineout, df_popout)

# Sum of hitting rates by batter-pitcher situation

sum(df_strikeout$Rate[1], df_flyout$Rate[1], df_groundout$Rate[1], df_lineout$Rate[1], df_popout$Rate[1])
sum(df_strikeout$Rate[2], df_flyout$Rate[2], df_groundout$Rate[2], df_lineout$Rate[2], df_popout$Rate[2])
sum(df_strikeout$Rate[3], df_flyout$Rate[3], df_groundout$Rate[3], df_lineout$Rate[3], df_popout$Rate[3])
sum(df_strikeout$Rate[4], df_flyout$Rate[4], df_groundout$Rate[4], df_lineout$Rate[4], df_popout$Rate[4])

# Data Visualization (1)
ggplot(df_outs, aes(x = reorder(Situation, Rate), y = Rate, fill = reorder(Event, Rate))) + 
  geom_col(position = "stack", width = 0.75) + ylim(0, 0.7) +
  labs(title = "Plot of Out Events by Batter-Pitcher Situation", 
       x = "Situation", fill = "Event") + 
  scale_fill_brewer() + theme_bw()

# Hitting Events Rate by Batter-Pitcher Situation

df_single <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.single), mean(right_right$is.single),
             mean(left_right$is.single), mean(right_left$is.single)),
  "Event" = rep("Single", 4)
)

df_double <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.double), mean(right_right$is.double),
             mean(left_right$is.double), mean(right_left$is.double)),
  "Event" = rep("Double", 4)
)

df_triple <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.triple), mean(right_right$is.triple),
             mean(left_right$is.triple), mean(right_left$is.triple)),
  "Event" = rep("Triple", 4)
)

df_homerun <- data.frame(
  "Situation" = c("LHB vs. LHP", "RHB vs. RHP", "LHB vs. RHP", "RHB vs. LHP"),
  "Rate" = c(mean(left_left$is.homerun), mean(right_right$is.homerun),
             mean(left_right$is.homerun), mean(right_left$is.homerun)),
  "Event" = rep("Home Run", 4)
)

df_hits <- rbind(df_single, df_double, df_triple, df_homerun)

# Sum of hitting rates by batter-pitcher situation

sum(df_homerun$Rate[1], df_triple$Rate[1], df_double$Rate[1], df_single$Rate[1])
sum(df_homerun$Rate[2], df_triple$Rate[2], df_double$Rate[2], df_single$Rate[2])
sum(df_homerun$Rate[3], df_triple$Rate[3], df_double$Rate[3], df_single$Rate[3])
sum(df_homerun$Rate[4], df_triple$Rate[4], df_double$Rate[4], df_single$Rate[4])

# Data Visualization (2)

ggplot(df_hits, aes(x = reorder(Situation, Rate), y = Rate, fill = reorder(Event, Rate))) + 
  geom_col(position = "stack", width = 0.75) + ylim(0, 0.25) +
  labs(title = "Plot of Hitting Events by Batter-Pitcher Situation", 
       x = "Situation", fill = "Event") + 
  scale_fill_brewer(palette = "Reds") + theme_bw()
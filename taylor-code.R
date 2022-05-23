library(mosaic)
library(readr)
pitch <- read_csv("Downloads/Statcast_2021.csv") # import dataset 
type <- select(pitch, c('pitch_type', 'events', 'description')) # extract necessary columns 
type
tally(~ pitch_type, data = type, margins = TRUE) # sorts pitch types 
type <- filter(type, pitch_type != "NA" & pitch_type != "CS" & pitch_type != "FA" & pitch_type != 'KN') # filter out NA outcomes 
type <- filter(type, events != "NA")
type <- filter(type, events == "strikeout") # overwrite type dataframe only keeping the plays that resulted in strikeout 
tally(~pitch_type, data = type, margins = TRUE)
tally(~pitch_type, data = type, format = "proportion")
slices <- c(1063, 4688, 2232, 13387, 1109, 4204, 11030, 4260)
lbls <- c("Splitter", "Changeup", "Cutter", "Four-seam Fastball", "Knuckle-Curve","Sinker", "Slider", "Curveball")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Distribution of Strikeouts by Pitch Type (21-22)")

ggplot(type, aes(x = pitch_type)) +
  geom_bar(fill="dodgerblue")

kertahni <- select(pitch, c('pitch_type', 'release_speed', 'player_name', 'pitcher', 'events', 'zone', 'p_throws', 'hit_location', 'balls', 'strikes', 'outs_when_up'))
kertahni <- filter(kertahni, player_name == "Ohtani, Shohei" | player_name == "Kershaw, Clayton")
kertahni 
histogram(~ release_speed, data = kertahni)
kertahni$pitcher <- recode <- recode(kertahni$pitcher, "660271" = "Ohtani", "477132" =
         "Kershaw")
histogram(~zone, data = kertahni)
kershaw <- filter(kertahni, player_name == "Kershaw, Clayton")
tally(~pitch_type, data = kershaw, format = "proportion")
kershout <- filter(kershaw, events == "strikeout")
shohei <- filter(kertahni, player_name == "Ohtani, Shohei")
tally(~pitch_type, data = shohei, format = "proportion")
shoout <- filter(shohei, events == "strikeout")
tally(~pitch_type, data = shoout, format = "proportion")

library(ggplot2)
bp <- ggplot(kertahni, aes(x=pitcher, y=release_speed, group=pitcher)) + 
  geom_boxplot(aes(fill=pitcher))
bp

ggplot(data=kershout, aes(x=pitcher, y=pitch_type, fill=pitch_type)) + geom_bar(stat="identity")

boxplot(release_speed~pitch_type,data=kertahni, main="Ohtani's Release Speeds by Pitch Type",
        xlab="Pitch Type", ylab="Release Speed (MPH)", col = "red")

histogram(~release_speed, data = kertahni)

p <- ggplot(data = kertahni, aes(x = events, y= release_speed, color = pitcher)) + geom_point()
p + facet_wrap(~pitcher)

strikeouts <- filter(kertahni, events == "strikeout")
ggplot(data=strikeouts, aes(x=pitcher, y=pitch_type, fill=pitch_type)) + geom_bar(stat="identity")
ggplot(data=strikeouts, aes(x=pitcher, y=pitch_type, fill=pitch_type)) + geom_bar(stat="identity") + ggtitle("Kershaw vs Ohtani: Strikeouts by Pitch Type")

tally(~pitch_type, data = shoout, format = "proportion")

scorpos <- select(pitch, c('on_2b', 'on_3b', 'pitch_type', 'player_name', 'release_speed', 'events'))
tally(~on_2b, data = scorpos)
tally(~on_3b, data = scorpos)
scorpos <- filter(scorpos, on_2b != 'NA')
scorpos <- filter(scorpos, on_3b != 'NA')
scorpos <- filter(scorpos, events != 'NA')
scorpos <- filter(scorpos, pitch_type != 'NA' & pitch_type != 'EP' & pitch_type != 'CS' & pitch_type != 'FA')
ggplot(scorpos, aes(x = pitch_type)) +
  geom_bar(fill="dodgerblue")
tally(~events, data = pitch)
# out <- filter(scorpos, events == 'caught_stealing_2b' | events == 'caught_stealing_3b' | events == 'caught_stealing_home' | events == 'double_play' | events == 'field_out' | events == 'fielders_choice_out' | events == 'force_out' | events == 'grounded_into_double_play' | events == 'other_out' | events == 'sac_fly_double_play' | events == 'sac_bunt_double_play' | events == 'strikeout' | events == 'triple_play')
out <- filter(scorpos, (events == "single" | events == "double" | events == "triple" | events == "sac_fly" | events == "sac_bunt" | events == "home_run" | events == "walk" | events == "wild_pitch"))
tally(~events, data = out)
ggplot(data=out, aes(x=pitch_type, y=events, fill=events)) + geom_bar(stat="identity") + ggtitle("Pitch Type vs Undesired Outcome with Runners in Scoring Position (21-22)")

              
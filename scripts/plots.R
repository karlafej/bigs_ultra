library(readr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(dplyr)
library(forcats)

## Read data =====
laps = read_csv("./data/2018_laps.csv", col_types = "ciiiccc")
leader = read_csv("./data/2018_leaderboard.csv", 
                  col_types = cols(`Avg loop` = col_character(),
                                   `Slow loop` = col_character(),
                                   `Race Time` = col_character()))


laps[is.na(laps$measurement),]
laps[is.na(laps$lap_split),]
laps[is.na(laps$rest_time),]

laps <-
  laps %>% 
  group_by(name) %>% 
  mutate(total = cumsum(as.numeric(ms(lap_split)))) %>% 
  ungroup() %>% 
  left_join(leader, by = c("name" = "Name")) %>% 
  mutate(lap_split = as_datetime(ms(lap_split)),
         name = forcats::fct_reorder(name, Rank)) 

## For plotting trail/road laps ======
times <- as_tibble(seq(0, 60, 12))
colnames(times) <- "hour"
times <- mutate(times, trail = if_else(((hour/12)) %% 2 == 1, "road", "trail"))

ymin <- min(laps$lap_split, na.rm = TRUE)
ymax <- max(laps$lap_split, na.rm = TRUE)

## Plots ======

laps %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(Rank < 11) %>% 
  ggplot() +
    geom_rect(data = times,
              aes(xmin = hour,
                  xmax = hour + 12,
                  ymin = ymin,
                  ymax = ymax,
                  fill = trail),
              alpha = .3) +
    scale_fill_manual(values = c("#888888", "#EEEEEE")) +
    geom_line(aes(x = lap, y = lap_split, colour = name)) +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 70, 2)) +
    scale_y_datetime(date_breaks = "2 min", date_labels = "%H:%M") +
    theme(legend.position = "bottom", legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
    ylab("Lap split") +
    xlab("Lap")
ggsave("./plots/splits.png", width = 10, dpi = 100)

ymin_t = min(laps$total, na.rm = TRUE)
ymax_t = max(laps$total, na.rm = TRUE)

laps %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(Rank < 6) %>% 
  ggplot() +
  geom_rect(data = times,
            aes(xmin = hour,
                xmax = hour + 12,
                ymin = ymin_t,
                ymax = ymax_t,
                fill = trail),
            alpha = .3) +
  scale_fill_manual(values = c("#888888", "#EEEEEE")) +
  geom_line(aes(x = lap, y = total, 
                colour = name)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 70, 2)) +
  scale_y_continuous(breaks = seq(0, 180000, 36000), labels = seq(0, 50, 10)) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  ylab("Race time (hours)") +
  xlab("Lap") 


library(broom)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)

# Used process similar to this article: 
#https://community.fangraphs.com/reverse-engineering-swing-mechanics-from-statcast-data/

#Final graphic here: https://twitter.com/Drew_Haugen/status/1502418762275901451

batterdata <- read_csv("hitter_data_21.csv")

inplay <- batterdata%>%
  filter(type == "X")%>%
  filter(!is.na(launch_speed),
         !is.na(launch_angle),
         !is.na(release_speed))%>%
  group_by(player_name)%>%
  mutate(bip = n(), ev_pctl = ntile(launch_speed, 100), pitch_speed_hp = release_speed*.916)%>%
  ungroup()

top_15 <- inplay%>%
  group_by(player_name)%>%
  arrange(-launch_speed)%>%
  slice(1:15)%>%
  ungroup()%>%
  group_by(player_name, batter)%>%
  filter(bip>=100)%>%
  mutate(ea = .21)%>%
  summarize(launch_speed = mean(launch_speed), ea = mean(ea))

bot_15 <- inplay%>%
  group_by(player_name)%>%
  arrange(launch_speed)%>%
  slice(1:15)%>%
  ungroup()%>%
  group_by(player_name, batter)%>%
  filter(bip>=100)%>%
  mutate(ea = -.1)%>%
  summarize(launch_speed = mean(launch_speed), ea = mean(ea))

bot_plus_top <- rbind(bot_15, top_15)

ea_lm <- bot_plus_top %>%
    group_by(player_name, batter)%>%
    do(fitEa = tidy(lm(ea ~ launch_speed, data = .)))%>%
    unnest(fitEa)%>%
    mutate(term_num = ifelse(term == "launch_speed", 1,0))

intercept <- ea_lm%>%
  filter(term_num == 0)%>%
  select(player_name, batter, estimate)%>%
  rename(intercept = estimate)


launch_speed_est <- ea_lm%>%
  filter(term_num == 1)%>%
  select(player_name, batter, estimate)%>%
  rename(launch_speed_est = estimate)


inplay <- inplay%>%
  left_join(intercept, by = c("player_name", "batter"))%>%
  left_join(launch_speed_est, by = c("player_name", "batter"))%>%
  mutate(ea = intercept + (launch_speed_est * launch_speed))%>%
  filter(!is.na(intercept))%>%
  mutate(hard_hit = ifelse( launch_speed >= 95, 1, 0))%>%
  mutate(est_swing_speed = (launch_speed - ea * pitch_speed_hp)/(1 + ea))%>%
  mutate(est_smash_factor = 1 + (launch_speed - est_swing_speed)/(pitch_speed_hp + est_swing_speed))

swing_speed <- inplay%>%
  group_by(player_name, batter)%>%
  summarize(bat_speed = (mean(est_swing_speed)), hard_hit_perc = mean(hard_hit)*100,
            ea = mean(ea), bip = mean(bip), smash_factor = mean(est_smash_factor),
            wobacon = mean(woba_value), avg_ev = mean(launch_speed),
            xwobacon = mean(estimated_woba_using_speedangle))%>%
  arrange(desc(bat_speed))%>%
  mutate(player_name = stringi::stri_conv(str = player_name, to = "iso-8859-1",
                                 from = "utf-8"))%>%
  ungroup()

lm_mod <- lm(avg_ev ~ bat_speed, data = swing_speed)

swing_speed$residual <- lm_mod$residuals

swing_speed_outliers <- swing_speed%>%
  filter(abs(residual)  >= 2.95)

swing_speed%>%
  ggplot(aes(bat_speed, avg_ev))+
  geom_point(aes(color = ea))+
  scale_color_distiller(palette = "Spectral")+
  geom_smooth(method = "lm")+
  geom_label_repel(data = swing_speed_outliers, aes(bat_speed, avg_ev, label = player_name),
                   min.segment.length = .01)+
  theme_bw()+
  labs(title = "Relationship Between Average EV and Estimated Bat Speed ",
       subtitle = "Hitters with at least 100 BIP in 2021; EA represents Average Collision Efficiency",
       x = "Estimated Bat Speed",
       y = "Average Exit Velocity",
       color = "EA",
       caption = "Data from Baseball Savant; Plot @Drew_Haugen")


#Libraries!
library(tidytuesdayR)
library(tidyverse)
library(waffle)
library(hrbrthemes)
library(futurevisions)
library(patchwork)

#Load Data

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)

plants <- tuesdata$plants
actions <- tuesdata$actions
threats <- tuesdata$threats

#cleaning is fun!

plants_by_year <- plants %>%
  group_by(continent, year_last_seen) %>%
  replace_na(list(year_last_seen = "Unknown")) %>%
  summarise(count = n()) %>%
  mutate(year_last_seen_f = factor(year_last_seen, levels = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020", "Unknown"))) %>%
  mutate(continent_f = factor(continent, levels = c("Africa", "North America", "South America", "Asia", "Oceania", "Europe"))) %>%
  arrange(year_last_seen_f)

threats_by_year <- threats %>%
  filter(threatened == 1) %>%
  mutate(year_last_seen_f = factor(year_last_seen, levels = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"))) %>%
  mutate(threat_type_f = fct_lump_n(as.factor(threat_type), 7)) %>%
  group_by(threat_type_f, year_last_seen_f, .drop = FALSE) %>%
  summarise(count = n()) %>%
  drop_na()

#Plot!
waffle_plot <- ggplot(data = plants_by_year, mapping = aes(fill = year_last_seen_f, values = count)) + 
  geom_waffle(flip = TRUE, color = "#09283C", n_rows = 8, size = .8) + 
  facet_wrap(~continent_f, strip.position = "bottom", nrow = 1) + 
  coord_fixed() +
  theme_minimal() + 
  theme_enhance_waffle() +
  labs(title = "Extinct Plant Species by Continent",
       subtitle = "Each Cell is One Species") + 
  scale_fill_manual(values = c(futurevisions("kepler186"))) +
  guides(fill = guide_legend(title = "Year Last Seen", reverse = TRUE)) +
  theme(panel.background = element_blank(), 
        plot.background = element_rect(fill = "#09283C", color = "#09283C"),
        text = element_text(family = "Roboto Condensed Light", color = "white"), 
        panel.border = element_rect(fill = NA, color = "#09283C"), 
        strip.text = element_text(color = "white", size = 9),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(10, 10, 10, 10),
        panel.spacing.x = unit(.5, "pt"),
        panel.spacing.y = unit(0, "pt"),
        legend.margin = margin(10, 10, 10, 10),
        legend.justification = "left"
        )


time_plot <- ggplot(data = threats_by_year, mapping = aes(x = unclass(year_last_seen_f), y = count, fill = threat_type_f)) +
  geom_area(color = NA) + 
  theme_minimal() +
  scale_fill_manual(values = c(futurevisions("kepler186"))) +
  guides(fill = guide_legend(title = "Type of Threat", 
                             color = "#09283C",
                             override.aes = list(color = "#09283C"),
                             reverse = TRUE)) +
  labs(title = "Threats over Time Globally") +
  scale_x_continuous(
    name = "", breaks = c(1:7), 
    labels = c("Before 1900", "1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020"),
    expand = expansion(mult = .005)) +
  scale_y_continuous(expand = expansion(mult = .01)) +
  ylab("Plants Threatened") +
  theme(panel.grid = element_blank(),
        rect = element_rect(color = NULL),
        plot.background = element_rect(fill = "#09283C", color = "#09283C"),
        text = element_text(family = "Roboto Condensed Light", color = "white"),
        axis.text = element_text(color = "white", size = 8, face = "bold"),
        axis.ticks.x = element_line(color = "white", size = .5),
        axis.ticks.length = unit(6, "pt"),
        plot.title = element_text(size = 15),
        plot.margin = margin(10, 10, 10, 10),
        legend.margin = margin(10, 10, 10, 10),
        legend.justification = "left"
        )


plants_viz <- waffle_plot / time_plot + 
  plot_layout(
    heights = 1.6) + 
  plot_annotation(
    caption = "data: IUCN | viz: @KabelJonathan",
    theme = theme(plot.margin = margin(10, 10, 10, 10),
                plot.background = element_rect("#09283C"),
                text = element_text(family = "Roboto Condensed Light", color = "white")
                )
    ) 

#Export
ggsave("plants34.png", plants_viz, width = 10, height = 10)

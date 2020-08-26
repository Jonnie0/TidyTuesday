

# Libraries ---------------------------------------------------------------


library(tidyverse)
library(tidytuesdayR)
library(schrute)
library(imager)


# Get the Data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2020, week = 12)

# theoffice dataset taken from schrute library


# Clean the Data ----------------------------------------------------------

schrute <- theoffice %>%
  mutate(words = sapply(strsplit(text, ""), length)) %>%
  select(season, character, words) %>%
  group_by(season, character) %>%
  summarise(char_words_per_season = sum(words)) %>%
  mutate(character_f = fct_lump_n(character, 4, w = char_words_per_season)) %>%  #set number of splits per letter/season
  group_by(character_f, .add = TRUE) %>%         #Combines 'other' characters
  summarise(char_words_per_season = sum(char_words_per_season)) %>% 
  mutate(total_words_per_season = sum(char_words_per_season)) %>%
  summarise(character_f,
            char_prop_per_season = char_words_per_season/total_words_per_season) %>%
  group_split(.keep = FALSE)



# Plot the Data -----------------------------------------------------------

img <- load.image("/Users/jonathankabel/Documents/TidyTuesday/The Office Letters/the_office_text.png") #Image of Text, choose lower resolution to make code run faster

gg_img <- as.data.frame(img)

gray_img <- flatten.alpha(img, "white") %>% grayscale()            #Convert to grayscale in order to identify pixel sets
px <- (gray_img < .99) %>%                                         #Identify pixel sets corresponding to letters
  split_connected() 

px_in_order <- lapply(list(px[[2]], px[[1]], px[[3]], px[[7]], px[[4]], px[[5]],    #Letters in order and combine 'i'
                    as.pixset(px[[6]] + px[[10]]), px[[8]], px[[9]]), as.data.frame) 

alphas <- filter(gg_img, cc == 4) %>%
  summarise(x, y, value)

for(i in 1:9) {                                         #Loop to run for each pixel set
  schrute[[i]] <- arrange(schrute[[i]],                 #Rearranges in alphabetical order: makes nicer gradient in plot
                          as.character(character_f))
  prct <- pull(schrute[[i]][2])                         #relevant proportions for season/letter
  c_prct <- cumsum(c(0, paste(prct)))                   #calculate cumulative proportions
  area_sum <- sum(px_in_order[[i]][4])                  # cumulative alpha level for image
  area_cum <- cumsum(px_in_order[[i]][4]) / area_sum    #tells us the cumulative alpha passed at each point
  
  prct_to_color <- function(x) {
    sapply(schrute[[i]][1], "[", findInterval(x, c_prct, all.inside = TRUE)) #'all.inside' so that last position doesn't fall into an 8th interval, which doesn't have a corresponding character in the 'schrute' list (introducing an NA)
    }
  px_in_order[[i]] <- mutate(px_in_order[[i]], 
                             current_char = sapply(area_cum, prct_to_color)) %>%
    left_join(alphas) %>%
    mutate(y = y + 50 * as.integer(as.factor(current_char))
           )
}

full_img <- bind_rows(px_in_order) %>%   #scale alphas to range between 1 and .7 (alpha of depth)
  mutate(value = .7 + .3 * value)


combine_px <- function(a, b) {
  bind_rows(list(a, anti_join(b, a, c("x", "y"))))
}

all_greys <- reduce(lapply(1:60, function (i) {
  mutate(full_img, x = x + i, 
                   y = y - i %/% 2, value = 
                  if (i != 60) value = .7 else (value - .7) / .3 - .3 * value) #scale alpha of final layer
      }
  ), combine_px
)


full_image_merged <- combine_px(full_img, all_greys)%>%
  ggplot(mapping = aes(x, y)) +
    geom_tile(aes(fill = current_char, alpha = value)) +
    scale_y_continuous(trans = scales::reverse_trans()) + 
    scale_alpha_identity() +
    scale_fill_manual(values = c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600")) +
    coord_fixed() + 
  theme(
    plot.background = element_rect(fill = "#FCF7F8"),
    panel.background = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    text = element_text(family = "American Typewriter"),
    legend.text = element_text(size = 32),
    legend.title = element_blank(),
    legend.position = c(.9, .77),
    legend.text.align = 1,
    legend.key = element_rect(size = 1, color = NA),
    legend.key.size = unit (1, "in"),
    plot.title = element_text(size = 56, hjust = 1),
    plot.subtitle = element_text(size = 40, hjust = 1),
    plot.caption = element_text(size = 26),
    plot.margin = margin(10, 20, 10, 20)
    ) +
  labs(title = "Character Speaking Proportion by Season",
       subtitle = "Each Letter is One Season",
       caption = "data: 'schrute' library | viz: @KabelJonathan") +
  guides(fill = guide_legend(title = "Character",
                             label.position = "left",
                             direction = "vertical",
                             color = "#FCF7F8",
                             ncol = 1))


ggsave("/Users/jonathankabel/Documents/TidyTuesday/Visualizations/office_final.png", full_image_merged, width = 28, height = 22, bg = "#FCF7F8")



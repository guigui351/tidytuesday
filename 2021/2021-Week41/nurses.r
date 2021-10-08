
# Load Packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(showtext)
library(ggrepel)
library(forcats)

## Load fonts
font_add_google("Poppins", "Poppins")
font_add_google("Bebas Neue", "Bebas Neue")
font_add_google("Indie Flower", "Indie Flower")

showtext_auto()

# Import data
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

# National US Median salary in 2010
median_salary_year <- nurses %>% 
  select(State, `Annual Salary Median`, Year) %>% 
  filter(Year == 2010) %>% 
  summarize(median_salary_2010 = mean(`Annual Salary Median`, na.rm = TRUE)) 

# Diff between median annual salaries of each state and national US median salary in 2010
# Select only states with a negative difference, or >20k difference
df <- nurses %>% 
  bind_cols(median_salary_year) %>%
  mutate(diff_global_median =  `Annual Salary Median` - median_salary_2010) %>% 
  group_by(State) %>% 
  mutate(last = diff_global_median[which(Year == max(Year))]) %>% 
  filter(last < -1000 | last > 20000) %>% 
  ungroup() %>% 
  mutate(State = fct_reorder(State, last, min))
  
# Value of median salary in 2010
median_salary_2010 <- round(df$median_salary_2010[1],1)

# Coordinates for arrows
arrows <-
  tibble(
    x1 = c(2.0, 13.6, 11.2, 6.4),
    x2 = c(1.5, 13, 13.1, 7.9),
    y1 = c(25000, 45000, -30000, 28000),
    y2 = c(1, 29000, -16500, 19000)
  )

# Label each state on Y axis
df_labs <- 
  tibble(
    State = factor(levels(df$State), levels = levels(df$State)),
    diff_global_median =  seq(1:18)
  )

options(scipen=10000)

# Use same jitter position for geom_point and geom_text_repel
pos <- position_jitter(width = 0.2, seed = 2)

## Create plot
ggplot(df, aes(x = State, y = diff_global_median, color = State)) +
  # Segement from US salry in 2010 and median salary of each state in 2020
  geom_segment(
    aes(x = State, xend = State,
        y = 0, yend = last),
    size = 0.8, alpha = 0.75
  ) +
  # Intercept line, corresponding to US annual salary in 2010
  geom_hline(aes(yintercept = 0), color = "white", size = 0.6) +
  # Draw bigger point for annual salaries of each state in 2020, size depends of number of nurses employed in each state
  geom_point(aes(y = last, size = `Total Employed RN`)) +
  # Draw all annual median salaries of registered nurses from 1998 to 2020
  geom_point(size = 2, alpha = 0.25, position = pos) +
  # Assign color for each state
  scale_color_manual(values = rev(c("firebrick4", "indianred1", "tomato3", "tan4", "wheat4", "tan3", "orange", "tan1",  "lightsalmon", "moccasin", "darkgrey","gray88", "gray95", "forestgreen", "seagreen", "darkolivegreen4", "cornflowerblue", "#50629E")), guide = "none") +
  # Customize axis text
  geom_text(
    data = df_labs, 
    aes(x = diff_global_median, y = 58000, label = State, 
        color = State, color = after_scale(colorspace::darken(color, .2))), 
    hjust = 0, size = 5.5, family = "Bebas Neue", fontface = "italic", lineheight = .75
  )  +
  # Add Year text to some states to see evolution overtime
  geom_text_repel(data = df %>% filter(State %in% c("Oregon", "South Dakota")), aes(label = Year), color = "white", size = 2, position = pos, segment.color = NA, point.size = NA, max.overlaps = 3) +
  coord_flip() +
  # Annotate plot
  annotate(
    "text", x = 2.8, y = 25000, family = "Indie Flower",
    size = 4.5, color = "red",
    label = glue::glue("US average median salary in 2010\n ${round(median_salary_2010, 1)} \n (reference year line)")
  ) +
  annotate(
    "text", x = 14, y = 45000, family = "Indie Flower",
    size = 3.7, color = "white",
    label = glue::glue("Bigger dots represent Median salary in 2020")
  ) +
  annotate(
    "text", x = 10.5, y = -33000, family = "Indie Flower",
    size = 3.7, color = "white",
    label = glue::glue("Each dot represents yearly median salary \n from 1998 to 2020")
  ) +
  annotate(
    "text", x = 5.5, y = 28000, family = "Indie Flower",
    size = 3.7, color = "white",
    label = glue::glue("Bars show the difference between median annual salaries \n of registered nurses of each state in 2020 \n and the national US median salary in 2010")
  ) +
  geom_curve(
    data = arrows, aes(x = x1, xend = x2,
                       y = y1, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "seagreen1", curvature = -0.3#
  ) +
  # Customize scales
  scale_y_continuous(expand = c(0, 0), limits = c(-50000, 75000),
                     breaks = c(-25000, -10000, 0, 10000, 25000, 50000)) +
  scale_size_continuous(name = "Total employed registered \n nurses per state in 2020: ",
                        breaks = c(50000, 150000, 250000, 350000),
                        limits = c(0, 350000),
                        labels = c("50K", "150K", "250K", "350K"),
                        guide = guide_legend(override.aes = list(colour = "seagreen",
                                                                 alpha = 0.5))) +
  # Customize theme
  theme_light() +
  theme(
    line = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black",
                                   color = "black"),
    panel.border = element_rect(color = "transparent"),
    strip.background = element_rect(color = "gray20"),
    axis.text.y = element_blank(),
    plot.margin = margin(10, 25, 10, 25),
    legend.position = c(0.12, 0.85),
    legend.background = element_rect(fill = "black"),
    legend.box.background = element_rect(fill = "black"),
    legend.key = element_rect(fill = "black", color = "black"),
    legend.text = element_text(color = "seagreen", size = 13, face = "bold.italic"),
    legend.title = element_text(color = "seagreen", size = 16, face = "bold.italic"),
    plot.title = element_text(color = "seagreen", size = 30, family = "Poppins", face = "bold"),
    plot.subtitle = element_text(color = "#405D41", size = 13, family = "Poppins", face = "italic"),
    plot.caption = element_text(color = "#405D41", size = 12),
  )  +
  # Plot title, subtitle and captions
  labs(
    title = "Evolution of Median salaries of Registered Nurses in the US since 2010",
    subtitle = "The difference between median annual salaries (from 1998 to 2020) of registered nurses of each state and the national median annual value ($62392,8) for the year 2010  \nThere are still few states within the US with lower median salaries in 2020 than the national median salary from 10 years ago, \n while in some states in 2020 annual median salaries exceed national one from 2010 by at least 20k.",
    caption = "@gabgrall | source: Data.World")

## Save plot and convert PDF to PNG
path <- here::here("2021", "2021-Week40", "plots", "nursesUS")
ggsave(glue::glue("{path}.pdf"), width = 18, height = 12, device = cairo_pdf)
pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"), 
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 450)

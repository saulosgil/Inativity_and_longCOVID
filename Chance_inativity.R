# Packages  ------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Making dataframe -----------------------------------------------------------------------------

data <- data.frame(
  name = c("Presence of symptons",
         "Have 3 symptons",
         "Have 5 or more symptons",
         "Severe muscle/joint pain",
         "Fatigue",
         "Post-Traumatic Stress",
         "Insomnia",
         "Dyspnea"),
  val = c(56, 31, 133, 49, 96, 53, 59, 122))
)

# making lollipop plot

plot2 <-
  data |>
  arrange(val) |>     # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name = factor(name, levels = name)) |>    # This trick update the factor levels
  ggplot(aes(x = name,
             y = val)) +
  geom_segment(aes(xend = name,
                   yend = 0,
                   color = val),
               linetype = "longdash",
               show.legend = FALSE) +
  geom_point(
    size = 4,
    color = "#6b03fc",
    shape = 21,
    stroke = 2
  ) +
  coord_flip() +
  labs(title = "Chance (%) to be inactive in accordance with presence of symptoms",
       caption = "https://www.researchsquare.com/article/rs-1638885/v1") +
  xlab("") +
  ylab("Chance to be inactivity (%)") +
  geom_label(aes(label = val,
                 color = val),
             show.legend = FALSE) +
  theme_bw() +
  theme(title = element_text(face = "bold",
                             size = 15),
        axis.text.y = element_text(face = "bold",
                                   size = 12),
        axis.text.x = element_text(face = "bold",
                                    size = 12)
        ) +
  scale_y_continuous(limits = c(0,150))

plot2

# save plot

ggsave(filename = "plot2.jpeg",
       width = 12,
       height = 7)





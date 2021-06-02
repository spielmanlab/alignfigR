library(tidyverse)

# Use this script to play around with geom_rect and geom_tile
hi <- data.frame(x1 = c(1,1,1,1.5,2,3,3), 
                 x2 = c(1.5,1.5,1.5,2,2.5,3.5,3.5), 
                 y1 = c(1,2,3,2.5,1,1,4), 
                 y2 = c(2,3,5,3.5,5,3.5,5), 
                 f = c("a", "a", "a", "a", "a", "b", "b"), 
                 l = c(1,2,3,4,5,6,7))
ggplot(hi) +
  geom_rect(data = hi, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=f), linetype = 2, color = "dodgerblue", size = 1, alpha = .75, show.legend = FALSE) +
  geom_text(data = hi, aes(x=(x2-x1)/2 + x1, y=(y2-y1)/2 + y1, label=l), size = 5, color = "black") +
  scale_fill_discrete("Legend") +
  theme_void()

library(tidyverse)
library(ggthemes)

cf <- c(6,7,5,5,3,4,2,5,2,2,8,2,6,11)
options(scipen=999)

cr <- tibble(n=-1, pr=-1)
for(i in c(0:10)){
 dpr <- dpois(i, mean(cf))
 cr <- cr %>% add_row(n=i, pr=dpr)
}

cr %>% filter(n!=-1) %>% 
  ggplot(aes(x=n, y=pr)) + geom_col() +
  theme_solarized() +
  labs(y="Probability of X caution periods",
       x="Number of cautions",
       title = "Probability of having X caution periods during the Firestone GP",
       caption = "@thesingleseater | thesingleseater.com") +
  scale_x_continuous(breaks = c(0, 1, 2,3,4,5,6,7,8,9,10))

ggsave("C:/users/drewb/Desktop/caution_plot.png", width = 7, height = 5)

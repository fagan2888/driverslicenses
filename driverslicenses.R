library(readxl)
library(tidycensus)
library(tidyverse)
library(ggrepel)
state_youth_pop <- read_excel("state_youth_pop.xlsx") %>%
  gather(year, pop, -Geography) %>%
  mutate(year = as.numeric(year)) %>%
  rename(STATE = Geography) 
driverslicenses <- read_excel("driverslicenses.xlsx", sheet = "Sheet2") %>%
  gather(age, num_licenses, -STATE, -year) %>%
  mutate(num_licenses = as.numeric(num_licenses)) %>% 
  mutate(STATE = str_replace(STATE, "1/", "")) %>%
  mutate(STATE = str_replace(STATE, "2/", "")) %>%
  mutate(STATE = str_replace(STATE, "3/", "")) %>%
  mutate(STATE = str_trim(STATE)) %>% 
  inner_join(state_youth_pop) %>%
  mutate(share = num_licenses/ pop) %>%
  select(-num_licenses, -pop) %>%
  spread(year, share) %>% mutate(class =  case_when(`2016` - `1999` > 0 ~ "Up",
                                                    STATE == "Total" ~ "Total",
                                                    TRUE ~ "Down"),
                                 class2 =  case_when(`2016` - `1999` > 0 ~ "Up",
                                                               TRUE ~ "Down")) %>%
  filter(STATE != "Dist. of Col." & STATE !="Rhode Island") %>%
  filter(age == 17)  



left_label <- paste(driverslicenses$STATE, round(driverslicenses$`1999`,2),sep=", ")
right_label <- paste(driverslicenses$STATE, round(driverslicenses$`2016`,2),sep=", ")
library(scales)

p <- ggplot(driverslicenses) + 
  geom_segment(aes(x=1, xend=2, y=`1999`, yend=`2016`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  #scale_color_manual(values = c("green"="#00ba38", "red"="#f8766d", "black")) +  # color of lines
  labs(x="", y="", subtitle="Number of 17-year-old Drivers Licenses  per 15-19 year old") +  # Axis labels
 xlim(.5, 2.5)   +
  facet_wrap(~class2) +
  ylim(0.03,(1.06*(max(driverslicenses$`1999`, driverslicenses$`2016`))))  # X and Y axis limits

p <- p + geom_text_repel(label=left_label, y=driverslicenses$`1999`, x=rep(1, NROW(driverslicenses)), hjust=1.1, size=3.5)
p <- p + geom_text_repel(label=right_label, y=driverslicenses$`2016`, x=rep(2, NROW(driverslicenses)), hjust=-0.1, size=3.5)
p <- p + geom_text(label="1999", x=1, y=1.05*(max(driverslicenses$`1999`, driverslicenses$`2016`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="2016", x=2, y=1.05*(max(driverslicenses$`1999`, driverslicenses$`2016`)), hjust=-0.1, size=5)  # title
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))
  

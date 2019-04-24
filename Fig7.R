library(tidyverse)
library(grid)
library(gridExtra)
library(ggpubr)

d1 <- read_csv("data/ProtestFate Net 5 5, TopConcern 9 10, 0.1 X 0.2 protest-mechanisms-table.csv", skip = 6, 
               col_types = cols(
                 .default = col_double(), `protest-mechanisms` = col_character(),`friends-network` = col_character(),
                 `keep-network` = col_logical(), `size-as` = col_character(), `show-links` = col_logical(),
                 `eff-num-topics` = col_double(), `randomSeed?` = col_logical())) %>% 
  select(-`used-random-seed`,-randomSeed,-`randomSeed?`, -`friends-network`, -`size-as`) %>% 
  mutate(`eff-num-topics` = replace(`eff-num-topics`,`eff-num-topics`==9, NA)) %>% 
  rename(social_protesters = `count turtles with [protest-status = \"social\"]`, 
         concern_protesters = `count turtles with [protest-status = \"concern\"]`) %>% 
  mutate(all_protesters = social_protesters + concern_protesters,
         `protest-mechanisms` = factor(`protest-mechanisms`, 
                                       levels = c("cp","sa", "cp-sa", "cp-smc","cp-smc-sa"),
                                       labels = c("concern protest (CP)", "social activation (SA)", "CP and SA",
                                                  "CP and social media concern (SMC)", "Full model CP, SMC, and SA")))
d2 <- read_csv("data/ProtestFate Net 5 5, TopConcern 9 10, X 0.5 0.2 protest-mechanisms-table.csv", 
               skip = 6, 
               col_types = cols(.default = col_double(),`protest-mechanisms` = col_character(),
                                `friends-network` = col_character(),`keep-network` = col_logical(),
                                `size-as` = col_character(),`show-links` = col_logical(),
                                `eff-num-topics` = col_double(),`randomSeed?` = col_logical())) %>% 
  select(-`used-random-seed`,-randomSeed,-`randomSeed?`, -`friends-network`, -`size-as`) %>% 
  mutate(`eff-num-topics` = replace(`eff-num-topics`,`eff-num-topics`==9, NA)) %>% 
  rename(social_protesters = `count turtles with [protest-status = \"social\"]`, 
         concern_protesters = `count turtles with [protest-status = \"concern\"]`) %>% 
  mutate(all_protesters = social_protesters + concern_protesters,
         `protest-mechanisms` = factor(`protest-mechanisms`, 
                                       levels = c("cp","sa", "cp-sa", "cp-smc","cp-smc-sa"),
                                       labels = c("concern protest (CP)", "social activation (SA)", "CP and SA",
                                                  "CP and social media concern (SMC)", "Full model CP, SA, and SMC")))

ini <- 0.1; dis <- 0.2; th <- 0.5; 
g1 <- d1 %>% 
  filter(`initial-concern-level`==ini, `threshold-dispersion`==dis, `threshold-level`<=0.8) %>% 
  ggplot(aes(color = factor(`protest-mechanisms`), y = all_protesters, x = `threshold-level`)) +
  geom_jitter(size = 0.5, width = 0.005, height = 0) + stat_summary(fun.y=mean, geom="line") +
  # stat_function(fun = function(x) 1000*(1 - pnorm(- (1 - x), sd=dis)), 
  # color = "black", linetype="dotted") + 
  ylim(0,1000) + labs(x = "Threshold level", y = "Number of protesters", tag="A") +
  scale_x_continuous(minor_breaks = seq(0,1,0.1), breaks = seq(0,0.8,0.1)) +
  scale_color_manual(values = c("red","blue","purple","gray50","black")) +
  theme_bw() + guides(color=FALSE) + theme()
g2 <- d1 %>% 
  filter(`protest-mechanisms` %in% c("CP and social media concern (SMC)", 
                                     "Full model CP, SMC, and SA"), `threshold-level`<=0.8) %>% 
  filter(`initial-concern-level`==ini, `threshold-dispersion`==dis) %>% 
  ggplot(aes(color = factor(`protest-mechanisms`), y = `eff-num-topics`, x = `threshold-level`)) +
  geom_jitter(size = 0.5, width = 0.005, height = 0) + stat_summary(fun.y=mean, geom="line") +
  labs(x = "Threshold level", y = "Effective number of topics", 
       caption = paste0("Initial concern level ", ini, "\nThreshold dispersion ", dis), tag="B") +
  scale_x_continuous(minor_breaks = seq(0,1,0.1), breaks = seq(0,0.8,0.1)) +
  scale_y_continuous(breaks=1:9) +
  scale_color_manual(values = c("gray50","black")) +
  theme_bw() + theme(legend.position = "bottom") + guides(color=F)
g3 <- d2 %>% 
  filter(`protest-mechanisms` %in% c("CP and social media concern (SMC)", 
                                     "Full model CP, SA, and SMC"), `initial-concern-level`<= 0.5) %>% 
  filter(`threshold-level`==th, `threshold-dispersion`==dis) %>% 
  ggplot(aes(color = factor(`protest-mechanisms`), y = `eff-num-topics`, x = `initial-concern-level`)) +
  geom_jitter(size = 0.5, width = 0.005, height = 0) + stat_summary(fun.y=mean, geom="line") + 
  labs(caption = paste0("Threshold level ", th, "\nThreshold dispersion ", dis), 
       x = "Initial concern level", y = "", tag="C") +
  scale_x_continuous(minor_breaks = seq(0,1,0.1), breaks = seq(0,0.5,0.1)) +
  scale_y_continuous(limits=c(1,9),breaks=1:9) +
  scale_color_manual(values = c("gray50","black")) +
  theme_bw() + theme(legend.position = "bottom", axis.text.y = element_blank()) + guides(color=FALSE)
leg <- legendGrob(labels=c("concern protest (CP)", "social activation (SA)", "CP and SA",
                           "CP and social media concern (SMC)", "Full model CP, SMC, and SA"),
                  pch=20, gp = gpar(col = c("red","blue","purple","gray50","black")))

ggarrange(g1, leg, g2, g3)
ggsave("Fig7.png", scale = 1.5, dpi = 300)
library(readxl)
library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(sjstats)
library(pwr)
library(viridis)
library(tidyr)
library(patchwork)

GL <- read_excel("C:/Users/Documents/GL.xlsx", 
                 col_types = c("text", "text", "text", 
                               "text", "text", "numeric", "numeric", 
                               "numeric"))

head(GL)

GL_mean =  GL %>% group_by(Condition,Participant,Groups) %>%
  summarise(dr = mean(DR, na.rm = TRUE))
GL_mean


#Discharge rates
fittadr <- lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant),
                          data = GL)
fittadr


fitsoldr.emm.s <- emmeans(fittadr, "Condition", "Groups")
pairs(fitsoldr.emm.s, adjust = "bonferroni")

(refgrid <- list (Condition=c("FI","FN"), Groups=c("AT","Control")))
mar_dr_ta <- emmip(fittadr, ~ as.factor(Condition)|as.factor(Groups), at = refgrid, CIs = T, plotit = F)
mar_dr_ta


emm <- emmeans(fittadr, pairwise ~ Condition*Groups)
confint(emm)
conditional_effect <- emmeans(fittadr, ~ Condition*Groups, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fittadr), edf = df.residual(fittadr))



#plot gl DR 
ggplot(data = GL_mean, aes(x = Condition, y = dr)) +
  geom_jitter(width = 0.02, alpha = 1, size = 3, aes(colour = Participant)) +
  scale_color_viridis(discrete = TRUE, option = "G") +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = GL_mean, aes(x = Condition, y = dr, group = Participant),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_dr_ta, aes(x = Condition, y = yvar), 
             size = 4,
             alpha = 0.6,
             position = position_nudge(x = -0.2), 
             shape = 18,
             fill = "black") +
  geom_errorbar(data = mar_dr_ta, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.2), width = 0.0, size = 1) +
  scale_x_discrete(breaks=c("FN", "FI"), labels=c("Feet neutral", "Feet in")) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  labs(y = "Gastrocnemius lateralis \n motor unit discharge rates (pps)", x = "") +
  facet_grid(~Groups) -> plot_gl_dr
plot_gl_dr


p2 <- plot_gl_dr + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                              labels=c("Feet neutral", "Feet in"))

ggsave(file = "GL_mean.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")



#p1 is plot from tracked MU analysis - from another file.

all <- p1 / p2 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all

ggsave(file = "GL_all_mean.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")

GM <- read_excel("C:/Users/Documents/GM.xlsx", 
                 col_types = c("text", "text", "text", 
                               "text", "text", "numeric", "numeric"))
head(GM)

GM_mean =  GM %>% group_by(Condition,Participant,Groups) %>%
  summarise(dr = mean(DR, na.rm = TRUE))
GM_mean


#Discharge rates
fittagm <- lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant),
                data = GM)
fittagm


fitgm.emm.s <- emmeans(fittagm, "Condition", "Groups")
pairs(fitgm.emm.s, adjust = "bonferroni")

(refgridgm <- list (Condition=c("FI","FN"), Groups=c("AT","Control")))
mar_dr_tagm <- emmip(fittagm, ~ as.factor(Condition)|as.factor(Groups), at = refgridgm, CIs = T, plotit = F)
mar_dr_tagm


emm2 <- emmeans(fittagm, pairwise ~ Condition*Groups)
confint(emm2)
conditional_effect2 <- emmeans(fittagm, ~ Condition*Groups, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fittagm), edf = df.residual(fittagm))



#plot gm DR 
ggplot(data = GM_mean, aes(x = Condition, y = dr)) +
  geom_jitter(width = 0.02, alpha = 1, size = 3, aes(colour = Participant)) +
  scale_color_viridis(discrete = TRUE, option = "F") +
  theme_bw(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = GM_mean, aes(x = Condition, y = dr, group = Participant),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_dr_tagm, aes(x = Condition, y = yvar), 
             size = 4,
             alpha = 0.6,
             position = position_nudge(x = -0.2), 
             shape = 18,
             fill = "black") +
  geom_errorbar(data = mar_dr_tagm, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.2), width = 0.0, size = 1) +
  scale_x_discrete(breaks=c("FN", "FI"), labels=c("Feet neutral", "Feet in")) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size = 16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(y = "Gastrocnemius medialis \n motor unit discharge rates (pps)", x = "") +
  facet_grid(~Groups) -> plot_gm_dr
plot_gm_dr


p4 <- plot_gm_dr + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                               labels=c("Feet neutral", "Feet in"))

ggsave(file = "GM_mean.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")

all2 <- p3 / p4 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all2

ggsave(file = "Gm_all_mean.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")

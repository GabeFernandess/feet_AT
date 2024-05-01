
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
library(ggbeeswarm)


#____data frame - mean data - GL


GL <- read_excel("C:/Users/gabep/OneDrive/Study 3B/data study 3/MUDR_RMS.xlsx", 
                 sheet = "GLDR")

head(GL)

GL_mean =  GL %>% group_by(condition,participant,group) %>%
  summarise(dr = mean(dr, na.rm = TRUE))
GL_mean


#Discharge rates

fitdr_mean <- lmer(dr ~ group * as.factor(condition) + (1 | participant), data = GL_mean)



anova(fitdr_mean) %>%
  knitr::kable()

fitdr.emm.s <- emmeans(fitdr_mean, "condition", "group")
pairs(fitdr.emm.s, adjust = "bonferroni")



(refgrid <- list (condition=c("FI","FN"), group=c("Achilles tendinopathy","Control")))
mar_drm_gl <- emmip(fitdr_mean, ~ as.factor(condition)|as.factor(group), at = refgrid, CIs = T, plotit = F)
mar_drm_gl


emm <- emmeans(fitdr_mean, pairwise ~ condition*group)
confint(emm)

anovafit_fit_dr <- anova(fitdr_mean)
effectsize::omega_squared(anovafit_fit_dr)
anovafit_fit_dr


#plot gl DR mean
ggplot(data = GL_mean, aes(x = condition, y = dr)) +
  geom_quasirandom(width = 0.04, size = 6, shape=18,aes(colour = participant)) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=1)+
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
geom_line(data = GL_mean, aes(x = condition, y = dr, group = participant),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_dr_gl, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_dr_gl, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +
  
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())+  
  labs(y = "Gastrocnemius lateralis \n mean motor unit discharge rates (pps)", x = "") +
  facet_grid(~group) -> plot_gl_dr_mean
plot_gl_dr_mean


p2dr <- plot_gl_dr_mean + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                     labels=c("Feet neutral", "Feet in"))

p2dr

ggsave(file = "GL_dr_mean.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")

#------------------------------------GM

GM <- read_excel("C:/Users/gabep/OneDrive/Study 3B/data study 3/MUDR_RMS.xlsx", 
                 sheet = "GMDR")
head(GM)

GM_mean =  GM %>% group_by(condition,participant,group) %>%
  summarise(dr = mean(dr, na.rm = TRUE))
GM_mean


#Discharge rates
fit_dr_gm <- lmer(dr ~ group * as.factor(condition) + (1 | participant),
                  data = GM_mean)
fit_dr_gm


anova(fit_dr_gm) %>%
  knitr::kable()

fitgm.emm.s <- emmeans(fit_dr_gm, "condition", "group")
pairs(fitgm.emm.s, adjust = "bonferroni")


(refgridgm <- list (condition=c("FI","FN"), group=c("Achilles tendinopathy","Control")))
mar_drm_gm <- emmip(fit_dr_gm, ~ as.factor(condition)|as.factor(group), at = refgridgm, CIs = T, plotit = F)
mar_drm_gm


emm2 <- emmeans(fit_dr_gm, pairwise ~ condition*group)
confint(emm2)


anovafit_fit_dr <- anova(fit_dr_gm)
effectsize::omega_squared(anovafit_fit_dr)
anovafit_fit_dr


#plot gm DR 
ggplot(data = GM_mean, aes(x = condition, y = dr)) +
  geom_jitter(width = 0.05, size = 6, alpha = 0.8, shape=18,aes(colour=participant)) +
  scale_color_viridis(discrete = TRUE, option = "F") +
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = GM_mean, aes(x = condition, y = dr, group = participant),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_dr_gm, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_dr_gm, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())+ 
  labs(y = "Gastrocnemius medialis \n mean motor unit discharge rates (pps)", x = "") +
  facet_grid(~group) -> plot_gm_dr
plot_gm_dr


p4dr <- plot_gm_dr + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                     labels=c("Feet neutral", "Feet in"))
p4dr

ggsave(file = "GM_mean.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")


#___full data frame

library(naniar)
library(visdat)
library(janitor)
library(cowplot)
library(psycho)
library(sjstats)
library(pwr)
library(viridis)
library(tidyr)
library(car)
library(Rmisc)

#_____GL_dr____

head(GL)

#Discharge rates


fitdr1 <- lmer(dr ~ group * as.factor(condition)* as.factor(contraction) + (1 | participant/mu_id), data = GL)
fitdr2 <- lmer(dr ~ group * as.factor(condition) + (1 | participant/mu_id), data = GL)


#Check for best model
anova(fitdr1,fitdr2)

#fit_dr best model
fitdr <- fitdr2


anova(fitdr) %>%
  knitr::kable()

fitdr.emm.s <- emmeans(fitdr, "condition", "group")
pairs(fitdr.emm.s, adjust = "bonferroni")

fitdr.emm.s <- emmeans(fitdr, "group", "condition")
pairs(fitdr.emm.s, adjust = "bonferroni")


(refgrid <- list (group=c("Achilles tendinopathy","Control")))
mar_dr_gl <- emmip(fitdr, ~ as.factor(group)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_dr_gl



emm <- emmeans(fitdr, pairwise ~ condition*group)
confint(emm)

anovafit_fit_dr <- anova(fitdr)
effectsize::omega_squared(anovafit_fit_dr)
anovafit_fit_dr



#plot gl DR 
ggplot(data = GL, aes(x = condition, y = dr)) +
  geom_quasirandom(width = 0.15, size = 5, shape=18,aes(colour = participant)) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=1)+
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
 # geom_line(data = GL, aes(x = condition, y = dr, group = participant),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_dr_gl, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_dr_gl, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +
  
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  labs(y = "Gastrocnemius lateralis \n motor unit discharge rates (pps)", x = "") +
  facet_grid(~group) -> plot_gl_dr
plot_gl_dr


p1 <- plot_gl_dr + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                     labels=c("Feet neutral", "Feet in"))

p1

ggsave(file = "GL_dr.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")


all <- p1 / p2 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all

ggsave(file = "GL_all.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")


#_______GM_dr_______


fit_dr_gm <- lmer(dr ~ group * as.factor(condition) + (1 | participant/mu_id), data = GM)
fit_dr_gm


anova(fit_dr_gm) %>%
  knitr::kable()

fitgm.emm.s <- emmeans(fit_dr_gm,"group", "condition")
pairs(fitgm.emm.s, adjust = "bonferroni")

fitgm.emm.s <- emmeans(fit_dr_gm,"condition", "group")
pairs(fitgm.emm.s, adjust = "bonferroni")

(refgridgm <- list (condition=c("FI","FN"), group=c("Achilles tendinopathy","Control")))
mar_dr_gm <- emmip(fit_dr_gm, ~ as.factor(condition)|as.factor(group), at = refgridgm, CIs = T, plotit = F)
mar_dr_gm


emm2 <- emmeans(fit_dr_gm, pairwise ~ condition*group)
confint(emm2)


anovafit_fit_dr <- anova(fit_dr_gm)
effectsize::omega_squared(anovafit_fit_dr)
anovafit_fit_dr



#plot gm DR 
ggplot(data = GM, aes(x = condition, y = dr)) +
  geom_quasirandom(width = 0.18, size = 5, shape=18,aes(colour = participant)) +
  scale_color_viridis(discrete = TRUE, option = "F") +
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
 # geom_line(data = GM_mean, aes(x = condition, y = dr, group = participant),color='grey50', size = .75, alpha = .25) +
  geom_point(data = mar_dr_gm, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.4), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_dr_gm, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.4), width = 0.0, size = 0.6) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())+ 
  labs(y = "Gastrocnemius medialis \n motor unit discharge rates (pps)", x = "") +
  facet_grid(~group) -> plot_gm_dr
plot_gm_dr


p3 <- plot_gm_dr + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                     labels=c("Feet neutral", "Feet in"))
p3



all2 <- p2 / p3 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all2

ggsave(file = "Gm_all_mean.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")




all_mean <- p2 / p4 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all_mean

ggsave(file = "dr_mean.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")


#mean RMS analysis

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
library(psych)
library(afex)
library(ggbeeswarm)


#RMS mean channels analysis ----

#GL mean----------

GLRMS <- read_excel("C:/Users/gabep/OneDrive/Study 3B/data study 3/MUDR_RMS2.xlsx", 
                    sheet = "GLRMS")

head(GLRMS)


GL_mean =  GLRMS %>% group_by(participant, group, condition) %>%
  summarise(rms = mean(norm_RMS, na.rm = TRUE))


#Linear mixed models GL

fit_RMS_1 <- lmer(rms ~ as.factor(group)*as.factor(condition) + (1 | participant), data = GL_mean)


fit_RMS <- fit_RMS_1

summary(fit_RMS)

anova(fit_RMS) %>%
  knitr::kable()


fitrms.emm.s <- emmeans(fit_RMS, "condition", "group")
pairs(fitrms.emm.s, adjust = "bonferroni")


fitrms.emm.s <- emmeans(fit_RMS, "condition")
pairs(fitrms.emm.s, adjust = "bonferroni")


#confint_omegasquared
anovafit_fit_RMS <- anova(fit_RMS)
effectsize::omega_squared(anovafit_fit_RMS)
anovafit_fit_RMS



#Refgrid
(refgrid <- list (group=c("Achilles tendinopathy","Control")))
mar_rms <- emmip(fit_RMS, ~ as.factor(group)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_rms


#Mean difference GL mean ------
#norm_RMS
emm <- emmeans(fit_RMS, pairwise ~ condition)
confint(emm)
summary(emm)




#Graph for GL Mean------------

#plot gl RMS
ggplot(data = GL_mean, aes(x = condition, y = rms)) +
  geom_jitter(width = 0.05, size = 6, alpha = 0.8, shape=18,aes(colour=participant)) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=1)+
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = GL_mean, aes(x = condition, y = rms, group = participant),color='grey50', size = .75, alpha = .30) +
  geom_point(data = mar_rms1, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_rms1, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +

  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())+ 
  labs(y = "Gastrocnemius lateralis RMS (%)", x = "") +
  facet_grid(~ group) -> plot_RMS2

p1m <- plot_RMS2+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                    labels=c("Feet neutral", "Feet in"))


p1 <- p1m + ylim(0, 70)

p1

ggsave(file = "GL_RMS1.tiff", units="in", width = 12, height =11, dpi=300, compression = "lzw")



#GM mean -----------------  

GMRMS <- read_excel("C:/Users/gabep/OneDrive/Study 3B/data study 3/MUDR_RMS2.xlsx", 
                    sheet = "GMRMS")

head(GMRMS)


GM_mean =  GMRMS %>% group_by(condition, participant,group) %>%
  summarise(rms = mean(norm_RMS, na.rm = TRUE))
GM_mean


#Linear mixed models GM

fit_RMS_1 <- lmer(rms ~ as.factor(group)*as.factor(condition) + (1 | participant), data = GM_mean)


fit_RMS2 <- fit_RMS_1

summary(fit_RMS2)

anova(fit_RMS2) %>%
  knitr::kable()

#confint
anovafit_fit_RMS <- anova(fit_RMS2)
effectsize::omega_squared(anovafit_fit_RMS)
anovafit_fit_RMS

fitrms.emm.s <- emmeans(fit_RMS2, "condition", "group")
pairs(fitrms.emm.s, adjust = "bonferroni")

#Mean difference GM mean ------
#norm_RMS
emm <- emmeans(fit_RMS2, pairwise ~ condition*group)
confint(emm)
summary(emm)


#Refgrid
(refgrid <- list (group=c("Achilles tendinopathy","Control")))
mar_rms2 <- emmip(fit_RMS2, ~ as.factor(group)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_rms2



fitrms.emm.s1 <- emmeans(fit_RMS2, "condition", "group")
pairs(fitrms.emm.s1, adjust = "bonferroni")



#Graph for GM Mean-------- 

#plot GM RMS
ggplot(data = GM_mean, aes(x = condition, y = rms)) +
  geom_jitter(width = 0.05, size = 6, alpha = 0.8, shape=18,aes(colour=participant)) +
  scale_color_viridis(discrete = TRUE, option = "F", direction=1)+
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
  geom_line(data = GM_mean, aes(x = condition, y = rms, group = participant),color='grey50', size = .75, alpha = .30) +
  geom_point(data = mar_rms3, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_rms3, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank())+ 
  labs(y = "Gastrocnemius medialis RMS (%)", x = "") +
  facet_grid(~ group) -> plot_RMS2

p2m <- plot_RMS2+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                             labels=c("Feet neutral", "Feet in"))
p2 <- p2m+ ylim(0, 60)


p2 


ggsave(file = "GM_RMS.tiff", units="in", width = 10, height = 11, dpi = 300, compression = "lzw")



RMS <- p1 / p2 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

RMS
ggsave(file = "RMS.tiff", units="in", width = 8.5, height = 10, dpi = 300, compression = "lzw")

ggsave(file = "RMS_l.tiff", units="in", width = 10, height = 13, dpi = 300, compression = "lzw")


#all channels - RMS analysis -----

library(readxl)
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(lmerTest)
library(lme4)
library(emmeans)
library(janitor)
library(cowplot)
library(emmeans)
library(lme4)
library(lmerTest)
library(psycho)
library(sjstats)
library(pwr)
library(viridis)
library(tidyr)
library(car)
library(Rmisc)
library(ggbeeswarm)
library(ggsignif)
library(patchwork)

#GL RMS ----

GLRMS <- read_excel("C:/Users/gabep/OneDrive/Study 3B/data study 3/MUDR_RMS2.xlsx", 
                             sheet = "GLRMS")

head(GLRMS)




#Linear mixed models GL

fit_RMS_1 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + (1 | participant), data = GLRMS)
fit_RMS_2 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + as.factor(contraction) + (1 | participant), data = GLRMS)
fit_RMS_3 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + as.factor(contraction) + (1 + contraction | participant), data = GLRMS)
fit_RMS_4 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + (1 + contraction | participant), data = GLRMS)

#fit_RMS_4 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + (1 + contraction | participant), data = GLRMS1)

anova(fit_RMS_1,fit_RMS_2, fit_RMS_3, fit_RMS_4)


fit_RMS <- fit_RMS_4

summary(fit_RMS)

 anova(fit_RMS) %>%
  knitr::kable()

#confint
anovafit_fit_RMS <- anova(fit_RMS)
effectsize::omega_squared(anovafit_fit_RMS)
anovafit_fit_RMS

emmeans(fit_RMS, "group", "condition", data=GLRMS)


GL_rms_emmeans = emmeans(fit_RMS, specs = pairwise ~ group*condition, adjust = "bonferroni")
GL_rms_emmeans

GL_rms_emmeans2 = emmeans(fit_RMS, specs = pairwise ~ condition, adjust = "bonferroni")
GL_rms_emmeans2



#Refgrid
(refgrid <- list (group=c("Achilles tendinopathy","Control")))
mar_rms1 <- emmip(fit_RMS, ~ as.factor(group)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_rms1


#GL Mean difference  ----
#norm_RMS
emm <- emmeans(fit_RMS, pairwise ~ condition)
confint(emm)
summary(emm)

conditional_effect <- emmeans(fit_RMS, ~ condition, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fit_RMS), edf = df.residual(fit_RMS))


#plot GLRMS ----

  ggplot(data= GLRMS, aes(x = condition, y = norm_RMS)) +
  geom_jitter(width = 0.08, size = 4, alpha = 0.5, shape=18,aes(colour=participant)) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=1)+
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
  geom_point(data = mar_rms, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_rms, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  labs(y = "Gastrocnemius lateralis \n normalised RMS", x = "") + facet_grid(~ group) -> plot_RMS2

p1 <- plot_RMS2+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                            labels=c("Feet neutral", "Feet in"))
p1



# GM RMS ----


GMRMS <- read_excel("C:/Users/gabep/OneDrive/Study 3B/data study 3/MUDR_RMS2.xlsx", 
                    sheet = "GMRMS")

head(GMRMS)


#Linear mixed models GL

fit_RMS_1 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + (1 | participant), data = GMRMS)
fit_RMS_2 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + as.factor(contraction) + (1 | participant), data = GMRMS)
fit_RMS_3 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + as.factor(contraction) + (1 + contraction | participant), data = GMRMS)
fit_RMS_4 <- lmer(norm_RMS ~ as.factor(group)*as.factor(condition) + (1 + contraction | participant), data = GMRMS)


anova(fit_RMS_1,fit_RMS_2, fit_RMS_3, fit_RMS_4)


fit_RMS <- fit_RMS_4

summary(fit_RMS)

anova(fit_RMS) %>%
  knitr::kable()

#confint
anovafit_fit_RMS <- anova(fit_RMS)
effectsize::omega_squared(anovafit_fit_RMS)
anovafit_fit_RMS


emmeans(fit_RMS, "group", "condition", data=GMRMS)

GM_rms_emmeans = emmeans(fit_RMS, specs = pairwise ~ group*condition, adjust = "bonferroni")
GM_rms_emmeans



#Refgrid
(refgrid <- list (group=c("Achilles tendinopathy","Control")))
mar_rms3 <- emmip(fit_RMS, ~ as.factor(group)|as.factor(condition), at = refgrid, CIs = T, plotit = F)
mar_rms3


#GM Mean difference-----
#norm_RMS
emm <- emmeans(fit_RMS, pairwise ~ group*condition)
confint(emm)
summary(emm)

conditional_effect <- emmeans(fit_RMS, ~ group*condition, adjust = "bonferroni")
eff_size(conditional_effect, sigma = sigma(fit_RMS), edf = df.residual(fit_RMS))



#plot GMMRS -----
ggplot(data= GMRMS, aes(x = condition, y = norm_RMS)) +
  geom_jitter(width = 0.08, size = 4, alpha = 0.5, shape=18,aes(colour=participant)) +
  scale_color_viridis(discrete = TRUE, option = "F", direction=1)+
  theme_light(base_size = 14) +
  guides(fill = F, color = F) +
  geom_point(data = mar_rms, aes(x = condition, y = yvar), 
             size = 2.5,
             alpha = 0.5,
             position = position_nudge(x = -0.3), 
             shape = 15,
             fill = "black") + 
  geom_errorbar(data = mar_rms, aes(ymin = LCL, ymax = UCL, y = yvar),
                position = position_nudge(x = -0.3), width = 0.0, size = 0.6) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  labs(y = "Gastrocnemius medialis \n normalised RMS", x = "") + 
  facet_grid(~ group) -> plot_RMS2

p2 <- plot_RMS2+ theme(legend.position = "none") + scale_x_discrete(limits=c("FN", "FI"),labels=c("Feet neutral", "Feet in"))
p2



 gl <- p1 / p1m +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

gl 
ggsave(file = "GL_RMS.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")


gm <- p2 / p2m +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

gm
ggsave(file = "GM_RMS.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")

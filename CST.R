
  #title: "Cumulative spike train analysis - CST
  
library(readxl)
library(naniar)
library(visdat)
library(dplyr)
library(tidyverse)
library(lmerTest)
library(emmeans)
library(janitor)
library(cowplot)
library(emmeans)
library(lme4)
library(psycho)
library(sjstats)
library(pwr)
library(viridis)
library(tidyr)
library(car)
library(Rmisc)
library(ggbeeswarm)
library(ggplot2)



#-dataset for CST_GL
  
  
cst_gl <- read_excel("C:/Users/Documents/CST - GL.xlsx")
head(cst_gl)


#Linear mixed models GL_CST

 
lmer_cst_gl = lmer(mean_by_MU ~ as.factor(group) * as.factor(condition) + (1 | participant),
               data = cst_gl)

qqPlot(residuals(lmer_cst_gl))

anova(lmer_cst_gl) %>%
  knitr::kable()


lmer_cst_gl_no_out <- lmer(mean_by_MU ~ as.factor(group) * as.factor(condition) + (1 | participant),
                           data = cst_gl %>% slice (-60,-37))

qqPlot(residuals(lmer_cst_gl_no_out))

cst_gl_out = cst_gl %>% slice (-60,-37)

anova(lmer_cst_gl_no_out) %>%
  knitr::kable()



cst_GL_emmeans = emmeans(lmer_cst_gl, specs = pairwise ~ as.factor(group, condition))
cst_GL_emmeans

cst_GL_sum <- summarySE(cst_gl, measurevar="mean_by_MU", groupvars=c("group", "condition"))
cst_GL_sum

cst_GL_sum_out <- summarySE(cst_gl_out, measurevar="mean_by_MU", groupvars=c("group", "condition"))
cst_GL_sum_out




#Graph for GL CST/MU _ without outlier for visualization

cst_GL <- cst_gl_out %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(condition),
             y=mean_by_MU))+
  geom_quasirandom(aes(color=participant), size=3, width= 0.15, dodge.width = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~group)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 4.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=cst_GL_sum_out, aes(ymin=mean_by_MU-ci, ymax = mean_by_MU+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= "Gastrocnemius latearlis\n normalised neural drive")

GL_g_CST <- cst_GL+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                    labels=c("Feet neutral", "Feet in"))

#-dataset for CST _GM


cst_gm <- read_excel("C:/Users/Documents/CST - GM.xlsx")
head(cst_gm)


#Linear mixed models GM_CST


lmer_cst_gm = lmer(mean_by_MU ~ as.factor(group) * as.factor(condition) + (1 | participant),
                   data = cst_gm)

qqPlot(residuals(lmer_cst_gm))

anova(lmer_cst_gm) %>%
  knitr::kable()


posthocgm_cst <- emmeans(lmer_cst_gm , "condition", data=cst_gm)
pairs(posthocgm_cst, adjust="bonferroni")

cst_GM_emmeans = emmeans(lmer_cst_gm, specs = pairwise ~ as.factor(condition))
cst_GM_emmeans

cst_GM_sum <- summarySE(cst_gm, measurevar="mean_by_MU", groupvars=c("group", "condition"))
cst_GM_sum



#testing by removing outliers# - no change in output without outliers

lmer_cst_gm_no_out <- lmer(mean_by_MU ~ as.factor(group) * as.factor(condition) + (1 | participant),
                           data = cst_gm %>% slice (-38, -49))

cst_gm_out = cst_gm %>% slice (-47, -13)

anova(lmer_cst_gm_no_out) %>%
  knitr::kable()

posthocgm_cst_out <- emmeans(lmer_cst_gm_no_out , "condition", data=cst_gm_out)
pairs(posthocgm_cst_out, adjust="bonferroni")

cst_GM_emmeans_out = emmeans(lmer_cst_gm_no_out, specs = pairwise ~ as.factor(group, condition))
cst_GM_emmeans_out

cst_GM_sum_out <- summarySE(cst_gm_out, measurevar="mean_by_MU", groupvars=c("group", "condition"))
cst_GM_sum_out


#Graph for GL CST/MU _ without outlier for visualization

cst_GM <- cst_gm %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(condition),
             y=mean_by_MU))+
  geom_quasirandom(aes(color=participant), size=3, width= 0.15, dodge.width = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "F", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~group)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 4.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=cst_GM_sum, aes(ymin=mean_by_MU-ci, ymax = mean_by_MU+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= " Gastrocnemius medialis\n normalised neural drive")

GM_g_CST <- cst_GM + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                   labels=c("Feet neutral", "Feet in"))

#Final graphs GL & GM



pcst1 <- GL_g_CST 

ggsave(file = "GL_cst.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")


pcst2 <-GM_g_CST

ggsave(file = "GM_cst.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")

all_cst <- pcst1 / pcst2 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all_cst
ggsave(file = "all_cst.tiff", units="in", width = 9, height = 10, dpi = 300, compression = "lzw")


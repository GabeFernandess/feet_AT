#Tracked analysis of MU discharge rates and Cov for both gastrocnemii

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


#file

GL <- read_excel("C:/Users/Document/GL.xlsx")

head(GL)


#Linear mixed models GL


lmer_GL = lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant/MU_id),
               data = GL)

qqPlot(residuals(lmer_GL))

anova(lmer_GL) %>%
  knitr::kable()

posthocgl <- emmeans(lmer_GL, "Condition", data=GL)
pairs(posthocgl, adjust="bonferroni")

posthocgl


lmer_GL_no_out <- lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant/MU_id),
                       data = GL %>% slice (-75, -83, -119, -120))

anova(lmer_GL_no_out) %>%
  knitr::kable()

GL_out =GL %>% slice (-75, -83, -119, -120)

GL_emmeans = emmeans(lmer_GL, specs = pairwise ~ as.factor(Groups, Condition))
GL_emmeans

GL_emmeans_out = emmeans(lmer_GL_no_out, specs = pairwise ~ as.factor(Groups, Condition))
GL_emmeans_out

GL_sum <- summarySE(GL, measurevar="DR", groupvars=c("Groups", "Condition"))
GL_sum



#Graph for GL Mean DR

GL_DR <- GL %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Condition),
             y=DR))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Participant)) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.3), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=GL_sum, aes(ymin=DR-ci, ymax = DR+ci),
                position = position_nudge(x = -0.3), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 20),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= " Motor unit discharge rates (pps) ")

GL_DR+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                         labels=c("Feet neutral", "Feet in"))




#Tracked MU analysis 

#data set for GM-
  
 
GM <- read_excel("C:/Users/Documentd/GM.xlsx")
head(GM)

#Linear mixed models - GM

#Testing the influence of outliers:  with outliers = interaction Groups:as.factor(Condition), but not in relevant interaction
#Removing outliers diff between conditions, no diff between groups

 
lmer_GM = lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant/MU_id),
               data = GM)


qqPlot(residuals(lmer_GM))

anova(lmer_GM) %>%
  knitr::kable()

posthocgm <- emmeans(lmer_GL, "Condition", data=GM)
pairs(posthocgm, adjust="bonferroni")

posthocgm


lmer_GM_no_out <- lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant/MU_id),
                       data = GM %>% slice (-404, -397, -460, -443))

anova(lmer_GL_no_out) %>%
  knitr::kable()

GM_out =GM %>% slice (-404, -397, -460, -443) 

GM_emmeans = emmeans(lmer_GM, specs = pairwise ~ as.factor(Condition, Groups))
GM_emmeans


GM_emmeans_no_out = emmeans(lmer_GL_no_out, specs = pairwise ~ as.factor(Condition))
GM_emmeans_no_out

posthocgm_out <- emmeans(lmer_GL, "Condition", data=GM)
pairs(posthocgm_out, adjust="bonferroni")

posthocgm

GM_sum <- summarySE(GM, measurevar="DR", groupvars=c("Groups", "Condition"))
GM_sum

GM_sum_no_out <- summarySE(GM_out, measurevar="DR", groupvars=c("Groups", "Condition"))
GM_sum_no_out


#Graph for GM Mean DR


GM_DR <- GM %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Condition),
             y=DR))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Participant)) +
  scale_color_viridis(discrete = TRUE, option = "F", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.3), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=GM_sum, aes(ymin=DR-ci, ymax = DR+ci),
                position = position_nudge(x = -0.3), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= "Gastrocnemius medialis \n motor unit discharge rates (pps)")

GM_DR+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                           labels=c("Feet neutral", "Feet in"))

#Final graphs GL & GM

p1 <- GL_DR+ theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                         labels=c("Feet neutral", "Feet in")) 

ggsave(file = "GL.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")


p3 <- GM_DR + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                                labels=c("Feet neutral", "Feet in"))

ggsave(file = "GM.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")




#Coefficient of Variation analysis for GL tested for outliers - no diff without outliers

lmer_GL_cv = lmer(CV ~ Groups * as.factor(Condition) + (1 | Participant/MU_id),
                  data = GL)


qqPlot(residuals(lmer_GL_cv))

anova(lmer_GL_cv ) %>%
  knitr::kable()


lmer_GL_cv_no_out <- lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant/MU_id),
                          data = GL %>% slice (-83, -168))

anova(lmer_GL_cv_no_out) %>%
  knitr::kable()


posthocglcv_out <- emmeans(lmer_GL_cv_no_out , "Condition", data=GL_out)
pairs(posthocglcv_out, adjust="bonferroni")

posthocglcv_out


GLcv_emmeans = emmeans(lmer_GL_cv , specs = pairwise ~ Groups:as.factor(Condition, Groups))
GLcv_emmeans

GLcv_emmeans_out = emmeans(lmer_GL_cv_no_out , specs = pairwise ~ Groups:as.factor(Condition, Groups))
GLcv_emmeans_out

GL_out =GL %>% slice (-83, -168) 

GLcv_sum <- summarySE(GL, measurevar="CV", groupvars=c("Groups", "Condition"))
GLcv_sum

GLcv_out_sum <- summarySE(GL_out, measurevar="CV", groupvars=c("Groups", "Condition"))
GLcv_out_sum


#Coefficient of Variation (CoV) Graph for GL


GL_CV <- GL %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Condition),
             y=CV))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Participant)) +
  scale_color_viridis(discrete = TRUE, option = "G", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=GLcv_sum, aes(ymin=CV-ci, ymax = CV+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= " Cov motor unit discharge rates (%)")

GL_CV + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                          labels=c("Feet neutral", "Feet inwards"))



#Coefficient of Variation (CoV) analysis for GM

 
lmer_GM_cv = lmer(CV ~ as.factor(Groups)* as.factor(Condition) + (1 | Participant/MU_id),
                  data = GM)


qqPlot(residuals(lmer_GM_cv))


#outlier row 154 - (136 and 154 row) and 443 (443 and 460 ) removed from analysis to test influence of outliers on results 
#No influence of outliers .

lmer_GM_cv = lmer(CV ~ as.factor(Groups)* as.factor(Condition) + (1 | Participant/MU_id),
                  data = GM)

lmer_GM_cv_no_out <- lmer(CV ~ as.factor(Groups) * as.factor(Condition) + 
                            (1 | Participant/MU_id), data=GM %>% slice (-136, -154, -443, -460))

gm_out =  data=GM %>% slice (-136, -154, -443, -460)


anova(lmer_GM_cv ) %>%
  knitr::kable()

anova(lmer_GM_cv_no_out ) %>%
  knitr::kable()

GMcv_emmeans = emmeans(lmer_GM_cv , specs = pairwise ~ as.factor(Condition))

GMcv_emmeans


posthocgmcv <- emmeans(lmer_GM_cv , "Condition", data=GM)
pairs(posthocgmcv, adjust="bonferroni")

GMcv_sum <- summarySE(GM, measurevar="CV", groupvars=c("Groups", "Condition"))
GMcv_sum 

GMcv_sum_out <- summarySE(gm_out, measurevar="CV", groupvars=c("Groups", "Condition"))
GMcv_sum_out
 

#Graph for GM CoV wihtout outliers - for better visuals.

GM_CV <- gm_out %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Condition),
             y=CV))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Participant)) +
  scale_color_viridis(discrete = TRUE, option = "F", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 3.5,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=GMcv_sum, aes(ymin=CV-ci, ymax = CV+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= " Cov motor unit discharge rates (%) ")

GM_CV + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                          labels=c("Feet neutral", "Feet inwards"))

#Final graphs GL & GM


GL_cv2 <- GL_CV + theme(axis.title = element_text(size = 12))+theme(legend.position = "none")+
  theme(plot.caption = element_text(size = 10))+
  theme(plot.tag = element_text(size = 11))+ 
  theme(axis.text.x = element_text(vjust=1))+ scale_x_discrete(limits=c("FN", "FI")) + theme(axis.title.x=element_blank())

GL_cv2

ggsave(file = "GL_cv.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")

GM_g2 <- GM_CV +  theme(axis.title = element_text(size = 12))+ theme(legend.position = "none")+
  theme(plot.caption = element_text(size = 10))+
  theme(plot.tag = element_text(size = 11))+ 
  theme(axis.text.x = element_text(vjust=1))+ scale_x_discrete(limits=c("FN", "FI"))+ theme(axis.title.x=element_blank())

GM_g2
ggsave(file = "GM_cv.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")





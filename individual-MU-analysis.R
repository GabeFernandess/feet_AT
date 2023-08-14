
  #title: "Neuromodulation to GL and GM using modified feet position in runners with AT"

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


#ind. MU analysis 

#-dataset for GL-
  
  
GL_ind <- read_excel("C:/Users/Documents/GL - individual.xlsx")

head(GL_ind)


#Linear mixed models GL

 
lmer_GL = lmer(DR ~ as.factor(Groups) * as.factor(Condition) + (1 | Participant),
               data = GL_ind)

qqPlot(residuals(lmer_GL))

anova(lmer_GL) %>%
  knitr::kable()


GL_emmeans = emmeans(lmer_GL, specs = pairwise ~ as.factor(Groups, Condition))
GL_emmeans

GL_sum <- summarySE(GL_ind, measurevar="DR", groupvars=c("Groups", "Condition"))
GL_sum


#Graph for GL Mean DR

GL_DR_ind <- GL_ind %>%
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
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=GL_sum, aes(ymin=DR-ci, ymax = DR+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= " Gastrocnemius lateralis \n motor unit discharge rates (pps)")



GL_DR_ind + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                              labels=c("Feet neutral", "Feet in")) 

#Individual MU analysis 

#data set for GM-
  

GM_ind <- read_excel("C:/Users/Documents/ GM - individual.xlsx")
head(GM_ind)



GM_mean <- GM_ind %>% group_by(Participant) %>% summarise(DR = mean(DR),
.groups = 'drop')
                                                              
#Linear mixed models - GM

lmer_Gm = lmer(DR ~ Groups * as.factor(Condition) + (1 | Participant),
               data = GM_ind)


qqPlot(residuals(lmer_Gm))


lmer_GM_no_out <- lmer(DR ~ Groups * as.factor(Condition) + 
                            (1 | Participant), data=GM_ind %>% slice (-712, -880))


anova(lmer_Gm) %>%
  knitr::kable()

anova(lmer_GM_no_out) %>%
  knitr::kable()


posthocgm <- emmeans(lmer_Gm, "Condition", data=GM_ind)
pairs(posthocgm, adjust="bonferroni")


posthocgm

Gm_emmeans = emmeans(lmer_Gm, specs = pairwise ~ as.factor(Condition))
Gm_emmeans

Gm_sum <- summarySE(GM_ind, measurevar="DR", groupvars=c("Groups", "Condition"))
Gm_sum


#Graph for GM Mean DR

GM_DR_ind <- GM_ind %>%
  drop_na() %>%
  ggplot(aes(x=as.factor(Condition),
             y=DR))+
  geom_quasirandom(width = 0.15, size = 2, aes(color=Participant)) +
  scale_color_viridis(discrete = TRUE, option = "F", direction=-1)+
  theme_bw(base_size = 14) +
  facet_grid(~Groups)+
  stat_summary(geom = "point",
               fun = "mean",
               size = 4,
               alpha = 0.6,
               position = position_nudge(x = -0.2), 
               shape = 18,
               fill = "black") + 
  geom_errorbar(data=Gm_sum, aes(ymin=DR-ci, ymax = DR+ci),
                position = position_nudge(x = -0.2), width = 0, size = 1) +
  theme(
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size =16),
    strip.text.x = element_text(size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())+  
  labs(x = "" , y= " Gastrocnemius medialis \n motor unit discharges rate (pps) ")

GM_DR_ind + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                              labels=c("Feet neutral", "Feet inwards"))



#Final graphs GL & GM

pp1 <- GL_DR_ind + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                              labels=c("Feet neutral", "Feet in"))

ggsave(file = "GL_ind.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")


pp2 <- GM_DR_ind + theme(legend.position = "none")+ scale_x_discrete(limits=c("FN", "FI"), 
                                                              labels=c("Feet neutral", "Feet in"))

ggsave(file = "GM_ind.tiff", units="in", width = 11, height = 6, dpi = 300, compression = "lzw")

all_ind <- pp1 / pp2 +plot_annotation( tag_levels = 'A', tag_suffix = '.') & 
  theme(plot.tag = element_text(size = 15.5,face="bold"))

all_ind

ggsave(file = "all_ind.tiff", units="in", width = 8, height = 10, dpi = 300, compression = "lzw")



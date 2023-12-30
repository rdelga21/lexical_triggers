## Load package into library
library(ggplot2)
library(tidyr)
library(lme4)

## Load a CSV file by selecting it from the computer
RDBKdata_wide <- read.csv(file.choose())

### DATA WRANGLING ###

## Calculate z-scores by participant
RDBKdata_wide$mean_rating <- rowMeans(RDBKdata_wide[, 21:200])

# Calculate the standard deviation of the judgment ratings for each participant; same columns
RDBKdata_wide$sd_rating <- apply(RDBKdata_wide[, 21:200], 1, sd)

## Convert data from wide format to long format
RDBKdata <- gather(RDBKdata_wide, stimulus, rating, mod_sp.en_1_aux_gr_trained.entrenado:main_en_180_doio_cog_soup.sopa)

## Create new columns that separate out the stimuli variables
RDBKdata <- RDBKdata %>%
  separate(stimulus, into = c("section", "language", "stimulus_number", "structure", "condition", "lexicalization"), sep = "_")

## Calculate the z-scores for each individual rating
RDBKdata$zscore <- (RDBKdata$rating - RDBKdata$mean_rating) / RDBKdata$sd_rating

## Reorder the variables
RDBKdata$condition <- factor(RDBKdata$condition, levels=c("gr", "un", "strand", "pied", "cog", "spec"))
RDBKdata$language <- factor(RDBKdata$language, levels=c("en", "sp", "sp.en", "en.sp"))
RDBKdata$structure <- factor(RDBKdata$structure, levels=c("aux", "pro", "neg", "mxwh", "emwh", "rc", "pp", "emadv", "mxadv", "adj", "do", "doio"))

## Create a subset of the data based on a specific variable
RDBKdata_trigger <- subset(RDBKdata_main, condition %in% c("cog", "spec"))

#Can subdivide by condition, language mode, or language of the lexical item
RDBKdata_trigger_spec <- subset(RDBKdata_trigger, condition == "spec")
RDBKdata_trigger_cog <- subset(RDBKdata_trigger, condition == "cog")
RDBKdata_trigger_cs <- subset(RDBKdata_trigger, language %in% c("sp.en", "en.sp"))
RDBKdata_trigger_en <- subset(RDBKdata_trigger, language %in% c("en", "sp.en"))
RDBKdata_trigger_sp <- subset(RDBKdata_trigger, language %in% c("sp", "en.sp"))

## DESCRIPTIVES

# Calculate the overall raw Likert rating means and SDs for each language(s) by condition
means_TRIGGER_overall_cog <- tapply(RDBKdata_trigger_cog$rating, RDBKdata_trigger_cog$language, mean)
means_TRIGGER_overall_cog
sds_TRIGGER_overall_cog <- tapply(RDBKdata_trigger_cog$rating, RDBKdata_trigger_cog$language, sd)
sds_TRIGGER_overall_cog
means_TRIGGER_overall_spec <- tapply(RDBKdata_trigger_spec$rating, RDBKdata_trigger_spec$language, mean)
means_TRIGGER_overall_spec
sds_TRIGGER_overall_spec <- tapply(RDBKdata_trigger_spec$rating, RDBKdata_trigger_spec$language, sd)
sds_TRIGGER_overall_spec

# Calculate the overall z-score means and SDs for each language(s) by condition
means_TRIGGER_overall_cog <- tapply(RDBKdata_trigger_cog$zscore, RDBKdata_trigger_cog$language, mean)
means_TRIGGER_overall_cog
sds_TRIGGER_overall_cog <- tapply(RDBKdata_trigger_cog$zscore, RDBKdata_trigger_cog$language, sd)
sds_TRIGGER_overall_cog
means_TRIGGER_overall_spec <- tapply(RDBKdata_trigger_spec$zscore, RDBKdata_trigger_spec$language, mean)
means_TRIGGER_overall_spec
sds_TRIGGER_overall_spec <- tapply(RDBKdata_trigger_spec$zscore, RDBKdata_trigger_spec$language, sd)
sds_TRIGGER_overall_spec

# Calculate the overall z-score means and SDs for each lexicalization by condition
means_TRIGGER_lexicalization_cog <- tapply(RDBKdata_trigger_cog$zscore, RDBKdata_trigger_cog$lexicalization, mean)
means_TRIGGER_lexicalization_cog
sds_TRIGGER_lexicalization_cog <- tapply(RDBKdata_trigger_cog$zscore, RDBKdata_trigger_cog$lexicalization, sd)
sds_TRIGGER_lexicalization_cog
means_TRIGGER_lexicalization_spec <- tapply(RDBKdata_trigger_spec$zscore, RDBKdata_trigger_spec$lexicalization, mean)
means_TRIGGER_lexicalization_spec
sds_TRIGGER_lexicalization_spec <- tapply(RDBKdata_trigger_spec$zscore, RDBKdata_trigger_spec$lexicalization, sd)
sds_TRIGGER_lexicalization_spec

## CHARTS

# First one is the raw Likert-scale ratings (i.e., 1-7)
box_TRIGGER_raw <- ggplot(RDBKdata_trigger, aes(x=language, y=rating, fill=language)) + # Define the x-axis smaller variable, y-axis variable, and coloring variable
  geom_boxplot(alpha=0.90) + # Set the fill shading percentage
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + # Label the x-axis and rename the variables
  facet_grid(. ~ condition, labeller = labeller(condition = c("cog" = "COGNATE", "spec" = "LANGUAGE-SPECIFIC"))) + # Define the x-axis larger groups and rename them
  scale_y_continuous(name = "Acceptability rating", # Label the y-axis
                     breaks = seq(1, 7, 1), # Set start, endpoint, and tick mark increments of the y-axis
                     limits=c(1, 7)) + # Sets the limits for the values; make sure these are larger than any of the individual values or there will be an error
  geom_hline(yintercept = 4, linetype="dashed", color = "black", linewidth=.5) + # Darken the middle for the ratings (i.e., 4)
  ggtitle("Lexical item type acceptability by language(s)") + # Title the boxplot
  theme(plot.title = element_text(size = 12, face = "bold")) + # Change the title font
  theme(axis.title = element_text(size = 12, face = "bold")) + # Change the x-axis and y-axis font
  theme(strip.text = element_text(size = 12)) + # Change the x-axis larger groups font
  theme(axis.text = element_text(size = 12)) + # Change x-axis and y-axis labels font
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) # Define the fill colors
box_TRIGGER_raw 

# Same as above, but with zscores
box_TRIGGER_z <- ggplot(RDBKdata_trigger, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("cog" = "COGNATE", "spec" = "LANGUAGE-SPECIFIC"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("Lexical item type acceptability by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_TRIGGER_z

# Just English items separated by the specific lexicalization
box_TRIGGER_lexicalization_en_z <- ggplot(RDBKdata_trigger_en, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "CS")) + 
  facet_grid(. ~ condition ~ lexicalization, labeller = labeller(condition = c("cog" = "COGNATE", "spec" = "LANGUAGE-SPECIFIC"), lexicalization = c("stupid.estúpido" = "stupid.boneheaded", "native.nativo" = "native.snooty", "fruit.fruta" = "fruit.crackers", "coffee.café" = "coffee.seltzer", "pancakes.panqueques" = "pancakes.biscuits", "soup.sopa" = "soup.clam chowder"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("English lexical item type acceptability by language mode") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 9)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#1ABC9C")) 
box_TRIGGER_lexicalization_en_z

# Just Spanish items separated by the specific lexicalization
box_TRIGGER_lexicalization_sp_z <- ggplot(RDBKdata_trigger_sp, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("SP", "CS")) + 
  facet_grid(. ~ condition ~ lexicalization, labeller = labeller(condition = c("cog" = "COGNATE", "spec" = "LANGUAGE-SPECIFIC"), lexicalization = c("stupid.estúpido" = "estúpido.naco", "native.nativo" = "nativo.chicano", "fruit.fruta" = "fruta.nopales", "coffee.café" = "café.champurrado", "pancakes.panqueques" = "panqueques.atole", "soup.sopa" = "sopa.pozole"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("Spanish lexical item type acceptability by language mode") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 9)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#E74C3C", "#F39C12")) 
box_TRIGGER_lexicalization_sp_z


## STATS

# Overall ANOVA, with participant and item as within-subject variables (i.e., repeated measures)
overall_trigger.aov <- aov(zscore ~ language * condition + Error(participant / stimulus_number), data = RDBKdata_trigger)
summary(overall_trigger.aov)

# English lexicalization ANOVA with participant and item as within-subject variables (i.e., repeated measures)
en_lexicalization_trigger.aov <- aov(zscore ~ lexicalization*condition + Error(participant / stimulus_number), data = RDBKdata_trigger_en)
summary(en_lexicalization_trigger.aov)

# Spanish lexicalization ANOVA with participant and item as within-subject variables (i.e., repeated measures)
sp_lexicalization_trigger.aov <- aov(zscore ~ lexicalization*condition + Error(participant / stimulus_number), data = RDBKdata_trigger_sp)
summary(sp_lexicalization_trigger.aov)

# Overall Tukey post hoc test (one variable)
overall_trigger.tukey <- TukeyHSD(overall_trigger.aov, which = "language")
overall_trigger.tukey

# English lexicalization Tukey post hoc test
en_lexicalization_trigger.tukey <- TukeyHSD(en_lexicalization_trigger.aov, which = "lexicalization")
en_lexicalization_trigger.tukey

# Spanish lexicalization Tukey post hoc test
sp_lexicalization_trigger.tukey <- TukeyHSD(sp_lexicalization_trigger.aov, which = "lexicalization")
sp_lexicalization_trigger.tukey

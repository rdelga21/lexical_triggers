### GENERAL SET UP ###


## Check which packages are already installed
library()


## Install package
# Package name must appear within quotation marks (" ")
install.packages("name")


## Load package into library
# This time, no " " is needed 
library(ggplot2)
library(tidyr)


## Load a CSV file by selecting it from the computer
# Data from Qualtrics comes in wide format, since there is one row for every participant
RDBKdata_wide <- read.csv(file.choose())



### DATA WRANGLING ###
## Calculate z-scores by participant
# Calculate the mean judgment rating for each participant; the numbers in the code indicate which columns are their judgment ratings
RDBKdata_wide$mean_rating <- rowMeans(RDBKdata_wide[, 21:200])

# Calculate the standard deviation of the judgment ratings for each participant; same columns
RDBKdata_wide$sd_rating <- apply(RDBKdata_wide[, 21:200], 1, sd)


## Convert data from wide format to long format
# Instead of having each row be one participant, we want a row for each participant's rating of a specific stimulus 
# Gather elements = original data frame, new column name for the gathered columns, new column name for the data that was in the rows, range of columns to be gathered)
library(tidyr)
RDBKdata <- gather(RDBKdata_wide, stimulus, rating, mod_sp.en_1_aux_gr_trained.entrenado:main_en_180_doio_cog_soup.sopa)


## Create new columns that separate out the stimuli variables
# Need to have set up the stimulus column data as a string that can be easily separated: variable1_variable2_variable3_variable4...
# For examples, mod_sp.en_1_aux_gr_trained.entrenado means for that stimulus, it was in the modality section of the study, with Spanish-to-English as the languages, etc.
library(tidyr)
RDBKdata <- RDBKdata %>%
  separate(stimulus, into = c("section", "language", "stimulus_number", "structure", "condition", "lexicalization"), sep = "_")


## Calculate the z-scores for each individual rating
RDBKdata$zscore <- (RDBKdata$rating - RDBKdata$mean_rating) / RDBKdata$sd_rating


## For the LANGUAGE DOMINANCE project, need to calculate the language dominance of each individual section of the BLP
# Create a new column that subtracts the value of one column from another
RDBKdata$blp_history <- RDBKdata$blp_history_english - RDBKdata$blp_history_spanish
RDBKdata$blp_use <- RDBKdata$blp_use_english - RDBKdata$blp_use_spanish
RDBKdata$blp_proficiency <- RDBKdata$blp_proficiency_english - RDBKdata$blp_proficiency_spanish
RDBKdata$blp_attitudes <- RDBKdata$blp_attitudes_english - RDBKdata$blp_attitudes_spanish


## Reorder the variables
# This the order that I want them to show up in later in charts, etc., instead of alphabetical
RDBKdata$condition <- factor(RDBKdata$condition, levels=c("gr", "un", "strand", "pied", "cog", "spec"))
RDBKdata$language <- factor(RDBKdata$language, levels=c("en", "sp", "sp.en", "en.sp"))
RDBKdata$structure <- factor(RDBKdata$structure, levels=c("aux", "pro", "neg", "mxwh", "emwh", "rc", "pp", "emadv", "mxadv", "adj", "do", "doio"))


## Create a subset of the data based on a specific variable
# When there is only one value you want
# The first three are used for the MODALITY project and the DOMINANCE project; the last one will be divided up for the other projects
RDBKdata_mod <- subset(RDBKdata, section == "mod")
RDBKdata_mod_gr <- subset(RDBKdata, condition == "gr")
RDBKdata_mod_un <- subset(RDBKdata, condition == "un")
RDBKdata_main <- subset(RDBKdata, section == "main")

# When there is multiple values you want
# The first one is for the P-STRANDING project, the second for the DIRECTION project, and the third for the TRIGGER project
RDBKdata_pstranding <- subset(RDBKdata_main, condition %in% c("strand", "pied"))
RDBKdata_direction <- subset(RDBKdata_main, structure %in% c("pp", "emadv", "mxadv")) # This one is being selected by structure instead of condition, because the "condition" is already in the language column for this project, but the labels are not unique to just this project
RDBKdata_trigger <- subset(RDBKdata_main, condition %in% c("cog", "spec"))

# Within the TRIGGER project, can subdivide by condition, language mode, or language of the lexical item
RDBKdata_trigger_spec <- subset(RDBKdata_trigger, condition == "spec")
RDBKdata_trigger_cog <- subset(RDBKdata_trigger, condition == "cog")
RDBKdata_trigger_cs <- subset(RDBKdata_trigger, language %in% c("sp.en", "en.sp"))
RDBKdata_trigger_en <- subset(RDBKdata_trigger, language %in% c("en", "sp.en"))
RDBKdata_trigger_sp <- subset(RDBKdata_trigger, language %in% c("sp", "en.sp"))

# Within the P-STRANDING project, can subdivide by preposition, structure, or condition
RDBKdata_pstranding_with.con <- subset(RDBKdata_pstranding, lexicalization == "with.con")
RDBKdata_pstranding_of.de <- subset(RDBKdata_pstranding, lexicalization == "of.de")
RDBKdata_pstranding_to.a <- subset(RDBKdata_pstranding, lexicalization == "to.a")
RDBKdata_pstranding_mxwh <- subset(RDBKdata_pstranding, structure == "mxwh")
RDBKdata_pstranding_emwh <- subset(RDBKdata_pstranding, structure == "emwh")
RDBKdata_pstranding_rc <- subset(RDBKdata_pstranding, structure == "rc")
RDBKdata_pstranding_strand <- subset(RDBKdata_pstranding, condition == "strand")
RDBKdata_pstranding_pied <- subset(RDBKdata_pstranding, condition == "pied")



### TRIGGER project ###
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


## TRIGGER CHARTS
# Box plots
library(ggplot2)
library(tidyverse)

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


## TRIGGER STATS
# Two-way ANOVA
# Write-up: There was a/no significant main effect of XXXX on the XXXX, F(#Df, #DfResiduals) = #.###, p = .###, ω2 = .##, with XXXX rated significantly higher than XXXX, and there was a/no significant main effect of XXXX on XXXX, F(#Df, #DfResiduals) = #.##, p = .###, ω2 = .##. There was also a/no significant interaction effect between XXXX and XXXX on XXXX, F(#Df, #DfResiduals) = #.###, p = .####, ω2 = .##. This indicates that XXXX and XXXX were affected differently by XXXX. 
# Overall ANOVA
overall_trigger.aov <- aov(zscore ~ language*condition, data = RDBKdata_trigger)
summary(overall_trigger.aov)

# Function to compute effect sizes
omega_factorial<-function(n, a, b, SSa, SSb, SSab, SSr)
{
  MSa<-SSa/(a-1) 
  MSb<-SSb/(b-1) 
  MSab<-SSab/((a-1)*(b-1)) 
  MSr<-SSr/(a*b*(n-1)) 
  varA<-((a-1)*(MSa-MSr))/(n*a*b) 
  varB<-((b-1)*(MSb-MSr))/(n*a*b) 
  varAB<-((a-1)*(b-1)*(MSab-MSr))/(n*a*b) 
  varTotal<-varA + varB + varAB + MSr 
  print(paste("Omega-Squared A: ", varA/varTotal)) 
  print(paste("Omega-Squared B: ", varB/varTotal)) 
  print(paste("Omega-Squared AB: ", varAB/varTotal))
}

# Actually calculate effect sizes
# First number is the number of ratings per group/condition (i.e., # of each type of stimuli)
# Second is the number of levels of first factor (i.e, # of languages)
# Third is the levels of second factor (i.e., # of conditions)
# The last three are the sum_of_squares, which come from the ANOVA output, in order
# Interpret effect size
# < 0.01 - Very small
# < 0.06 - Small
# < 0.14 - Medium
# > 0.14 - Large
omega_factorial(6, 4, 2, 1.78, 98.8, 0.2, 0.9) 

#Tukey post hoc test (one variable)
overall_trigger.tukey <- TukeyHSD(overall_trigger.aov, which = "language")
overall_trigger.tukey



### P-STRANDING project ###
## DESCRIPTIVES
# Calculate the overall raw Likert rating means and SDs for each language(s) by condition
means_PSTRANDING_overall_strand <- tapply(RDBKdata_pstranding_strand$rating, RDBKdata_pstranding_strand$language, mean)
means_PSTRANDING_overall_strand
sds_PSTRANDING_overall_strand <- tapply(RDBKdata_pstranding_strand$rating, RDBKdata_pstranding_strand$language, sd)
sds_PSTRANDING_overall_strand
means_PSTRANDING_overall_pied <- tapply(RDBKdata_pstranding_strand$rating, RDBKdata_pstranding_strand$language, mean)
means_PSTRANDING_overall_pied
sds_PSTRANDING_overall_pied <- tapply(RDBKdata_pstranding_strand$rating, RDBKdata_pstranding_strand$language, sd)
sds_PSTRANDING_overall_pied

# Calculate the overall z-score means and SDs for each language(s)
means_PSTRANDING_overall_strand <- tapply(RDBKdata_pstranding_strand$zscore, RDBKdata_pstranding_strand$language, mean)
sds_PSTRANDING_overall_strand <- tapply(RDBKdata_pstranding_strand$zscore, RDBKdata_pstranding_strand$language, sd)
means_PSTRANDING_overall_pied <- tapply(RDBKdata_pstranding_strand$zscore, RDBKdata_pstranding_strand$language, mean)
sds_PSTRANDING_overall_pied <- tapply(RDBKdata_pstranding_strand$zscore, RDBKdata_pstranding_strand$language, sd)
means_PSTRANDING_overall_strand
sds_PSTRANDING_overall_strand
means_PSTRANDING_overall_pied
sds_PSTRANDING_overall_pied


## P-STRANDING CHARTS
# Box plots
library(ggplot2)
library(tidyverse)

# First one is the raw Likert-scale ratings (i.e., 1-7)
box_PSTRANDING_raw <- ggplot(RDBKdata_pstranding, aes(x=language, y=rating, fill=language)) + # Define the x-axis smaller variable, y-axis variable, and coloring variable
  geom_boxplot(alpha=0.90) + # Set the fill shading percentage
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + # Label the x-axis and rename the variables
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + # Define the x-axis larger groups and rename them
  scale_y_continuous(name = "Acceptability rating", # Label the y-axis
                     breaks = seq(1, 7, 1), # Set start, endpoint, and tick mark increments of the y-axis
                     limits=c(1, 7)) + # Sets the limits for the values; make sure these are larger than any of the individual values or there will be an error
  geom_hline(yintercept = 4, linetype="dashed", color = "black", linewidth=.5) + # Darken the middle for the ratings (i.e., 4)
  ggtitle("P-stranding and pied-piping acceptability by language(s)") + # Title the boxplot
  theme(plot.title = element_text(size = 12, face = "bold")) + # Change the title font
  theme(axis.title = element_text(size = 12, face = "bold")) + # Change the x-axis and y-axis font
  theme(strip.text = element_text(size = 12)) + # Change the x-axis larger groups font
  theme(axis.text = element_text(size = 12)) + # Change x-axis and y-axis labels font
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) # Define the fill colors
box_PSTRANDING_raw 

# Same as above, but with zscores
box_PSTRANDING_z <- ggplot(RDBKdata_pstranding, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_z 

# Just with/con
box_PSTRANDING_with.con_z <- ggplot(RDBKdata_pstranding_with.con, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability of with/con by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_with.con_z 

# Just of/de
box_PSTRANDING_of.de_z <- ggplot(RDBKdata_pstranding_of.de, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability of of/de by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_of.de_z 

# Just to/a
box_PSTRANDING_to.a_z <- ggplot(RDBKdata_pstranding_to.a, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability of to/a by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_to.a_z 

# Just matrix wh-questions
box_PSTRANDING_mxwh_z <- ggplot(RDBKdata_pstranding_mxwh, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability in matrix wh-questions by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_mxwh_z 

# Just embedded wh-questions
box_PSTRANDING_emwh_z <- ggplot(RDBKdata_pstranding_emwh, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability in embedded wh-questions by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_emwh_z 

# Just relative clauses
box_PSTRANDING_rc_z <- ggplot(RDBKdata_pstranding_rc, aes(x=language, y=zscore, fill=language)) + 
  geom_boxplot(alpha=0.90) + 
  scale_x_discrete(name = "Language(s)", labels = c("EN", "SP", "SP-to-EN", "EN-to-SP")) + 
  facet_grid(. ~ condition, labeller = labeller(condition = c("pied" = "PIED-PIPING", "strand" = "P-STRANDING"))) + 
  scale_y_continuous(name = "Acceptability z-score", 
                     breaks = seq(-2, 2, .5),  
                     limits=c(-2, 2)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", linewidth=.5) + 
  ggtitle("P-stranding and pied-piping acceptability in relative clauses by language(s)") + 
  theme(plot.title = element_text(size = 12, face = "bold")) + 
  theme(axis.title = element_text(size = 12, face = "bold")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12)) + 
  theme(legend.position="none") + 
  scale_fill_manual(values=c("#3498DB", "#E74C3C", "#1ABC9C", "#F39C12")) 
box_PSTRANDING_rc_z 



### DOMINANCE project ###
## CHARTS
# Scatter plots
library(ggplot2)

# Overlay two scatter plots -- need to have already create two new data frames that separate the two variables you are comparing
# Overall language dominance
scatter_DOMINANCE_overall_raw <- ggplot() + #First one is the raw Likert-scale ratings (i.e., 1-7)
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp, y = rating), color = "#4c9fdc") + # First scatter plot that just plots the grammatical switches
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp, y = rating), color = "#e65c51") + # Second scatter plot with just the ungrammatical switches
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + # First trend line; TRUE turns on the confidence interval shading
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp, y = rating), method = "lm", se = TRUE, color = "#e65c51") + # Second trend line
  geom_text(data = data.frame(x = 50, y = 5.25, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + # Label the first trend line
  geom_text(data = data.frame(x = 50, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + # Label the second trend line
  scale_x_continuous(
    breaks = seq(0, 125, by = 25) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Language Dominance") + # Label the x-axis
  ylab("Acceptabilty Rating") + # Label the y-axis
  ggtitle("(Un)grammatical switch ratings by overall language dominance") # Label the graph
scatter_DOMINANCE_overall_raw

# Same as above, but with z-scores
scatter_DOMINANCE_overall_z <- ggplot() + # Same as above, but with zscores
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 65, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 65, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(0, 125, by = 25) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Language Dominance") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by overall language dominance") 
scatter_DOMINANCE_overall_z

# Language history
scatter_DOMINANCE_history_raw <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_history, y = rating), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_history, y = rating), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_history, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_history, y = rating), method = "lm", se = TRUE, color = "#e65c51") +
  geom_text(data = data.frame(x = 25, y = 5.25, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 25, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(0, 125, by = 10) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Language Dominance - History Only") +
  ylab("Acceptabilty Rating") + 
  ggtitle("(Un)grammatical switch ratings by language history") 
scatter_DOMINANCE_history_raw

scatter_DOMINANCE_history_z <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_history, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_history, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_history, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_history, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 37, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 37, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(0, 125, by = 10) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Language Dominance") +   
  xlab("Language Dominance") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by language history") 
scatter_DOMINANCE_history_z

# Language use
scatter_DOMINANCE_use_raw <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_use, y = rating), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_use, y = rating), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_use, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_use, y = rating), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 20, y = 5.25, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 20, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(0, 125, by = 10) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Language Dominance - Use Only") + 
  ylab("Acceptabilty Rating") +
  ggtitle("(Un)grammatical switch ratings by language use") 
scatter_DOMINANCE_use_raw

# Same as above, but with z-scores
scatter_DOMINANCE_use_z <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_use, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_use, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_use, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_use, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 18, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") +
  geom_text(data = data.frame(x = 18, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(0, 125, by = 10) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Language Dominance - Use Only") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by language use") 
scatter_DOMINANCE_use_z

# Language proficiency
scatter_DOMINANCE_proficiency_raw <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_proficiency, y = rating), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_proficiency, y = rating), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_proficiency, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_proficiency, y = rating), method = "lm", se = TRUE, color = "#e65c51") +
  geom_text(data = data.frame(x = 6, y = 5.25, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") +
  geom_text(data = data.frame(x = 6, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 2) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Language Dominance - Proficiency Only") + 
  ylab("Acceptabilty Rating") + 
  ggtitle("(Un)grammatical switch ratings by language proficiency") 
scatter_DOMINANCE_proficiency_raw

# Same as above, but with z-scores
scatter_DOMINANCE_proficiency_z <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_proficiency, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_proficiency, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_proficiency, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_proficiency, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 6.5, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 6.5, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 2) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Language Dominance - Proficiency Only") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by language proficiency") 
scatter_DOMINANCE_proficiency_z

# Language attitudes
scatter_DOMINANCE_attitudes_raw <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_attitudes, y = rating), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_attitudes, y = rating), color = "#e65c51") +
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_attitudes, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_attitudes, y = rating), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = -1, y = 5.25, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 1, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 2) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Language Dominance - Attitudes Only") + 
  ylab("Acceptabilty Rating") + 
  ggtitle("(Un)grammatical switch ratings by language attitudes") 
scatter_DOMINANCE_attitudes_raw

# Same as above, but with z-scores
scatter_DOMINANCE_attitudes_z <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = blp_attitudes, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = blp_attitudes, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = blp_attitudes, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") +
  geom_smooth(data = RDBKdata_mod_un, aes(x = blp_attitudes, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 7.5, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 7.5, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 2) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Language Dominance - Attitudes Only") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by language attitudes") 
scatter_DOMINANCE_attitudes_z

# Spanish proficiency DELE
scatter_DOMINANCE_dele_raw <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = proficiency_spanish, y = rating), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = proficiency_spanish, y = rating), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = proficiency_spanish, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = proficiency_spanish, y = rating), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 34.5, y = 6.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 34.5, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 5) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Spanish Proficiency (Multiple-Choice Test)") + 
  ylab("Acceptabilty Rating") + 
  ggtitle("(Un)grammatical switch ratings by Spanish proficiency") 
scatter_DOMINANCE_dele_raw

# Same as above, but with z-scores
scatter_DOMINANCE_dele_z <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = proficiency_spanish, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = proficiency_spanish, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = proficiency_spanish, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = proficiency_spanish, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 34.5, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 34.5, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 5) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Spanish Proficiency (Multiple-Choice Test)") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by Spanish proficiency") 
scatter_DOMINANCE_dele_z

# Spanish proficiency LEXTALE
scatter_DOMINANCE_lextale_raw <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = lextale_spanish, y = rating), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = lextale_spanish, y = rating), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = lextale_spanish, y = rating), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = lextale_spanish, y = rating), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 23.5, y = 5.25, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 23.5, y = 1.25, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 5) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(1, 7, by = 1)
  ) +
  xlab("Spanish Proficiency (Lexical Decision Task)") + 
  ylab("Acceptabilty Rating") + 
  ggtitle("(Un)grammatical switch ratings by Spanish proficiency") 
scatter_DOMINANCE_lextale_raw

# Same as above, but with z-scores
scatter_DOMINANCE_lextale_z <- ggplot() +
  geom_point(shape = 1, data = RDBKdata_mod_gr, aes(x = lextale_spanish, y = zscore), color = "#4c9fdc") + 
  geom_point(shape = 1, data = RDBKdata_mod_un, aes(x = lextale_spanish, y = zscore), color = "#e65c51") + 
  geom_smooth(data = RDBKdata_mod_gr, aes(x = lextale_spanish, y = zscore), method = "lm", se = TRUE, color = "#4c9fdc") + 
  geom_smooth(data = RDBKdata_mod_un, aes(x = lextale_spanish, y = zscore), method = "lm", se = TRUE, color = "#e65c51") + 
  geom_text(data = data.frame(x = 19.5, y = 1.5, label = "Grammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#4c9fdc") + 
  geom_text(data = data.frame(x = 23.5, y = -2, label = "Ungrammatical"), aes(x = x, y = y, label = label), vjust = -1, color = "#e65c51") + 
  scale_x_continuous(
    breaks = seq(-10, 125, by = 5) # Define the x-axis tick marks; where they start, where they end, and at what interval
  ) +
  scale_y_continuous(
    breaks = seq(-3, 2, by = .5)
  ) +
  xlab("Spanish Proficiency (Lexical Decision Task)") + 
  ylab("Acceptabilty z-score") + 
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size=.5) + 
  ggtitle("(Un)grammatical switch ratings by Spanish proficiency") 
scatter_DOMINANCE_lextale_z



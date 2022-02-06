

# setup -------------------------------------------------------------------


#library

library(tidyverse)

# Read the data files

Fish <- read.csv("./Data/FishData.csv")
Lakes <- read.csv("./Data/LakeData.csv")


# Examine data structure --------------------------------------------------


# basic merge by 'Pop' (repeat from last week's script)
FishLakes <- merge(Fish, Lakes, all.x = T)

# show first 6 rows of all columns
head(FishLakes)

# what type of object is FishLake?
class(FishLakes)

# Let's explore the structure a bit more - what's inside?
str(FishLakes)

# What are the unique character values?
unique(FishLakes$Tech)
unique(FishLakes$Sex)
unique(FishLakes$Pop)
unique(FishLakes$Pred)
unique(FishLakes$EW)


# Fix a couple of things, factors, reorder --------------------------------


# EW might need fixed, replace empty cells with NA
FishLakes$EW <- 
  ifelse(FishLakes$EW == '',  # if EW is empty, return NA (character), if not, keep the information
       NA_character_, 
       FishLakes$EW)

# change sex from ch to factor
FishLakes$Sex <- as.factor(FishLakes$Sex)

# did that actually happen?
str(FishLakes)

# change multiple character variables to factors

vars_to_fct <- c('Pop', 'Tech', 'Pred', 'EW') # create a vector of variable names 
FishLakes[,vars_to_fct] <- lapply(FishLakes[,vars_to_fct], 
                                  factor)

# check again (does not have to be part of the script, but one should do it!)

str(FishLakes)

## reordering predator factor levels

levels(FishLakes$Pred) # show levels of 'predators'

FishLakes$Pred <- 
  factor(FishLakes$Pred,
         levels = c('No', 'Bass', 'Bluegill', 'Both', 'Hatchery'), # reorder
         labels = c('No', 'Bass', 'Bluegill', 'Bass & Bluegill', 'Hatchery')  # rename   
  )   

# Add variable indicating whether lake there was a predator or not? 

FishLakes$PredPresence <- 
  as.factor(
    ifelse(FishLakes$Pred == 'No',
         'No',
         'Yes'))


# Summaries -----------------------------------------------------------------


# summary of all variables
summary(FishLakes)

# calculate mean and sd of fish length
mean(FishLakes$Len_cm)
sd(FishLakes$Len_cm)

# calculate mean and sd of fish length separately for female and male
tapply(FishLakes$Len_cm, FishLakes$Sex, mean)
tapply(FishLakes$Len_cm, FishLakes$Sex, sd)

# calculate mean and sd of fish length separately for female and male
tapply(FishLakes$Len_cm, FishLakes$Sex, mean)
tapply(FishLakes$Len_cm, FishLakes$Sex, sd)


# Little bit of data manipulation -----------------------------------------


# e.g., subset data to those collected by CJS (indexing = [row index , column index]
FishLakes[which(FishLakes$Tech == 'CJS'), ]

# CJS and only angle data
FishLakes[which(FishLakes$Tech == 'CJS'), 5:7]

# selection of multiple subsets 
pops <-  c('AT', 'AW', 'SY')
FishLakes[which(FishLakes$Pop %in% pops),]

# select only fish that are larger than mean value 
mean_length <- mean(FishLakes$Len_cm)
FishLakes[which(FishLakes$Len_cm > mean_length),]

# RQ: How does presence of predators affect mosquito fish length? 

# Intro to GGplot ---------------------------------------------------------

## distributions (frequent/unique values, shape of distribution - skew, kurtosis)

# add data
ggplot(data = FishLakes)

# map variables 
ggplot(data = FishLakes, 
       mapping = aes(x = Len_cm))

# geometries
ggplot(data = FishLakes, aes(x = Len_cm)) +
  geom_histogram()

ggplot(data = FishLakes, aes(x = Len_cm)) +
  geom_density()

# color
ggplot(data = FishLakes, aes(x = Len_cm, color = Sex)) +
  geom_histogram() 

# fill
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram() 

# scales
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram() +
  scale_fill_manual(values = c('firebrick', 'skyblue3')) 

# positioning, add labels, legend
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram(position = position_dodge()) +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  labs(x = "Length (cm)", 
       y = "Number of obervations")

# facetting
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram() +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  labs(x = "Length (cm)", 
       y = "Number of obervations") +
  facet_wrap(~Sex)

# panels under
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram() +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  labs(x = "Length (cm)", 
       y = "Number of obervations") +
  facet_wrap(~Sex, nrow = 2)

# adjust binwidth
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  labs(x = "Length (cm)", 
       y = "Number of obervations") +
  facet_wrap(~Sex, nrow = 2)

# adjust limits of y-axis
ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0,0)) +
  labs(x = "Length (cm)", 
       y = "Number of obervations") +
  facet_wrap(~Sex, nrow = 2)

# rename panels
sex <- c(
  'F'="Female",
  'M'="Male")

ggplot(data = FishLakes, aes(x = Len_cm, fill = Sex)) +
  geom_histogram(binwidth = 0.05) +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  scale_y_continuous(limits = c(0, 50),
                     expand = c(0,0)) +
  labs(x = "Length (cm)", 
       y = "Number of obervations") +
  facet_wrap(~Sex, 
             labeller = labeller(Sex = sex), 
             nrow = 2)

# make it fancy
FishLakes %>% 
  group_by(Sex) %>% 
  mutate(meanLength = mean(Len_cm)) %>% 
  ggplot(aes(x = Len_cm)) +
  geom_histogram(aes(fill = Sex), binwidth = 0.05) +
  geom_vline(aes(xintercept = meanLength, 
                 color = Sex), 
             linetype = "dashed", 
             size = 0.7) +
  scale_fill_manual('Sex of fish:',
                    values = c('firebrick', 'skyblue3'),
                    labels = c('Female', 'Male')) +
  scale_color_manual(values = c('firebrick2', 'skyblue2')) +
  labs(x = "Length (cm)", 
       y = "Number of obervations") +
  facet_wrap(~Sex, 
             labeller = labeller(Sex = sex), 
             nrow = 2)

# what do we know about fish length now??
#
#
#

# Number of samples -------------------------------------------------------

# bar plot (balance of design)

# how many observations of females and males?
ggplot(data = FishLakes, aes(x = Sex)) +
  geom_bar() +
  labs(x = "Sex", 
       y = "Count") 

# how many observations of each population/lake?
ggplot(data = FishLakes, aes(x = Pop)) +
  geom_bar() +
  labs(x = "Population", 
       y = "Count") 

# reoreder
ggplot(data = FishLakes, aes(x = fct_infreq(Pop))) +
  geom_bar() +
  labs(x = "Population", 
       y = "Count")

# flip
ggplot(data = FishLakes, aes(x = fct_infreq(Pop))) +
  geom_bar() +
  labs(x = "Population", 
       y = "Count") +
  coord_flip()

# fancy it up a little bit
ggplot(data = FishLakes, aes(x = fct_infreq(Pop))) +
  geom_bar(aes(fill = Sex),
           position = position_dodge()) +
  labs(x = "Population", 
       y = "Count") +
  scale_fill_manual(values = c('firebrick', 'skyblue3')) +
  scale_y_continuous(limits = c(0, 40), # adjust axis limits
                     expand = c(0, 0), # adjust expansion
                     breaks = seq(0, 40, # add breaks
                                  by = 5)) +
  coord_flip()


# boxplots ----------------------------------------------------------------

# outliers, range of values, median+IQR, variance

# variability in fish length between sexes
ggplot(data = FishLakes, aes(x = Sex, y = Len_cm, color = Sex)) +
  geom_boxplot() +
  labs(x = "Sex", 
       y = "Length (cm)") +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  scale_x_discrete(labels = c('Female', 'Male'))

# populations
ggplot(data = FishLakes, aes(x = reorder(Pop, Len_cm), y = Len_cm)) +
  geom_boxplot() +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

# pradation
ggplot(data = FishLakes, aes(x = reorder(Pred, Len_cm), y = Len_cm)) +
  geom_boxplot() +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

# boxplots + distribution

ggplot(data = FishLakes, aes(x = reorder(Pred, Len_cm), y = Len_cm)) +
  geom_boxplot() +
  geom_point(alpha = 0.7) +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

ggplot(data = FishLakes, aes(x = reorder(Pred, Len_cm), y = Len_cm)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, width = 0.1) +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

# violins = alternative to boxplots, shows distribution
ggplot(data = FishLakes, aes(x = reorder(Pred, Len_cm), y = Len_cm)) +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(size = 0.7, alpha = 0.3, width = 0.05) +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

# boxplots do not show mean!
ggplot(data = FishLakes, aes(x = reorder(Pred, Len_cm), y = Len_cm)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.7, width = 0.1) +
  stat_summary(fun = mean,
               size = 1,
               color = 'red') +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

# what's going on with sex * predations
ggplot(data = FishLakes, aes(x = reorder(Pred, Len_cm), y = Len_cm)) +
  geom_boxplot(aes(color = Sex)) +
  geom_point(aes(color = Sex), alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25)) +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

ggplot(data = FishLakes, aes(x = reorder(PredPresence, Len_cm), y = Len_cm)) +
  geom_boxplot(aes(color = Sex)) +
  geom_point(aes(color = Sex), alpha = 0.2, 
             position = position_jitterdodge(jitter.width = 0.25)) +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  labs(x = "Population", 
       y = "Length (cm)") +
  coord_flip()

# looks like larger fish are out with predation but min length stays the same

# what about the effect of different technicians?
ggplot(data = FishLakes, aes(x = Tech, y = Len_cm, color = Sex)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  labs(x = "Population", 
       y = "Length (cm)") 

# number of observations per lake / per tech
table(FishLakes$Pop, FishLakes$Tech)

# check the one population that was handled by all techs
del <- FishLakes[which(FishLakes$Pop == 'DeL'),] # subset

ggplot(data = del, aes(x = Tech, y = Len_cm, color = Sex)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  labs(x = "Population", 
       y = "Length (cm)") 

# two continuous variables, scatter ---------------------------------------

ggplot(data = FishLakes, aes(x = Len_cm, y = Tail_Area, color = Sex)) +
  geom_point(alpha = 0.5) +
  labs(x = "Length", 
       y = "Tail_Area") +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) 

# geom_rug
ggplot(data = FishLakes, aes(x = Len_cm, y = Tail_Area, color = Sex)) +
  geom_point(alpha = 0.5) +
  geom_rug() +
  labs(x = "Length", 
       y = "Tail_Area") +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Female', 'Male'),
                     values = c('firebrick', 'skyblue3')) 

# facetting with wrap
FishLakes %>% 
  pivot_longer(cols = 6:16,
               names_to = 'variables',
               values_to = 'value') %>% 
  ggplot(aes(x = Len_cm, y = value, color = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm) +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Femal', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  facet_wrap(~variables)

# facetting with grid
FishLakes %>% 
  pivot_longer(cols = 6:16,
               names_to = 'variables',
               values_to = 'value') %>% 
  ggplot(aes(x = Len_cm, y = value, color = Sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm) +
  scale_color_manual(name = 'Sex:', 
                     labels = c('Femal', 'Male'),
                     values = c('firebrick', 'skyblue3')) +
  facet_grid(variables~Sex, scales = 'free')

# what about predation?
FishLakes %>% 
  pivot_longer(cols = 6:16,
               names_to = 'variables',
               values_to = 'value') %>% 
  ggplot(aes(x = Len_cm, y = value, color = PredPresence)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = lm) +
  scale_color_manual(name = 'Predation:', 
                     labels = c('No', 'Yes'),
                     values = c('cornflowerblue', 'tomato')) +
  facet_wrap(~variables, scales = 'free')

# what do we know about fish length now??
#
#
#


# more stuff ---------------------------------------------------------------

# correlation matrix with something extra

library(GGally)

FishLakes %>% 
  select(3,4,6:16) %>% 
  ggpairs(aes(color = Sex))

FishLakes %>% 
  select(4,6:16, 24) %>% 
  ggpairs(aes(color = PredPresence))

# -------------------------------------------------------------------------




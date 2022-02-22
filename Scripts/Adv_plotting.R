
# setup -------------------------------------------------------------------


#library

library(tidyverse) 
library(ggdist) # awesome package to visualize distributions
library(patchwork) # making panels 
library(colorspace) # functions like lighten, darken, etc to work with colors

# Read the data files

Fish <- read.csv("./Data/FishData.csv")
Lakes <- read.csv("./Data/LakeData.csv")


# prepare data ------------------------------------------------------------

# basic merge by 'Pop' (repeat from last week's script)
FishLakes <- merge(Fish, Lakes, all.x = T)

# EW might need fixed, replace empty cells with NA
FishLakes$EW <- 
  ifelse(FishLakes$EW == '',  # if EW is empty, return NA (character), if not, keep the information
         NA_character_, 
         FishLakes$EW)

# change multiple character variables to factors

vars_to_fct <- c('Pop', 'Tech', 'Pred', 'EW', 'Sex') # create a vector of variable names 
FishLakes[,vars_to_fct] <- lapply(FishLakes[,vars_to_fct], 
                                  factor)

## reordering predator factor levels

levels(FishLakes$Pred) # show levels of 'predators'

FishLakes$Pred <- 
  factor(FishLakes$Pred,
         levels = c('No', 'Bass', 'Bluegill', 'Both', 'Hatchery'), # reorder
         labels = c('Without\nPredator', 'Bass', 'Bluegill', 'Bass &\nBluegill', 'Hatchery')  # rename   
  )   
# \n will spread it in two rows

# Add variable indicating whether lake there was a predator or not? 

FishLakes$PredPresence <- 
  as.factor(
    ifelse(FishLakes$Pred == 'Without\nPredator',
           'No',
           'Yes'))


# plotting ----------------------------------------------------------------

# set theme for the whole script!
# you can also tweak the settings with each plot separately using theme()
# if you dont like any of the pre-made themes that come along with gg, you can also write a theme function yourself and then load it 
# using a source script

theme_set(theme_bw(
  base_size = 10,
  base_family = "Helvetica")) 

# one can actually download fonts from google and use those instead, use {showtext}


# boxplot alternative
# wrapping an object in parenthesis prints it without specifically calling its name)

(box <- 
    ggplot(data = FishLakes, 
           aes(x = reorder(Pred, Len_cm), y = Len_cm,
               group = Sex)) +
    geom_point(aes(color = Sex),
               stroke = 0,
               size = 0.5,
               alpha = 0.15, 
               position = position_jitterdodge(jitter.width = 0.55,
                                               dodge.width = 1), # jitter points, specify width of jitter and space between categories
               show.legend = FALSE) + # do not show legend for points
    scale_fill_manual(labels = c('Female', 'Male'), # specify lables and colors for fill and color aesthetics
                      values = c('#F2721D', '#009DA6')) +
    scale_color_manual(labels = c('Female', 'Male'),
                       values = c('#F2721D', '#009DA6')) +
    stat_gradientinterval(aes(fill = Sex,
                              color = after_scale(darken(fill, 0.2))), # gg can only handle one color and one fill scale
                          # using after_scale + some function like lighten or darken, can allow one to increase the range of 
                          # colors used
                          position = "dodge",
                          scale = 0.65,
                          justification = 0.5, # the interval will be in the middle of the strip
                          point_size = 2) +
    labs(x = NULL, # if I specify NULL< there will be no space on the side
         y = 'Length of mosquitofish (cm)',
         tag = 'a)') + # tags are used for designating panel, you also add captions with parameter 'caption'
    guides(color = guide_legend(override.aes = list(point_size = 2,
                                                    fill = 'transparent'))) + # legend can be manipulate with guides but also from themes
    coord_flip() +
    theme(legend.position = c(0.85, 0.2),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.text.align = 1,
          legend.title.align = 0.2,
          axis.text.y = element_text(size = 10, color = 'black')))

ggsave(
  './Plots/box.pdf',
  plot = box,
  width = 12,
  height = 7,
  units = c("cm"),
  dpi = 300
)


# rename panels
sex <- c(
  'F'="Female",
  'M'="Male")

predLabels <- 
  c(
    'No' = 'Without predator',
    'Yes' = 'With predator'
  )


(reg <- 
    ggplot(data = FishLakes, 
           aes(x = Len_cm, y = Head_Area, color = Sex)) +
    geom_smooth(aes(group = Pop), # add regressions for each population (LMM - think random factors)
                method = 'lm',
                se = F,
                size = 0.25,
                linetype = 'solid') +
    geom_smooth(aes(fill = Sex, # add general regression
                    color = after_scale(darken(fill, 0.2))), 
                method = 'lm',
                se = T,
                size = 1,
                linetype = 'solid') +
    geom_point(size = 0.5,
               alpha = 0.3,
               stroke = 0) +
    geom_rug(size = 0.25) + # add position of each observation in the margin
    labs(x = "Length", 
         y = "Head_Area") +
    scale_fill_manual(labels = c('Female', 'Male'),
                      values = c('#F2721D', '#009DA6')) +
    scale_color_manual(labels = c('Female', 'Male'),
                       values = c('#F2721D', '#009DA6')) +
        labs(x = 'Length of mosquitofish (cm)',
         y = 'Head area (rad)',
         tag = 'b)') +
    facet_grid(Sex ~ PredPresence,
               labeller = labeller(Sex = sex, PredPresence = predLabels)) + # use custom labels
    theme(legend.position = 'none',
          strip.background = element_rect(fill = 'transparent',
                                          color = 'transparent'),
          strip.text = element_text(hjust = 0.2, size = 10, face = 'bold')))

# that's how I can get the darker colors from the previous plot: darken(c('#F2721D', '#009DA6'), 0.2)
  
# always use ggsave to save plots, does not mess with sizes

ggsave(
  './Plots/reg.pdf',
  plot = reg,
  width = 12,
  height = 8,
  units = c("cm"),
  dpi = 300
)



# BONUS -------------------------------------------------------------------

# using the package patchwork, we can make panels right in gg
# https://patchwork.data-imaginist.com/articles/patchwork.html

  box / reg +
  plot_annotation(tag_levels = 'a',
                  tag_suffix = '.') + # one can actually tag right as we are making panels
  plot_layout(heights = c(1, 2))


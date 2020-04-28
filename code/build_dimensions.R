library(ggplot2)
library(reshape)
library(tidyr)
library(GGally)
library(directlabels)
library(extrafont)
library(patchwork)
library(rlist)
library(ff)
library(data.table)
library(plyr)
library(pdist)

setwd('~/RedditEmbeddingAnalysis')
#######################################################################
#### THIS PART IS MOSTLY FROM KOZLOWSKI ET AL. (2019) ####
# https://github.com/KnowledgeLab/GeometryofCulture/tree/master/code
survey<-read.csv(file='survey_data/survey_means_weighted.csv',
                 header=TRUE,row.names=1)

#####DEFINE FUNCTIONS##########
#Calculate norm (MAGNITUDE or LENGTH) of vector#
norm_vec <- function(x) sqrt(sum(x^2))

#Dot product#
dot <- function(x,y) (sum(x*y))

#Cosine Similarity#
cos <- function(x,y) dot(x,y)/norm_vec(x)/norm_vec(y)

#Normalize vector: 
nrm <- function(x) x/norm_vec(x)

#Calculate semantic dimension from antonym pair#
dimension<-function(x,y) nrm(nrm(x)-nrm(y))

###IMPORT LISTS OF TERMS TO PROJECT AND ANTONYM PAIRS#####
ant_pairs_aff <- read.csv("data/affluence_pairs.csv",header=FALSE, stringsAsFactor=F)
ant_pairs_gen <- read.csv("data/gender_pairs.csv",header=FALSE, stringsAsFactor=F)
ant_pairs_race <- read.csv("data/race_pairs.csv",header=FALSE, stringsAsFactor=F)

ant_pairs_edu <- read.csv("data/education_pairs.csv",header=FALSE, stringsAsFactor=F)
ant_pairs_cult <- read.csv("data/cultivation_pairs.csv",header=FALSE, stringsAsFactor=F)
ant_pairs_moral <- read.csv("data/morality_pairs.csv",header=FALSE, stringsAsFactor=F)
ant_pairs_empl <- read.csv("data/employment_pairs.csv",header=FALSE, stringsAsFactor=F)
ant_pairs_stat <- read.csv("data/status_pairs.csv",header=FALSE, stringsAsFactor=F)

###SETUP "make_dim" FUNCTION, INPUT EMBEDDING AND ANTONYM PAIR LIST#######
###OUTPUT AVERAGE SEMANTIC DIMENSION###

make_dim <- function(embedding,pairs){
  word_dims <- data.frame(matrix(NA,nrow(pairs),300))
  for (j in 1:nrow(pairs)){
    rp_word1<-pairs[j,1]
    rp_word2<-pairs[j,2]
  tryCatch(word_dims[j,] <- dimension(embedding[rp_word1,],
                                      embedding[rp_word2,]),
           error=function(e){})
  }
  dim_ave <- colMeans(word_dims, na.rm = TRUE)
  dim_ave_n <- nrm(dim_ave)
  return(dim_ave_n)
}

########################################################################

#### MY FUNCTIONS ####

# Read vectors and vocabulary to create model matrix
file2model <- function(dir_vec,dir_voc){
  model <- fread(dir_vec, sep=' ', header=FALSE)
  vocab <- fread(dir_voc, header=FALSE)
  model <- as.data.frame(model)
  rownames(model) <- vocab$V1
  return(model)
}

# Turn model into normalized matrix
model2matrix <- function(model){
  cdfm <- as.matrix(data.frame(model))
  cdfmn <- t(apply(cdfm,1,nrm))
  return(cdfmn)
}

# Construct all dimensions
cons_dim <- function(model){
  
  aff_dim <- make_dim(model,ant_pairs_aff)
  gender_dim <- make_dim(model,ant_pairs_gen)
  race_dim <- make_dim(model,ant_pairs_race)
  
  edu_dim <- make_dim(model,ant_pairs_edu)
  cult_dim <- make_dim(model,ant_pairs_cult)
  empl_dim <- make_dim(model,ant_pairs_empl)
  stat_dim <- make_dim(model,ant_pairs_stat)
  moral_dim <- make_dim(model,ant_pairs_moral)
  
  dim_df <- rbind(aff_dim,gender_dim,race_dim,edu_dim,
                  cult_dim,empl_dim,stat_dim,moral_dim)
  rownames(dim_df) <- c("aff_dim","gender_dim","race_dim","edu_dim",
                        "cult_dim","empl_dim","stat_dim","moral_dim")
  
  return(dim_df)
}

# Calculate angles of all dimensions
angle_dim <- function(dim_df){
  df <- matrix(, nrow = 8, ncol = 8)
  colnames(df) <- c("Affluence","Gender","Race","Education",
                    "Cultivation","Employment","Status","Morality")
  rownames(df) <- colnames(df)
  
  for (row in 1:nrow(dim_df)){
    for (row2 in 1:nrow(dim_df)){
      df[row,row2] <- cos(dim_df[row,],dim_df[row2,])
    }
  }
  return(df)
}

# Visualize angles as correlation matrix
angle_vis <- function(angles,title){
  "http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-
  heatmap-r-software-and-data-visualization"
  
  # abbreviations for readability
  colnames(angles) <- c("A", "G", "R", "Ed", "C", "E", "S", "M")
  rownames(angles) <- c("A", "G", "R", "Ed", "C", "E", "S", "M")
  
  angles <- melt(angles, na.rm = TRUE)
  colnames(angles) <- c('X1', 'X2', 'value')

  # reorder
  angles$X1 <- factor(angles$X1, c("A", "G", "R",
                                   "Ed", "C", "E",
                                   "S", "M"))
  angles$X2 <- factor(angles$X2, c("M", "S", "E",
                                   "C", "Ed", "R",
                                   "G", "A"))
  
  # Heatmap
  ggplot(data = angles, aes(X1, X2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2))) +
    ggtitle(title) +
    theme_minimal() +
    # set coloring scale and legend 
    scale_fill_gradient2(low = "blue", 
                         high = "red", 
                         mid = "white", 
                         midpoint = 0, 
                         limit = c(-0.7,0.7), 
                         space = "Lab", 
                         name="Cosine\nSimilarity",
                         breaks = c(-.6,-.5,-.4,-.3,-.2,-.1,
                                    0,.1,.2,.3,.4,.5,.6),
                         labels = c('','-.5','','','','',
                                    '0','','','','','.5','')
                         ) +
    # set theme
    theme(axis.text.y = element_text(size = 10, 
                                     hjust = 1),
          axis.text.x = element_text(vjust = 0, 
                                     size = 10, 
                                     hjust = 0.45),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position="none",
          plot.title = element_text(margin = margin(t=20,b=10),
                                    hjust=0.5,
                                    family='Gill Sans MT',
                                    size=18)) +
    scale_x_discrete(position = "top") + 
    coord_fixed()
}

#### RESULTS ####
# example: AskReddit

# read model
subreddit <- file2model('models/syn0_ngf_askreddit.txt',
                      'models/vocab_list_ngf_askreddit.txt')

# create matrix
subreddit_matrix <- model2matrix(subreddit)

# create dimensions
subreddit_dim <- cons_dim(subreddit)

# calculate all angles
subreddit_angles <- angle_dim(subreddit_dim)

# visualize as heatmap
angle_vis(subreddit_angles,'AskReddit')

########################################################################


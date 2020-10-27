#import data
library(readr)

chocolates <- 
  read_csv(
    "https://dqlab-dataset.s3-ap-southeast-1.amazonaws.com/chocolates.csv",
    col_types = cols(
      panelist = col_factor(),
      session = col_factor(),
      rank = col_factor(),
      product = col_factor(levels = paste0("choc", 1:6)),
      .default = col_integer()
    )
  )
View(chocolates)
colnames(chocolates)
str(chocolates)

install.packages("skimr",repos = "http://cran.us.r-project.org")
library(skimr)
skim(chocolates)

library(dplyr)

chocolates %>% 
  summarise(
    sample = toString(levels(product)),
    n_sample = n_distinct(product),
    n_panelist = n_distinct(panelist)
  )
View(chocolates)
n_sample <- 6
n_panelist <- 29

ncol(chocolates) - 4
atribut_sensoris <- colnames(chocolates[-c(1, 2, 3, 4)])
atribut_sensoris

chocolates %>% 
  select(atribut_sensoris) %>% 
  skim_without_charts()

batas_bawah <- 0
batas_atas <- 10

#ANOVA
model_bitterness <- aov(bitterness ~ product + panelist + session + panelist:product + panelist:session + product:session + rank, data = chocolates)

model_bitterness

anova(model_bitterness)
summary.lm(model_bitterness)


library(FactoMineR)

res_bitterness <- AovSum(model_bitterness)

anova(model_bitterness)

res_bitterness$Ftest
res_bitterness$Ttest[1:7, 1:2]
c("choc1", "choc4", "choc2","choc5", "choc6", "choc3")

install.packages('agricolae', repos="http://cran.rstudio.com/")
library(agricolae)

posthoc_bitterness <- HSD.test(model_bitterness, trt = "product")
posthoc_bitterness$groups
plot.group(posthoc_bitterness, variation = "SE")

#visualization of correlation between attribute sensory
install.packages('corrplot')
library(corrplot)

chocolates2 <- chocolates %>%
  select(atribut_sensoris)%>%
  cor()%>%
corrplot(
  type='upper',
  method='ellipse',
  diag=FALSE,
  addgrid.col=FALSE,
  order='FPC',
  tl.col='gray',
  tl.srt=30,
  title='Correlation'
)
?corrplot

library(readRDS)
?readRDS
atribut_sensoris2 <- colnames(chocolates[-c(1,2,3)])
chocolates_adjmean<- chocolates %>%
  group_by(product)%>%
  summarise_each(funs(mean),atribut_sensoris)
chocolates_adjmean
dim(chocolates_adjmean)

#Modelling PCA
library(FactoMineR)
chocolates_pca <- PCA(chocolates_adjmean[,-1],graph = FALSE)
chocolates_pca$eig

install.packages('factoextra')
library(factoextra)
fviz_eig(chocolates_pca,choice = 'eigenvalue',addlabels = TRUE)
fviz_eig(chocolates_pca,choice = 'variance',addlabels = TRUE)

fviz_pca_ind(chocolates_pca,repel = TRUE)

choc2 <- FALSE
choc3 <- TRUE
choc4 <- FALSE
choc5 <- FALSE
choc6 <- TRUE

fviz_pca_var(chocolates_pca,repel = TRUE)

#biplot
fviz_pca_biplot(chocolates_pca,repel = TRUE)

install.packages('sensehubr')

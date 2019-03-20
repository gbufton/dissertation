library(OpenMx)
library(tidyverse)
library(magrittr)
library(RCurl)


x <- getURL("https://raw.githubusercontent.com/gbufton/dissertation/master/path_data.csv")
data <- read.csv(text = x)

manifests <- c("Tech_LoA_Avg2","Tech_PEU_Avg", "Tech_PU_Avg", "Tech_Rel_Avg", 
               "Tech_Fam_Avg", "Tech_Skill_Avg")

Person_data <- data %>%
  mutate(Tech_LoA_Avg2 = 10 - Tech_LoA_Avg) %>%
  select(ID,manifests) %>%
  distinct(ID,.keep_all = TRUE) %>%
  ungroup()

personModel <- mxModel(
  model = "person", type="RAM",
  mxData(Person_data, 'raw', primaryKey = "ID"),
  manifestVars = manifests, ## x
  latentVars = c('Agency_IO_PEAvg', 'Agency_Control_PEAvg'), ## mb
  mxPath('one',manifests), # means
  mxPath(manifests, arrows=2, values=diag(cov(Person_data[,2:7]))), # variances
  mxPath(c(manifests, 'Agency_IO_PEAvg'), ## x & mb --> yb
         "Agency_Control_PEAvg", values = 1),
  mxPath(manifests,"Agency_IO_PEAvg", values = 1)) ## x --> mb


Within_data <- data %>%
  select(ID, PE_Agency_Control, PE_Agency_IO)

withinModel <- mxModel(
  model = "within", type="RAM", personModel,
  mxData(Within_data, 'raw'),
  manifestVars = c('PE_Agency_Control', 'PE_Agency_IO'), ## m & y
  mxPath("one", c('PE_Agency_Control', 'PE_Agency_IO')), ##manifest means
  mxPath(c('PE_Agency_Control', 'PE_Agency_IO'), arrows = 2), ## manifest residuals
  mxPath("PE_Agency_IO","PE_Agency_Control"), ## m --> y
  mxPath('person.Agency_IO_PEAvg', 'PE_Agency_IO', values=1, free=FALSE, joinKey = "ID"), ## mb --> m
  mxPath('person.Agency_Control_PEAvg', 'PE_Agency_Control', values=1, free=FALSE, joinKey = "ID")  ##yb --> y
)

m1 <- mxRun(withinModel, intervals = TRUE)
OpenMx:::summary.MxModel(m1, refModels = mxRefModels(m1, run = TRUE))
mxCheckIdentification(m1, details = TRUE)



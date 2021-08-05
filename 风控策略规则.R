library(pacman)
library(tidyverse)
library(funModeling)
library(Hmisc)
library(DataExplorer)
library(dplyr)
library(sqldf)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
library(partykit)
library(smbinning)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
p_load(scorecard)


new_customers <- read.delim("~/Desktop/数亿惠/RdataAnylst/20210624/new_customers.txt")

old_customers <- new_customers[, -1]

old_customers[is.na(old_customers)] <- -999

old_customers <- sqldf("select * from old_customers where target <> 2")

old_customers$target <- as.factor(old_customers$target)

table(old_customers$target)

bins_tree <- woebin(old_customers, y = "target", method = "tree")

tree_woe<-data.table::rbindlist(bins_tree)


bins_chi <- woebin(old_customers, y = "target", method = "chimerge", stop_limit = 0.5)

chi_woe<-data.table::rbindlist(bins_chi)

dt_sel = var_filter(old_customers, y = "target",iv_limit = 0.05, missing_limit = 0.9,
                    identical_limit = 0.8)

names(dt_sel)

dt_sel$target <- as.factor(dt_sel$target)

control <- rpart.control(xval = 10, minsplit = 10, minbucket = 30, maxdepth = 30)

#control <- rpart.control(xval = 10)

dtree <- rpart(target ~ ., data = dt_sel, method = "class", parms = list(split = "information"), control = control)

summary(dtree)
dtree$variable.importance
printcp(dtree)
plotcp(dtree, lwd = 2)

library(rpart.plot)
rpart.plot(dtree,
           branch = 1, shadow.col = "gray",
           box.col = "green", border.col = "blue",
           split.col = "red", split.cex = 1.2, main = "决策树"
)

asRules(dtree)



library(C50)

ls('package:C50')


tc <- C5.0Control(
  subset = F,
  CF=0.25,
  winnow = F,
  noGlobalPruning = F,
  minCases = 20
)

model2 <- C5.0(target~.,data = dt_sel,rules = T,contrl = tc)

summary(model2)
library(tree)
library(partykit)
#plot(model2)
#C5imp(model2)



#+
knitr::opts_chunk$set(warning = FALSE, message = FALSE, echo = FALSE)
#' ---
#' title: "Exploring Instacart Dataset with PCA"
#' author: "Notesofdabbler"
#' date: "2017-05-22"
#' ---
#'
#' Recently, [Instacart](https://www.instacart.com/) released a [dataset](https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2) 
#' of ~3 million orders made by ~200,000 users at different days of week and times of day. There is also an ongoing [Kaggle competition](https://www.kaggle.com/c/instacart-market-basket-analysis) 
#' to predict which products a user will buy again. My goal here is more modest where I just wanted to explore the dataset to find patterns of
#' purchasing behaviour by hour of day, day of week and number of days prior to current order. An [example](https://cdn-images-1.medium.com/max/800/1*wKfV6OV-_1Ipwrl7AjjSuw.png)
#' of this kind of analysis is also shown in their blog. Here I wanted to explore if I can find such kind of patters by using 
#' the very common and popular dimension reduction technique - Principal Component Analysis (PCA). There are several great
#' resources that introduce PCA if you are not familiar with PCA. One of the resources is the set of 
#' [video lectures](https://www.r-bloggers.com/in-depth-introduction-to-machine-learning-in-15-hours-of-expert-videos/) on 
#' machine learning by Prof. Hastie and Prof. Tibshirani.
#' 
#' The general approach that I have followed is:
#' 
#' *  Do principal component analysis on the data (each row is a product, each column is a time period 
#' (hour of day, day of week or number of days prior to current order))
#' *  Review the loading plots of first two principal components to see purchase patters
#' *  Identify top 20 products that have high scores in either first or the second principal component
#' *  Check the purchasing pattern by checking the average number of orders for the products that were identified 
#' as having top scores in one of the principal components. 
#' 
#' **Spoiler Alert**: Since my analysis is basic, don't be disappointed if there are no big Aha moments (there will be none). But I think it is still 
#' fun to see how we can extract such information directly from data.
#' 
#' We first load the libraries needed for the analysis
#+
# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# source functions that are used for PCA analysis, extracting key information from PCA results
source("pca_fns.R")

#'
#'  I downloaded the data from the following [link](https://www.instacart.com/datasets/grocery-shopping-2017). The
#'  data dictionary is in the following [link](https://gist.github.com/jeremystan/c3b39d947d9b88b3ccff3147dbcf6c6b). Since
#'  the datasets are big, they are stored in tmpdata folder and not currently in the git repository. So somebody trying 
#'  to reproduce this code will need to manually download the data and extract it to tmpdata subdirectory to be able
#'  to run this code.
#'

#+ cache = TRUE
# load datasets
aisles = read_csv("tmpdata/aisles.csv")
depts = read_csv("tmpdata/departments.csv")
prods = read_csv("tmpdata/products.csv")

orders = read_csv("tmpdata/orders.csv")
orders$order_hour_of_day = as.numeric(orders$order_hour_of_day)

ordprd_prior = read_csv("tmpdata/order_products__prior.csv")
ordprd_train = read_csv("tmpdata/order_products__train.csv")

# stack order-product datasets (prior and train) together
ordprd = bind_rows(ordprd_prior, ordprd_train)

# Basic Data Info
#

# number of users
length(unique(orders$user_id))
# number of orders
nrow(orders)

#' Below are some basic info on the datasets
#' 
#' * The number of users are ~200,000. 
#' * The number of orders are ~3.4M.

#' Next, we find products that account for top 80% of orders. We will work with this product set through
#' the rest of the analysis

#+
# Find products that account for top 80% of total orders
prod_orders = ordprd %>% group_by(product_id) %>% summarize(numorders = n())
prod_orders = inner_join(prod_orders, prods, by = "product_id")
prod_orders = prod_orders %>% arrange(desc(numorders)) %>% mutate(pctorders = cumsum(numorders)/sum(numorders)*100)
prod_top80pct = prod_orders %>% filter(prod_orders$pctorders <= 80)

#' The number of products are ~50K or which ~5K account for 80% of total orders
#'
#'## PCA to find patterns of purchase by hour of day

#+
# orders by day of week/hour of day
wkhr_orders = orders %>% group_by(order_dow, order_hour_of_day) %>% summarize(numorders = n())

# Plot of orders by hour of day for each week of day
ggplot(data = wkhr_orders) + 
  geom_line(aes(x = order_hour_of_day, y = numorders, color = factor(order_dow))) + 
  xlab("Hour of Day") + ylab("# Orders") + scale_color_discrete(name = "Day of Week") + 
  theme_bw() 

#' It is not clear whether day 0 is Saturday or Sunday. Since the number of orders is higher
#' for days 0 and 1, maybe 0 and 1 are Saturday and Sunday. But certain other analysis further
#' down seem to indicate that 0 is Sunday.

#+
# create a dataset that calculates for each product, the percentage of product orders at each hour of day
ordprd2 = inner_join(ordprd, orders[,c("order_id","order_hour_of_day")], by = "order_id")
ordprd2a = ordprd2 %>% group_by(product_id, order_hour_of_day) %>% 
            summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd2a = inner_join(ordprd2a, prod_top80pct[,c("product_id"), drop = FALSE], by = "product_id")

#' Find products with different patterns of purchase timing by hour of day with PCA.
#' Dataset for PCA has for each product (rows), the percentage of product orders at each hour of day (column)
#+
ordprd2a_s = ordprd2a %>% select(-numorders) %>% spread(order_hour_of_day, pctorders, fill = 0)

#' Here I am plotting the average purchasing pattern by hour of day for a sample product item 
#' (chocolate sandwich cookies).
#' 
#+ 
# Plot a row (product_id - 1, chocolate sandwich cookies)
ordprd2a_sample = data.frame(hour_of_day = names(ordprd2a_s)[-1], pctorders = t(ordprd2a_s[ordprd2a_s$product_id == 24852,-1]))
ggplot() + geom_line(data = ordprd2a_sample, aes(x = as.numeric(hour_of_day), y = pctorders)) + theme_bw()

#' Next I will do a PCA on this data. Since all the data is in percentages, I didn't do any
#' further scaling of data

#+
# Perform a PCA (rows - products, variables - hour of day)
pcdata = ordprd2a_s
pc_mdl = prcomp(x = pcdata[,-1], center = TRUE, scale. = FALSE)

# Get the plot of cumulative variance explained by each principal component
pc_mdl_var = cumsum(pc_mdl$sdev^2)/sum(pc_mdl$sdev^2)
pc_mdl_vardf = data.frame(pc = 1:length(pc_mdl_var), cumvar = pc_mdl_var)
p_cumvar = ggplot() + 
           geom_line(data = pc_mdl_vardf, aes(x = pc, y = cumvar)) + 
           geom_point(data = pc_mdl_vardf, aes(x = pc, y = cumvar)) + 
           xlab("PC") + ylab("Cumulative Variance") + 
           scale_x_continuous(breaks = 1:length(pc_mdl_var)) + 
           scale_y_continuous(labels = percent, breaks = seq(0,1,by=0.1)) + expand_limits(y = 0) + 
           theme_bw()
p_cumvar

#' The plot of cumulative variance shows that first component accounts for 44% of variance,
#' first two account for 58% and first 3 account for 67% of variance. Next, we will look at the
#' first two loadings since first 2 components account for 58% of variance.

#+
# Get loadings plot for 2 principal components
numcomp = 2

pc_loadings = data.frame(pc_mdl$rotation)
pc_loadings$hour_of_day = seq(0,23)
pc_loadings_g = pc_loadings %>% gather(pc, pcload, -hour_of_day)

pcfilt = paste0("PC",seq(1,numcomp))
pc_loadings_g = pc_loadings_g[pc_loadings_g$pc %in% pcfilt,]

p_pcload = ggplot() + geom_line(data = pc_loadings_g, aes(x = hour_of_day, y = pcload, color = pc)) +
             theme_bw()
p_pcload

#' First principal component loading PC1 indicates a pattern of either higher percentage of purcahses
#' in the morning or evening. The second principal component loading indicates a pattern where
#' there is higher purchase around 11am and 4pm. To check which product items follow these patterns,
#' we look at products that either have high scores or low scores on a principal component. So here
#' we take the top 20 and bottom 20 products in terms of their scores on PC1. The actual pattern still
#' may not quite match the loading plot since the overall pattern is a combination of all principal 
#' component loadings. 

#+
# Scatter plot of PC1 and PC2 scores
pc_scores = data.frame(pc_mdl$x)
pc_scores$product_id = pcdata$product_id
pc_scores = inner_join(pc_scores, prods[,c("product_id", "product_name")], by = "product_id")

# sort products based on PC1 scores
pc_scores_pick = pc_scores %>% arrange(desc(PC1))

# get top and bottom 20 products based on the scores
prod_top20 = pc_scores_pick$product_name[1:20]
prod_bot20 = pc_scores_pick$product_name[(nrow(pc_scores_pick) - 19):nrow(pc_scores_pick)]

# plot hourly purchase profile for a list of products that are in top and bottom 20 scores of PC1
prodlist_topbot20 = data.frame(top_scores = prod_top20, bottom_scores = prod_bot20)
prodlist = c(prod_top20, rev(prod_bot20))
prodlist_label = c(rep("top 20 scores", 20), rep("bottom 20 scores", 20))
scoreid = c(seq(1,20), seq(1,20))
prodlistdf = data.frame(product_name = prodlist, prodlist_label, scoreid, stringsAsFactors = FALSE)
prodlistdf = inner_join(prodlistdf, prods[, c("product_id", "product_name")], by = "product_name")

prodlist_profile = inner_join(ordprd2a_s, prodlistdf[, c("product_id", "product_name", "prodlist_label")], by = "product_id")

prodlist_profile_g = prodlist_profile %>% gather(hour_of_day, pctorders, -product_name, -product_id, -prodlist_label)

ggplot() + 
  geom_line(data = prodlist_profile_g, aes(x = as.numeric(hour_of_day), y = pctorders, 
                                           group = product_name, color = prodlist_label)) + 
         xlab("Hour of Day") + ylab("% orders") + 
         scale_x_continuous(breaks = seq(0,23)) + scale_y_continuous(labels = percent) + 
         theme_bw() + theme(legend.title = element_blank())

#' Below is the table that lists the actual products that are in top and bottom scores of PC1. 
#' Ice cream purchases tend to occur more in the evening. Items like granola bars, krispie treats, 
#' apples are purchased more in the morning.
#' 
#+
prodlist_topbot20

#' ## PCA to find patterns of purchase by day of week
#' Dataset for PCA has for each product (rows), the percentage of product orders at each day of week (column)
#+
ordprd3 = inner_join(ordprd, orders[, c("order_id", "order_dow")], by = "order_id")

ordprd3a = ordprd3 %>% group_by(product_id, order_dow) %>% 
  summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd3a = inner_join(ordprd3a, prod_top80pct[,c("product_id"), drop = FALSE], by = "product_id")

#' The different analysis for PCA done for the case of purchase by hour of day have been wrapped 
#' into 3 functions that are defined in `pca_funs.R` (code)[pca_funs.R]
#+
ordprd3a_s = ordprd3a %>% select(-numorders) %>% spread(order_dow, pctorders, fill = 0)

# perform PCA and plot cumulative variance
pcmdl_dow = getpca(pcdata = ordprd3a_s)
pcmdl_dow$p_cumvar

#' In this case, the first two principal components account for ~90% of variance. The loadings
#' for first two components are plotted next.
#+
pcplots_dow = get_pcaplots(pcmdl_dow, numcomp = 2, ordprd3a_s, ordprd3a_s, prods, seq(0,6), "day of week")
pcplots_dow$p_pcload

#' The products with high scores on first component would tend to be purchased more at the ends of 
#' the week and products with low scores on first component would tend to be purchased less at the ends 
#' of the week. The plot of products having top and bottom 20 scores on PC1 is shown next.
#+
pcplots_dow$p_items[[1]]
#' The table below shows the names of products with top and bottom 20 scores on PC1. It looks like
#' cookies and chocolates are bought less in weekends compared to cooking items.
#+
pcplots_dow$prodlist_topbot20[[1]]
#' The products with high scores on second component would tend to be purchased more at the end of 
#' the week and products with low scores on first component would tend to be purchased more at the beginning 
#' of the week. The plot of products having top and bottom 20 scores on PC2 is shown next.
#+
pcplots_dow$p_items[[2]]
#' Looks like some wine and vodka is purchased more as the week progresses.
pcplots_dow$prodlist_topbot20[[2]]

#' ## PCA to find patterns of purchase by days since prior order

#' Dataset for PCA has for each product (rows), the percentage of product orders at each of days since prior order (column)
#+
ordprd4 = inner_join(ordprd, orders[!is.na(orders$days_since_prior_order), c("order_id", "days_since_prior_order")], by = "order_id")

ordprd4a = ordprd4 %>% group_by(product_id, days_since_prior_order) %>% 
  summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd4a = inner_join(ordprd4a, prod_top80pct[,c("product_id"), drop = FALSE], by = "product_id")

# PCA of purchase timing (days since prior order)

ordprd4a_s = ordprd4a %>% select(-numorders) %>% spread(days_since_prior_order, pctorders, fill = 0)

# Do a PCA
pcmdl_prior = getpca(pcdata = ordprd4a_s)
pcmdl_prior$p_cumvar

#' First two principal components account for 75% of variance. The loadings for the first two
#' components is shown next.

#+
pcplots_prior = get_pcaplots(pcmdl_prior, numcomp = 2, ordprd4a_s, ordprd4a_s, prods, seq(0, 30), "days since prior order")
pcplots_prior$p_pcload

#' Products with high scores in first principal component will tend to be purchased more in 
#' orders that are less than a week from previous order and products with low scores on the first
#' principal component will tend to be purchased in orders that are more than a month after the previous
#' order
pcplots_prior$p_items[[1]]
#' Items like trash bag, laundary detergents, towels are in orders that are made less often (longer than once per month)
#' whereas the food/drink related items are in orders that are made more frequently (less than a week)
pcplots_prior$prodlist_topbot20[[1]]

#' Products with high scores in principal component 2 will tend to have higher percentage of orders
#' with days since previous order of less than a week and products with low scores in principal
#' component 2 will tend to have a higher percentage of orders that are weekly in frequency.
#+
pcplots_prior$p_items[[2]]
#' Juices, milk and wine tend to be in orders who frequency is less than a week. Items like Greek Yogurt,
#' Apples and Granola bar are in orders who frequency is weekly.
#+
pcplots_prior$prodlist_topbot20[[2]]
#'
#'## Session Info
#+
sessionInfo()

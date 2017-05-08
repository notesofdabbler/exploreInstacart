
# load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

#
#  Data is downloaded from the following [link](https://www.instacart.com/datasets/grocery-shopping-2017). The
#  data dictionary is in the following [link](https://gist.github.com/jeremystan/c3b39d947d9b88b3ccff3147dbcf6c6b). Since
#  the datasets are big, they are stored in tmpdata folder and not currently in the git repository. So somebody trying 
#  to reproduce this code will need to manually download the data and extract it to tmpdata subdirectory to be able
#  to run this code.
#

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

#
# * The number of users are ~200,000. 
# * The number of orders are ~3.4M.



# Find products that account for top 80% of total orders
prod_orders = ordprd %>% group_by(product_id) %>% summarize(numorders = n())
prod_orders = inner_join(prod_orders, prods, by = "product_id")
prod_orders = prod_orders %>% arrange(desc(numorders)) %>% mutate(pctorders = cumsum(numorders)/sum(numorders)*100)
prod_top80pct = prod_orders %>% filter(prod_orders$pctorders <= 80)

# The number of products are ~50K or which ~5K account for 80% of total orders

# orders by day of week/hour of day
wkhr_orders = orders %>% group_by(order_dow, order_hour_of_day) %>% summarize(numorders = n())

# Plot of orders by hour of day for each week of day
ggplot(data = wkhr_orders) + 
  geom_line(aes(x = order_hour_of_day, y = numorders, color = factor(order_dow))) + 
  xlab("Hour of Day") + ylab("# Orders") + scale_color_discrete(name = "Day of Week") + 
  theme_bw() 

# Explore products ordered by hour of day

# create a dataset that calculates for each product, the percentage of product orders at each hour of day
ordprd2 = inner_join(ordprd, orders[,c("order_id","order_hour_of_day")], by = "order_id")
ordprd2a = ordprd2 %>% group_by(product_id, order_hour_of_day) %>% 
            summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd2a = inner_join(ordprd2a, prod_top80pct[,c("product_id"), drop = FALSE], by = "product_id")

# Find products with different patterns of purchase timing by hour of day with PCA
# Dataset for PCA has for each product (rows), the percentage of product orders at each hour of day (column)

ordprd2b = data.frame(ordprd2a %>% ungroup()) 
ordprd2a_s = ordprd2a %>% select(-numorders) %>% spread(order_hour_of_day, pctorders, fill = 0)

# Plot a row (product_id - 1, chocolate sandwich cookies)
ordprd2a_sample = data.frame(hour_of_day = names(ordprd2a_s)[-1], pctorders = t(ordprd2a_s[ordprd2a_s$product_id == 24852,-1]))
ggplot() + geom_line(data = ordprd2a_sample, aes(x = as.numeric(hour_of_day), y = pctorders)) + theme_bw()

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

# Get loadings plot for 2 principal components
numcomp = 4

pc_loadings = data.frame(pc_mdl$rotation)
pc_loadings$hour_of_day = seq(0,23)
pc_loadings_g = pc_loadings %>% gather(pc, pcload, -hour_of_day)

pcfilt = paste0("PC",seq(1,numcomp))
pc_loadings_g = pc_loadings_g[pc_loadings_g$pc %in% pcfilt,]

p_pcload = ggplot() + geom_line(data = pc_loadings_g, aes(x = hour_of_day, y = pcload, color = pc)) +
             theme_bw()
p_pcload

pc_scores = data.frame(pc_mdl$x)
pc_scores$product_id = pcdata$product_id
pc_scores = inner_join(pc_scores, prods[,c("product_id", "product_name")], by = "product_id")

pc_scores_pick = pc_scores %>% arrange(desc(PC3))
prod_top20 = pc_scores_pick$product_name[1:20]
prod_bot20 = pc_scores_pick$product_name[(nrow(pc_scores_pick) - 19):nrow(pc_scores_pick)]

# plot hourly purchase profile for a list of products

prodlist = prod_bot20
prodlistdf = prods[prods$product_name %in% prodlist, c("product_id", "product_name")]

prodlist_profile = inner_join(ordprd2a_s, prodlistdf, by = "product_id")

prodlist_profile_g = prodlist_profile %>% gather(hour_of_day, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = prodlist_profile_g, aes(x = as.numeric(hour_of_day), y = pctorders, group = product_name)) +
     theme_bw()


# Top products ordered by week of day
ordprd3 = inner_join(ordprd, orders[, c("order_id", "order_dow")], by = "order_id")

ordprd3a = ordprd3 %>% group_by(product_id, order_dow) %>% 
  summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd3a = inner_join(ordprd3a, prodcnt80[,c("product_id"), drop = FALSE], by = "product_id")
ordprd3b = ordprd3a %>% group_by(order_dow) %>% arrange(desc(pctorders)) %>% 
  filter(row_number() <= 10) %>% arrange(order_dow)
ordprd3b = inner_join(ordprd3b, prods, by = "product_id")

# PCA of purchase timing (day of week)

ordprd3b = data.frame(ordprd3a %>% ungroup()) 
ordprd3bs = ordprd3b %>% select(-numorders) %>% spread(order_dow, pctorders, fill = 0)

tmp = prcomp(x = ordprd3bs[,-1])
plot(tmp)
str(tmp)
plot(tmp$rotation[,1], type = "l")
plot(tmp$rotation[,2], type = "l")
plot(tmp$rotation[,3], type = "l")
plot(tmp$rotation[,4], type = "l")

x1 = tmp$x[,2]
x1df = data.frame(product_id = ordprd3bs$product_id, x1 = x1)
x1df = inner_join(x1df, prods[,c("product_id", "product_name")], by = "product_id")
x1df = x1df %>% arrange(desc(x1))

chktop = inner_join(ordprd3bs, x1df[1:20,c("product_id", "product_name")], by = "product_id")

chktop_g = chktop %>% gather(wkday, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = chktop_g, aes(x = as.numeric(wkday), y = pctorders, group = product_name)) +
  theme_bw()

chkbot = inner_join(ordprd3bs, x1df[(nrow(x1df)-9):nrow(x1df),c("product_id", "product_name")], by = "product_id")

chkbot_g = chkbot %>% gather(wkday, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = chkbot_g, aes(x = as.numeric(wkday), y = pctorders, group = product_name)) +
  theme_bw()

# market basket

aisleord = inner_join(ordprd, prods[, c("product_id", "aisle_id")], by = "product_id")
aisleord2 = aisleord %>% group_by(order_id, aisle_id) %>% summarize(numorders = n())
aisleord3 = inner_join(aisleord2, orders[, c("order_id", "user_id")], by = "order_id")
aisleord4 = aisleord3 %>% group_by(user_id, aisle_id) %>% 
     summarize(numorders = sum(numorders)) %>% mutate(pctorders = numorders/sum(numorders))

aisleord4s = aisleord4 %>% select(-numorders) %>% spread(aisle_id, pctorders, fill = 0)

tmp = orders %>% group_by(user_id) %>% 
        summarize(numorders = n(), daysprior_md = median(days_since_prior_order, na.rm = TRUE)) 
tmp2 = tmp %>% group_by(numorders) %>% summarize(daysprior_mdavg = mean(daysprior_md))

ggplot(tmp2) + geom_point(aes(x = numorders, y = daysprior_mdavg)) + theme_bw()

# PCA users vs aisles
tmp = prcomp(aisleord4s[,-1])
plot(tmp)
tmpvar = cumsum(tmp$sdev^2)/sum(tmp$sdev^2)


tmpload = data.frame(tmp$rotation)
tmpload$aisle_id = as.numeric(names(aisleord4s[-1]))
tmpload = inner_join(tmpload, aisles, by = "aisle_id")
tmpload = tmpload[,c("aisle","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8")]
tmpload_g = tmpload %>% gather(pc, pcload, - aisle)
tmpload_g$abspcload = abs(tmpload_g$pcload)
tmpload_g = tmpload_g %>% group_by(pc) %>% arrange(desc(abspcload)) %>% 
                 filter(row_number() <= 3) %>% arrange(pc)

plot(tmp$rotation[,1], type = "l")
plot(tmp$rotation[,2], type = "l")
plot(tmp$rotation[,3], type = "l")
plot(tmp$rotation[,4], type = "l")


# Top products ordered by (days since prior order)
ordprd4 = inner_join(ordprd, orders[, c("order_id", "days_since_prior_order")], by = "order_id")

ordprd4a = ordprd4 %>% group_by(product_id, days_since_prior_order) %>% 
  summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd4a = inner_join(ordprd4a, prodcnt80[,c("product_id"), drop = FALSE], by = "product_id")

# PCA of purchase timing (days since prior order)

ordprd4b = data.frame(ordprd4a %>% ungroup()) 
ordprd4bs = ordprd4b %>% select(-numorders) %>% spread(days_since_prior_order, pctorders, fill = 0)

tmp = prcomp(x = ordprd4bs[,-1])
plot(tmp)
str(tmp)
plot(tmp$rotation[,1], type = "l")
plot(tmp$rotation[,2], type = "l")
plot(tmp$rotation[,3], type = "l")
plot(tmp$rotation[,4], type = "l")

x1 = tmp$x[,2]
x1df = data.frame(product_id = ordprd4bs$product_id, x1 = x1)
x1df = inner_join(x1df, prods[,c("product_id", "product_name")], by = "product_id")
x1df = x1df %>% arrange(desc(x1))

chktop = inner_join(ordprd4bs, x1df[1:20,c("product_id", "product_name")], by = "product_id")

chktop_g = chktop %>% gather(daysprior, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = chktop_g, aes(x = as.numeric(daysprior), y = pctorders, group = product_name)) +
  theme_bw()

chkbot = inner_join(ordprd4bs, x1df[(nrow(x1df)-9):nrow(x1df),c("product_id", "product_name")], by = "product_id")

chkbot_g = chkbot %>% gather(daysprior, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = chkbot_g, aes(x = as.numeric(daysprior), y = pctorders, group = product_name)) +
  theme_bw()

tmpdaysprior = orders %>% group_by(days_since_prior_order) %>% summarize(cnt = n())
ggplot(tmpdaysprior) + geom_line(aes(x = days_since_prior_order, y = cnt)) + theme_bw()

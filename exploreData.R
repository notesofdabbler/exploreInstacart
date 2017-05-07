
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

aisles = read.csv("instacart_2017_05_01/aisles.csv", stringsAsFactors = FALSE)
depts = read.csv("instacart_2017_05_01/departments.csv", stringsAsFactors = FALSE)
prods = read.csv("instacart_2017_05_01/products.csv", stringsAsFactors = FALSE)

orders = fread("instacart_2017_05_01/orders.csv")
ordprd_prior = fread("instacart_2017_05_01/order_products__prior.csv")
ordprd_train = fread("instacart_2017_05_01/order_products__train.csv")

ordprd = bind_rows(ordprd_prior, ordprd_train)

orders = data.frame(orders)
ordprd = data.frame(ordprd)

# Find top purchase products
prodcnt = ordprd %>% group_by(product_id) %>% summarize(numpurchase = n())
prodcnt = inner_join(prodcnt, prods, by = "product_id")
prodcnt = prodcnt %>% arrange(desc(numpurchase))
prodcnt$pctpurchase = cumsum(prodcnt$numpurchase)/sum(prodcnt$numpurchase)*100

prodcnt80 = prodcnt[prodcnt$pctpurchase <= 80,]

# Find top purchases aisles
aislecnt = prodcnt %>% group_by(aisle_id) %>% summarize(numpurchase = sum(numpurchase))
aislecnt = inner_join(aislecnt, aisles, by = "aisle_id") %>% arrange(desc(numpurchase))

# Find top purchase depts
deptcnt = prodcnt %>% group_by(department_id) %>% summarize(numpurchase = sum(numpurchase))
deptcnt = inner_join(deptcnt, depts, by = "department_id") %>% arrange(desc(numpurchase))

# orders by day of week/hour of day
wkhrcnt = orders %>% group_by(order_dow, order_hour_of_day) %>% summarize(numorders = n())

# Plot of orders by hour of day for each week of day
ggplot(data = wkhrcnt) + 
  geom_line(aes(x = order_hour_of_day, y = numorders, color = factor(order_dow))) + theme_bw()

# Top products ordered by hour of day
ordprd2 = inner_join(ordprd, orders[,c("order_id","order_hour_of_day")], by = "order_id")

ordprd2a = ordprd2 %>% group_by(product_id, order_hour_of_day) %>% 
            summarize(numorders = n()) %>% mutate(pctorders = numorders/sum(numorders))
ordprd2a = inner_join(ordprd2a, prodcnt80[,c("product_id"), drop = FALSE], by = "product_id")
ordprd2b = ordprd2a %>% group_by(order_hour_of_day) %>% arrange(desc(pctorders)) %>% 
      filter(row_number() <= 10) %>% arrange(order_hour_of_day)
ordprd2b = inner_join(ordprd2b, prods, by = "product_id")

# PCA of purchase timing (hour of day)

ordprd2b = data.frame(ordprd2a %>% ungroup()) 
ordprd2bs = ordprd2b %>% select(-numorders) %>% spread(order_hour_of_day, pctorders, fill = 0)

tmp = prcomp(x = ordprd2bs[,-1])
plot(tmp)
str(tmp)
plot(tmp$rotation[,1], type = "l")
plot(tmp$rotation[,2], type = "l")
plot(tmp$rotation[,3], type = "l")
plot(tmp$rotation[,4], type = "l")

tmp$var = tmp$sdev^2
tmp$varpct = cumsum(tmp$var)/sum(tmp$var)

x1 = tmp$x[,3]
x1df = data.frame(product_id = ordprd2bs$product_id, x1 = x1)
x1df = inner_join(x1df, prods[,c("product_id", "product_name")], by = "product_id")
x1df = x1df %>% arrange(desc(x1))

chktop = inner_join(ordprd2bs, x1df[1:10,c("product_id", "product_name")], by = "product_id")

chktop_g = chktop %>% gather(hrday, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = chktop_g, aes(x = as.numeric(hrday), y = pctorders, group = product_name)) +
     theme_bw()

chkbot = inner_join(ordprd2bs, x1df[(nrow(x1df)-9):nrow(x1df),c("product_id", "product_name")], by = "product_id")

chkbot_g = chkbot %>% gather(hrday, pctorders, -product_name, -product_id)

ggplot() + geom_line(data = chkbot_g, aes(x = as.numeric(hrday), y = pctorders, group = product_name)) +
  theme_bw()

scores = data.frame(s1 = tmp$x[,1], s2 = tmp$x[,2], s3 = tmp$x[,3], s4 = tmp$x[,4])

clus = kmeans(scores, 5)

scores$cluster = clus$cluster

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

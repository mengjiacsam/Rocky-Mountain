library(data.table)
library(ggplot2)

sales_data_clean <- fread("data/sales_data_for_clustering.csv")
aggr_quantity <- aggregate(x = sales_data_clean$quantity,
                           by = list(sales_data_clean$product_id),
                           FUN = sum)
aggr_price <- aggregate(x = sales_data_clean$price,
                        by = list(sales_data_clean$product_id),
                        FUN = sum)

aggr_sales <- data.table(aggr_quantity, aggr_price$x)

names(aggr_sales)[1] <- 'product_id'
names(aggr_sales)[2] <- 'agg_quantity'
names(aggr_sales)[3] <- 'agg_price'

aggr_sales <- cbind(aggr_sales, avg_price = aggr_price/aggr_quantity)
aggr_sales$avg_price.Group.1 <- NULL
names(aggr_sales)[4] <- 'avg_price'


ggplot(aggr_sales, aes(agg_quantity, avg_price)) + geom_point()

aggr_sales_clean <- aggr_sales[, c("agg_price", "product_id"):=NULL]
aggr_sales_clean[avg_price>4000, avg_price := NA]
aggr_sales_clean[agg_quantity>250, agg_quantity := NA]
aggr_sales_clean <- na.omit(aggr_sales_clean)

km_agg_sales  <- kmeans(aggr_sales_clean, centers = 3, nstart = 20)

ggplot(aggr_sales_clean, aes(x = agg_quantity, y = avg_price)) +
  geom_point(colour = (km_agg_sales$cluster + 3))

ks <- 1:8
tot_within_ss <- sapply(ks, function(k) {
  km_output <- kmeans(aggr_sales_clean, centers = k, nstart = 20)
  km_output$tot.withinss
})
tot_within_ss

#a teszt alapj�n az l�that�, hogy a teljes sum of squares �rt�k, melyet minimaliz�lni szeretn�nk,
# folyamatosan cs�kken a klaszterek n�veked�s�vel. ez valamelyest �rhet�, hiszen erre a kapcsolatra
# igaz�b�l egy hiperbolikus g�rbe illeszthet� lenne. �gy gondoljuk ennek ellen�re, hogy klaszterez�ssel
# el�g j�l l�that�k a k�l�nb�z� term�kek - min�l t�bbet vesznek egy term�kb�l, annak az �tlag�ra
# alacsonyabb lesz, m�g a magas �r� term�kekb�l jellemz�en nem vesznek t�bbet m�g aggreg�ltan sem.

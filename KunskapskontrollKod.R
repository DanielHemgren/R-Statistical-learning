install.packages("caret")
library(caret)
install.packages("pxweb")
library(pxweb)
library("readxl")
# Change path to your own
 file_path <- "C:/Users/Daniel/Documents/ec_utbildning/R/kunskapskontroll/Skarp1.xlsx"
 car_data <- read_excel(file_path)
View(car_data)
class(car_data)
head(car_data)
summary(car_data)
dim(car_data)
residuals <- resid(model)
summary(residuals)
summary(model)

###################4diagnostic#########################
par(mfrow = c(2, 2)) 
plot(model, which = 1)
plot(model, which = 2)
plot(model, which = 3)
plot(model, which = 4)
#####################kurtosis##########
install.packages("moments")
library(moments)
kurtosis_value <- kurtosis(residuals)
print(kurtosis_value)
##############data partition##############################################
X <- car_data[, c('hk', 'mil', 'ålder', 'automat', 'premium', 'el', 'Bildensitet')]
y <- car_data$pris
set.seed(42)
index_train <- createDataPartition(y, p = 0.6, list = FALSE)
index_validate_test <- createDataPartition(y[index_train], p = 0.5, list = FALSE)
X_train <- X[index_train, ]
y_train <- y[index_train]
X_validate <- X[index_validate_test, ]
y_validate <- y[index_validate_test]
X_test <- X[-index_train, ]
y_test <- y[-index_train]
#################resampling########################
train_control <- trainControl(method = "cv",number = 5)     
model <- train(pris ~ .,                       
               data = data.frame(pris = y_train, X_train),  
               method = "lm",                  
               trControl = train_control)       
print(model)
summary(model)
resampling_results <- model$resample
print(resampling_results)
summary(resampling_results)
predictions <- predict(model, newdata = data.frame(pris = y, X))
####################1000kr###############################
model <- lm(pris ~ ., data = data.frame(cbind(pris = y_test, X_test)))
fitted_values <- fitted(model)
fitted_values[fitted_values < 0] <- 1000
train_data <- data.frame(cbind(pris = y_train, X_train))
results <- data.frame(Index = rownames(train_data), Actual_Prices = y_train, Fitted_Values = fitted_values)
View(results)
################subset BIC#####################################
library(leaps)
subsets <- regsubsets(y ~ ., data = X, method = "forward", nvmax = 7)
bic_values <- summary(subsets)$bic
adj_r_squared <- summary(subsets)$adjr2
cp_values <- summary(subsets)$cp
summary_info <- summary(subsets)
bic_values <- summary_info$bic
adj_r_squared <- summary_info$adjr2
cp_values <- summary_info$cp
best_subset <- which.min(bic_values)
best_subset2 <- which.min(adj_r_squared)
best_subset3 <- which.min(cp_values)
best_subset_details <- summary(subsets)$outmat[best_subset,best_subset2,best_subset3 ]
print(bic_values)
print(best_subset_details)
summary(best_subset_details)
############################################################
predictions <- predict(model, newdata = data.frame(X_test))
mse <- mean((predictions - y_test)^2)
print(paste("Mean Sqrd Eror:", mse))
fitted_values <- fitted(model)
train_data <- data.frame(cbind(pris = y_train, X_train))
results <- data.frame(Index = rownames(train_data), Actual_Prices = y_train, Fitted_Values = fitted_values)
plot(y_train, fitted_values, main = "Act vs Fit ",
     xlab = "Actualprice", ylab = "FittedVal")
abline(lm(fitted_values ~ y_train), col = "green")
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarA",
            query = pxweb_query_list)
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
bildensitet<-px_data_frame
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

pxweb_query_list <- 
  list("Region"=c("00","01","03","04","05","06","07","08","09","10","12","13","14","15","16","17","18","19","20","21","22","23","24","25","0114","0115","0117","0120","0123","0125","0126","0127","0128","0136","0138","0139","0140","0160","0162","0163","0180","0181","0182","0183","0184","0186","0187","0188","0191","0192","0305","0319","0330","0331","0360","0380","0381","0382","0428","0461","0480","0481","0482","0483","0484","0486","0488","0509","0512","0513","0560","0561","0562","0563","0580","0581","0582","0583","0584","0586","0604","0617","0642","0643","0662","0665","0680","0682","0683","0684","0685","0686","0687","0760","0761","0763","0764","0765","0767","0780","0781","0821","0834","0840","0860","0861","0862","0880","0881","0882","0883","0884","0885","0980","1060","1080","1081","1082","1083","1214","1230","1231","1233","1256","1257","1260","1261","1262","1263","1264","1265","1266","1267","1270","1272","1273","1275","1276","1277","1278","1280","1281","1282","1283","1284","1285","1286","1287","1290","1291","1292","1293","1315","1380","1381","1382","1383","1384","1401","1402","1407","1415","1419","1421","1427","1430","1435","1438","1439","1440","1441","1442","1443","1444","1445","1446","1447","1452","1460","1461","1462","1463","1465","1466","1470","1471","1472","1473","1480","1481","1482","1484","1485","1486","1487","1488","1489","1490","1491","1492","1493","1494","1495","1496","1497","1498","1499","1715","1730","1737","1760","1761","1762","1763","1764","1765","1766","1780","1781","1782","1783","1784","1785","1814","1860","1861","1862","1863","1864","1880","1881","1882","1883","1884","1885","1904","1907","1917","1960","1961","1962","1980","1981","1982","1983","1984","2021","2023","2026","2029","2031","2034","2039","2061","2062","2080","2081","2082","2083","2084","2085","2101","2104","2121","2132","2161","2180","2181","2182","2183","2184","2260","2262","2280","2281","2282","2283","2284","2303","2305","2309","2313","2321","2326","2361","2380","2401","2403","2404","2409","2417","2418","2421","2422","2425","2460","2462","2463","2480","2481","2482","2505","2506","2510","2513","2514","2518","2521","2523","2560","2580","2581","2582","2583","2584"),
       "Agarkategori"=c("060"),
       "ContentsCode"=c("TK1001AB"),
       "Tid"=c("2023"))
pxweb_cite(px_data)
#################################################################################################
####################################################################################
file_path2 <- "C:/Users/Daniel/Documents/ec_utbildning/R/kunskapskontroll/Skarp2.xlsx"
car_data2 <- read_excel(file_path2)
View(car_data2)
class(car_data2)
head(car_data2)
summary(car_data2)
dim(car_data2)
summary(model2)
par(mfrow = c(2, 2)) 
plot(model2, which = 1)
plot(model2, which = 2)
plot(model2, which = 3)
plot(model2, which = 4)
predicted_values <- predict(model2)
min_residual_index <- which.min(model2$residuals)
print(min_residual_index)
min_residual_observation <- car_data2[min_residual_index, ]
print(min_residual_observation)
#######################data partition##################################
X2 <- car_data2[, c('hk', 'mil+ålder', 'auto+premium+el')]
y2 <- car_data2$pris
set.seed(46)
train_indices2 <- sample(1:nrow(car_data2), 0.6 * nrow(car_data2))
X_train2 <- X2[train_indices2, ]
y_train2 <- y2[train_indices2]
X_test2 <- X2[-train_indices2, ]
y_test2 <- y2[-train_indices2]
#####################################################################3
model2 <- lm(pris ~ ., data = data.frame(cbind(pris = y_test2, X_test2)))
predictions2 <- predict(model2, newdata = data.frame(X_test2))
mse2 <- mean((predictions2 - y_test2)^2)
print(paste("Mean Square err:", mse2))
fitted_values2 <- fitted(model2)
train_data2 <- data.frame(cbind(pris = y_train2, X_train2))
results2 <- data.frame(Index = rownames(train_data2), Actual_Prices = y_train2, Fitted_Values = fitted_values2)
View(X2)
plot(y_train2, fitted_values2, main = "Act vs Fit Val",
     xlab = "Act Prices", ylab = "Fit Val")
abline(lm(fitted_values2 ~ y_train2), col = "green")
#####################resampling set2########################
train_control <- trainControl(method = "cv",number = 5)     
model2 <- train(pris ~ .,                       
               data = data.frame(pris = y_train2, X_train2),  
               method = "lm",                  
               trControl = train_control)       
print(model2)
summary(model2)
resampling_results2 <- model2$resample
print(resampling_results2)
summary(resampling_results2)
#############################################################################################################
#################################GYLLENE INDIKATOR?############################################################
file_path <- "C:/Users/Daniel/Documents/ec_utbildning/R/kunskapskontroll/Skarp4.xlsx"
car_data3 <- read_excel(file_path)
View(car_data3)
class(car_data3)
head(car_data3)
summary(car_data3)
dim(car_data3)
residuals <- resid(model)
summary(residuals)
summary(model3)
###################4diagnos#########################
par(mfrow = c(1, 1)) 
plot(model3, which = 1)
plot(model3, which = 2)
plot(model3, which = 3)
plot(model3, which = 4)
##############data partition##################
X3 <- car_data3[, c('stdhästkraft-stdålder')]
y3 <- car_data3$logpris
View(X3)
set.seed(42)
index_train3 <- createDataPartition(y, p = 0.6, list = FALSE)
index_validate_test3 <- createDataPartition(y[index_train3], p = 0.5, list = FALSE)
X_train3 <- X3[index_train3, ]
y_train3 <- y3[index_train3]
X_validate3 <- X3[index_validate_test3, ]
y_validate3 <- y3[index_validate_test3]
X_test3 <- X3[-index_train3, ]
y_test3 <- y3[-index_train3]
#################resampling########################
train_control <- trainControl(method = "cv",number = 5)     
model <- train(pris ~ .,                       
               data = data.frame(pris = y_train, X_train),  
               method = "lm",                  
               trControl = train_control)       
resampling_results <- model$resample
print(resampling_results)
summary(resampling_results)
predictions <- predict(model, newdata = data.frame(pris = y, X))
####################1000kr###############################
model3 <- lm(pris ~ ., data = data.frame(cbind(pris = y_train3, X_train3)))
View(X_train3)
summary(model3)
fitted_values3 <- fitted(model3)
fitted_values[fitted_values < 0] <- 1000
train_data3 <- data.frame(cbind(pris = y_train3, X_train3))
results3 <- data.frame(Index = rownames(train_data3), Actual_Prices = y_train3, Fitted_Values = fitted_values3)
View(results3)
plot(results3)
####################predictions########################################
predictions3 <- predict(model3, newdata = data.frame(X_test3))
View(predictions3)
mse3 <- mean((predictions3 - y_test3)^2)
print(paste("Mean Sqr Err:", mse3))
fitted_values3 <- fitted(model3)
plot(y_test3, predictions3, main = "Act vs Pred Values testset",
     xlab = "Act Price", ylab = "Pred Val")
abline(lm(predictions3 ~ y_test3), col = "green")
print(rmse3)
rmse3<-sqrt(mse3)
summary(predictions3-y_test3)


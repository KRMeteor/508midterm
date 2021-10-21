library(tidyverse)
library(tidycensus)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(jtools)
library(ggstance)
library(kableExtra)
library(broom.mixed)
library(sp)
library(maptools)
library(rgdal)
library(geojsonio)
library(mapview)
library(car)
library(stargazer)

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

nhoods <- 
  st_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson") %>%
  st_transform('ESRI:102286')

boston <- 
  read.csv(file.path(root.dir,"/Chapter3_4/bostonHousePriceData_clean.csv"))

boston.sf <- 
  boston %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102286')

ggplot() +
  geom_sf(data = nhoods, fill = "grey40") +
  geom_sf(data = boston.sf, aes(colour = q5(PricePerSq)), 
          show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels=qBr(boston,"PricePerSq"),
                      name="Quintile\nBreaks") +
  labs(title="Price Per Square Foot, Boston") +
  mapTheme()

bostonCrimes <- read.csv(file.path(root.dir,"/Chapter3_4/bostonCrimes.csv"))

group_by(bostonCrimes, OFFENSE_CODE_GROUP) %>%
  summarize(count = n()) %>%
  arrange(-count) %>% top_n(10) %>%
  kable() %>%
  kable_styling()

bostonCrimes.sf <-
  bostonCrimes %>%
  filter(OFFENSE_CODE_GROUP == "Aggravated Assault",
         Lat > -1) %>%
  dplyr::select(Lat, Long) %>%
  na.omit() %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102286') %>%
  distinct()

#画密度等值图
ggplot() + geom_sf(data = nhoods, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(bostonCrimes.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", 
                      breaks=c(0.000000003,0.00000003),
                      labels=c("Minimum","Maximum"), name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Aggravated Assaults, Boston") +
  mapTheme()

#房子660米之内所有犯罪的数量的sum
boston.sf$crimes.Buffer =
  st_buffer(boston.sf, 660) %>% 
  aggregate(mutate(bostonCrimes.sf, counter = 1),., sum) %>%
  pull(counter)

#离房子距离最近的1，2，3，4，5个犯罪的平均距离
st_c <- st_coordinates
boston.sf <-
  boston.sf %>% 
  mutate(
    crime_nn1 = nn_function(st_c(boston.sf), st_c(bostonCrimes.sf), 1),
    crime_nn2 = nn_function(st_c(boston.sf), st_c(bostonCrimes.sf), 2), 
    crime_nn3 = nn_function(st_c(boston.sf), st_c(bostonCrimes.sf), 3), 
    crime_nn4 = nn_function(st_c(boston.sf), st_c(bostonCrimes.sf), 4), 
    crime_nn5 = nn_function(st_c(boston.sf), st_c(bostonCrimes.sf), 5)) 

#和上边那个ggplot一样的图
ggplot() + geom_sf(data = nhoods, fill = "grey40") +
  stat_density2d(data = data.frame(st_coordinates(bostonCrimes.sf)), 
                 aes(X, Y, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 40, geom = 'polygon') +
  scale_fill_gradient(low = "#25CB10", high = "#FA7800", name = "Density") +
  scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
  labs(title = "Density of Aggravated Assaults, Boston") +
  mapTheme()

#画自变量和因变量的散点图并简单线性回归
st_drop_geometry(boston.sf) %>% 
  mutate(Age = 2015 - YR_BUILT) %>%
  dplyr::select(SalePrice, LivingArea, Age, GROSS_AREA) %>%
  filter(SalePrice <= 1000000, Age < 500) %>%
  gather(Variable, Value, -SalePrice) %>% 
    ggplot(aes(Value, SalePrice)) +
      geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
      facet_wrap(~Variable, ncol = 3, scales = "free") +
      labs(title = "Price as a function of continuous variables") +
      plotTheme()

boston.sf %>%
  st_drop_geometry() %>%
  mutate(Age = 2015 - YR_BUILT) %>%
  dplyr::select(SalePrice, starts_with("crime_")) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
    ggplot(aes(Value, SalePrice)) +
      geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
      facet_wrap(~Variable, nrow = 1, scales = "free") +
      labs(title = "Price as a function of continuous variables") +
      plotTheme()

#数据的条形图汇总
boston %>% 
  dplyr::select(SalePrice, Style, OWN_OCC, NUM_FLOORS) %>%
  mutate(NUM_FLOORS = as.factor(NUM_FLOORS)) %>%
  filter(SalePrice <= 1000000) %>%
  gather(Variable, Value, -SalePrice) %>% 
  ggplot(aes(Value, SalePrice)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  facet_wrap(~Variable, ncol = 1, scales = "free") +
  labs(title = "Price as a function of\ncategorical variables", y = "Mean_Price") +
  plotTheme() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#相关矩阵
numericVars <- 
  select_if(st_drop_geometry(boston.sf), is.numeric) %>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

#简单pearson相关
cor.test(boston$LivingArea, boston$SalePrice, method = "pearson")

ggplot(filter(boston, SalePrice <= 2000000), aes(y=SalePrice, x = LivingArea)) +
  geom_point() +
  geom_smooth(method = "lm")

#房价和居住面积的线性回归
livingReg <- lm(SalePrice ~ LivingArea, data = boston)
summary(livingReg)

#用上边的模型预测一个值
new_LivingArea = 4000
#手动
157968.32 + 216.54 * new_LivingArea
#用预测函数
predict(livingReg, newdata = data.frame(LivingArea = 4000))

effect_plot(livingReg, pred = LivingArea, interval = TRUE, plot.points = TRUE)

#多自变量线性回归
reg1 <- lm(SalePrice ~ ., data = st_drop_geometry(boston.sf) %>% 
             dplyr::select(SalePrice, LivingArea, Style, 
                           GROSS_AREA, R_TOTAL_RM, NUM_FLOORS,
                           R_BDRMS, R_FULL_BTH, R_HALF_BTH, 
                           R_KITCH, R_AC, R_FPLACE))
summary(reg1)

effect_plot(reg1, pred = R_BDRMS, interval = TRUE, plot.points = TRUE)
#制作系数图
plot_summs(reg1)
#制作多个预测模型的系数图
plot_summs(reg1, livingReg)

#将房子层数从数字变成了不同的类，变成了dummy变量
boston.sf <- 
  boston.sf %>%
  mutate(NUM_FLOORS.cat = case_when(
    NUM_FLOORS >= 0 & NUM_FLOORS < 3  ~ "Up to 2 Floors",
    NUM_FLOORS >= 3 & NUM_FLOORS < 4  ~ "3 Floors",
    NUM_FLOORS > 4                    ~ "4+ Floors"))

reg2 <- lm(SalePrice ~ ., data = st_drop_geometry(boston.sf) %>% 
             dplyr::select(SalePrice, LivingArea, Style, 
                           GROSS_AREA, R_TOTAL_RM, NUM_FLOORS.cat,
                           R_BDRMS, R_FULL_BTH, R_HALF_BTH, R_KITCH, 
                           R_AC, R_FPLACE))
summary(reg2)

plot_summs(reg1, reg2)
ggplot(boston.sf) + geom_point(aes(R_BDRMS,R_TOTAL_RM))

#将数据集分为60%的train和40%的test，并把R_AC列中的D类去掉，因为D只出现了一次
inTrain <- createDataPartition(
  y = paste(boston.sf$NUM_FLOORS.cat, boston.sf$Style, boston.sf$R_AC), 
  p = .60, list = FALSE)
boston.training <- boston.sf[inTrain,] 
boston.test <- boston.sf[-inTrain,]  

reg.training <- lm(SalePrice ~ ., data = st_drop_geometry(boston.training) %>% 
                     dplyr::select(SalePrice, LivingArea, Style, 
                                   GROSS_AREA, NUM_FLOORS.cat,
                                   R_BDRMS, R_FULL_BTH, R_HALF_BTH, 
                                   R_KITCH, R_AC, R_FPLACE,
                                   crimes.Buffer))

#将预测模型带入test数据集计算误差，误差绝对值，误差绝对值的变化率
boston.test <-
  boston.test %>%
  mutate(SalePrice.Predict = predict(reg.training, boston.test),
         SalePrice.Error = SalePrice.Predict - SalePrice,
         SalePrice.AbsError = abs(SalePrice.Predict - SalePrice),
         SalePrice.SSE = (SalePrice.Predict - SalePrice)^2,
         SalePrice.SST = (SalePrice.Predict - mean(boston.test$SalePrice, na.rm = T))^2,
         SalePrice.APE = (abs(SalePrice.Predict - SalePrice)) / SalePrice.Predict)%>%
  filter(SalePrice < 5000000)
#误差绝对值的百分数，MAE
mean(boston.test$SalePrice.AbsError, na.rm = T)
#误差绝对值变化率的百分数，MAPE
mean(boston.test$SalePrice.APE, na.rm = T)

SSE=sum(boston.test$SalePrice.SSE, na.rm = T)
SST=sum(boston.test$SalePrice.SST, na.rm = T)
1-SSE/SST

#k-fold
fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)

reg.cv <- 
  train(SalePrice ~ ., data = st_drop_geometry(boston.sf) %>% 
          dplyr::select(SalePrice, 
                        LivingArea, Style, GROSS_AREA, 
                        NUM_FLOORS.cat, R_BDRMS, R_FULL_BTH, 
                        R_HALF_BTH, R_KITCH, R_AC, 
                        R_FPLACE, crimes.Buffer), 
        method = "lm", trControl = fitControl, na.action = na.pass)

reg.cv

reg.cv$resample[1:5,]


#作业
sd.sf<-st_read("./studentData.geojson")%>%
  st_set_crs('ESRI:102254')

topredict <-
  sd.sf%>%
  filter(toPredict==1)

sd.sf<-
  sd.sf%>%
  filter(price<10000000)
#简单柱状图和散点图
ggplot(sd.sf) + geom_histogram(aes(price))
ggplot(sd.sf) + geom_histogram(aes(qualityCode))
ggplot(sd.sf) + geom_histogram(aes(year-builtYear))
ggplot(sd.sf) + geom_histogram(aes(nbrBedRoom))
ggplot(sd.sf) + geom_histogram(aes(Heating), binwidth=1)
ggplot(sd.sf) + geom_histogram(aes(ExtWallPrim), binwidth=1)
ggplot(sd.sf) + geom_point(aes(qualityCode,price))
ggplot(sd.sf) + geom_point(aes(2021-builtYear,price))
ggplot(sd.sf) + geom_point(aes(nbrBedRoom,price))
ggplot(sd.sf) + geom_point(aes(Heating,price))
ggplot(sd.sf) + geom_point(aes(ExtWallPrim,price))
ggplot(sd.sf) + geom_point(aes(IntWallDscr,price))
ggplot(sd.sf) + geom_point(aes(ExtWallPrim,IntWallDscr))

#系数矩阵
sd.sf.rd<-subset(sd.sf,select=c(-status_cd,-UnitCount,-Stories,-Roof_CoverDscr,
                      -Roof_Cover,-ExtWallDscrSec,-ExtWallSec,-AcDscr,-Ac))%>%
  mutate(age=year-builtYear)

topredict<-subset(topredict,select=c(-status_cd,-UnitCount,-Stories,-Roof_CoverDscr,
                                 -Roof_Cover,-ExtWallDscrSec,-ExtWallSec,-AcDscr,-Ac))%>%
  mutate(age=year-builtYear)

numericVars <- 
  select_if(st_drop_geometry(sd.sf.rd), is.numeric) %>% na.omit()

cor(numericVars)
ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

#模型尝试
reg <- lm(price ~ ., data = st_drop_geometry(sd.sf.rd) %>% 
             dplyr::select(price, qualityCode, age, 
                           nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                           ExtWallDscrPrim))
plot_summs(reg)
effect_plot(reg, pred = builtYear, interval = TRUE, plot.points = TRUE)
summary(reg)

#census数据
census_api_key("f75c994cf9151eecca2d2f379ec6962793e32828", overwrite = TRUE)

acs_variable_list.2019 <- load_variables(2019, #year
                                         "acs5", #five year ACS estimates
                                         cache = TRUE)


tracts19 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E","B28010_007E","B08101_001E",
                                             "B09001_001E","B09001_003E","B09021_002E",
                                             "B11001I_001E", "B14001_009E",
                                             "B17001_002E","B27001_001E","B18101_001E",
                                             "B19001_001E","B25001_001E","B25040_001E"), 
          year=2019, state=08, county=013, geometry=T, output="wide") %>%
  st_transform('ESRI:102254') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E,
         Nocom = B28010_007E, 
         Waytowork = B08101_001E,
         Popunder18 = B09001_001E, 
         Popunder3 = B09001_003E,
         Singleadult = B09021_002E, 
         Householdtype = B11001I_001E,
         Addmittogra = B14001_009E,
         Poverty  = B17001_002E,
         Healthins  = B27001_001E,
         Disable  = B18101_001E,
         Familyincome  = B19001_001E,
         Housingunits  = B25001_001E,
         Househeatingfuel  = B25040_001E)%>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2019") 


#数据合并
boulder<-
  st_join(sd.sf.rd,tracts19)%>%
  dplyr::select(price, qualityCode, age, 
                nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                ExtWallDscrPrim, NAME,Nocom,Waytowork,Popunder18,
                Popunder3,Singleadult,Householdtype,Addmittogra,
                Poverty,Healthins,Disable,Familyincome,
                Housingunits,Househeatingfuel,pctWhite,
                pctBachelors,pctPoverty)

topredict<-
  st_join(topredict,tracts19)%>%
  dplyr::select(MUSA_ID, price, qualityCode, age, 
                nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                ExtWallDscrPrim, NAME,Nocom,Waytowork,Popunder18,
                Popunder3,Singleadult,Householdtype,Addmittogra,
                Poverty,Healthins,Disable,Familyincome,
                Housingunits,Househeatingfuel,pctWhite,
                pctBachelors,pctPoverty)

numericVarsboulder <- 
  select_if(st_drop_geometry(boulder), is.numeric) %>% na.omit()

cor(numericVarsboulder)
ggcorrplot(
  round(cor(numericVarsboulder), 1), 
  p.mat = cor_pmat(numericVarsboulder),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

regboulder <- lm(price ~ ., data = st_drop_geometry(boulder) %>% 
            dplyr::select(price, qualityCode, age, 
                          nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                          ExtWallDscrPrim, NAME, pctWhite))
summary(regboulder)
plot_summs(regboulder)
effect_plot(regboulder, pred = builtYear, interval = TRUE, plot.points = TRUE)

#偷车数据
stobike<-st_read("./Stolen_Bikes.geojson")%>%
  st_set_crs('EPSG:4326')%>%
  st_transform('ESRI:102254')

stobike.sf <-
  stobike %>%
  dplyr::select(geometry) %>%
  na.omit()

crime.cor <-
  st_c(stobike.sf)%>%
  na.omit()

boulder.cor <-
  st_c(boulder)%>%
  na.omit()

st_c <- st_coordinates
boulder <-
  boulder %>% 
  mutate(
    crime_nn1 = nn_function(boulder.cor, crime.cor, 1),
    crime_nn2 = nn_function(boulder.cor, crime.cor, 2), 
    crime_nn3 = nn_function(boulder.cor, crime.cor, 3), 
    crime_nn4 = nn_function(boulder.cor, crime.cor, 4), 
    crime_nn5 = nn_function(boulder.cor, crime.cor, 5)) 

topredict <-
  topredict %>% 
  mutate(crime_nn5 = nn_function(st_c(topredict), crime.cor, 5)) 

regboulder <- lm(price ~ ., data = st_drop_geometry(boulder) %>% 
                   dplyr::select(price, qualityCode, age, 
                                 nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                                 ExtWallDscrPrim, NAME,pctWhite,
                                 pctBachelors,pctPoverty,crime_nn5))
summary(regboulder)

#学校数据
school<-st_read("./Address_Points.geojson")%>%
  st_transform(st_crs(boulder))

boulder <-
  boulder %>% 
  mutate(
    school_nn1 = nn_function(boulder.cor, st_c(school), 1),
    school_nn2 = nn_function(boulder.cor, st_c(school), 2), 
    school_nn3 = nn_function(boulder.cor, st_c(school), 3), 
    school_nn4 = nn_function(boulder.cor, st_c(school), 4), 
    school_nn5 = nn_function(boulder.cor, st_c(school), 5))

topredict <-
  topredict %>% 
  mutate(school_nn5 = nn_function(st_c(topredict), st_c(school), 5),
         logschool_nn5=log(school_nn5))

boulder<-
  boulder%>%
  mutate(
    logprice=log(price),
    logschool_nn5=log(school_nn5))

regboulder <- lm(logprice ~ ., data = st_drop_geometry(boulder) %>% 
                   dplyr::select(logprice, qualityCode, age, 
                                 nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                                 ExtWallDscrPrim, NAME,crime_nn5,logschool_nn5))
summary(regboulder)

#训练集和测试集
inTrain <- createDataPartition(
  y= paste(boulder$ExtWallDscrPrim),
  p = .75, list = FALSE)
boulder.training <- boulder[inTrain,] 
boulder.test <- boulder[-inTrain,] 

regtrain <- lm(logprice ~ ., data = st_drop_geometry(boulder.training) %>% 
                   dplyr::select(logprice, qualityCode, age, 
                                 nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                                 ExtWallDscrPrim, NAME,crime_nn5,logschool_nn5))
summary(regtrain)

#把结果用干净的表格展示出来
stargazer(regtrain, header = FALSE, type = "text", notes.append = FALSE, 
          notes = c("<sup>&sstarf;</sup>p<0.1; <sup>&sstarf;&sstarf;</sup>p<0.05; <sup>&sstarf;&sstarf;&sstarf;</sup>p<0.01"),
          dep.var.labels   = "log price")

#测试集的MAE和MAPE
boulder.test <-
  boulder.test %>%
  mutate(logprice.Predict = predict(regtrain, boulder.test),
         price.Predict=exp(logprice.Predict),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.SSE = (price.Predict - price)^2,
         price.SST = (price.Predict - mean(boulder.test$price, na.rm = T))^2,
         price.APE = (abs(price.Predict - price)) / price.Predict)
#误差绝对值的百分数，MAE
mean(boulder.test$price.AbsError, na.rm = T)
#误差绝对值变化率的百分数，MAPE
mean(boulder.test$price.APE, na.rm = T)

testset <- data.frame(MAE = mean(boulder.test$price.AbsError, na.rm = T),
                      MAPE = mean(boulder.test$price.APE, na.rm = T))

kable(testset, 
      col.names = c("MAE", "MAPE"),
      caption = "Mean absolute error and MAPE for test set") %>% 
  kable_styling(full_width = F)

#kfold
fitControl <- trainControl(method = "cv", number = 100)
set.seed(825)

reg.cv <- 
  train(logprice ~ ., data = st_drop_geometry(boulder) %>% 
          dplyr::select(logprice, qualityCode, age, 
                        nbrRoomsNobath, mainfloorSF, TotalFinishedSF,
                        ExtWallDscrPrim, NAME,crime_nn5,logschool_nn5), 
        method = "lm", trControl = fitControl, na.action = na.pass)

reg.cv

reg.cv$resample[1:100,]

kfold <- data.frame(mean_MAE = mean(reg.cv[["resample"]][["MAE"]]),
                    sd_MAE = sd(reg.cv[["resample"]][["MAE"]]))

kable(kfold, 
      col.names = c("Average MAE", "Std Deviation of MAE"),
      caption = "100-fold Cross-Validation Results") %>% 
  kable_styling(full_width = F)

ggplot(as.data.frame(reg.cv$resample), aes(MAE)) + 
  geom_histogram(bins = 50, colour="white", fill = "#FFD365") +
  geom_vline(aes(xintercept = mean(MAE)), color = "blue", size = 1) + 
  labs(title="Distribution of MAE", subtitle = "k-fold cross validation; k = 100",
       x="Mean Absolute Error", y="Count") +
  plotTheme()

#预测值和真实值的相关图
ggplot(boulder.test, aes(price.Predict, price)) +
  geom_point(size = 0.75, colour = "black") +
  stat_smooth(data=boulder.test, aes(price, price),
              method = "lm", se = FALSE, size = 1, colour="#FA7800") +
  stat_smooth(data=boulder.test, aes(price, price.Predict),
              method = "lm", se = FALSE, size = 1, colour="#25CB10") +
  labs(title="Predicted sale price as a function of observed price",
       subtitle="Orange line represents a perfect prediction; Grean line represents prediction") +
  plotTheme()

#测试集残差图
ggplot() +
  geom_sf(data = tracts19, fill = "grey80") +
  geom_sf(data = boulder.test, aes(colour = q5(price.Error)), show.legend = "point", size = 1) +
  scale_colour_manual(values = palette5,
                      labels = qBr(boulder.test, "price.Error"),
                      name = "Quintile\nBreaks") +
  labs(title = "Sale price errors for test set") +
  mapTheme()

#Moran's I和lag图
boulder.test.cor <- 
  boulder.test %>%
  st_coordinates()

neighborList.test <- knn2nb(knearneigh(boulder.test.cor, 5))

spatialWeights.test <- nb2listw(neighborList.test, style="W")

boulder.test$lagPriceError <- lag.listw(spatialWeights.test, boulder.test$price.Error)

moranTest <- moran.mc(boulder.test$price.Error, 
                      spatialWeights.test , nsim = 999)

ggplot(as.data.frame(moranTest$res[c(1:999)]), aes(moranTest$res[c(1:999)])) +
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = moranTest$statistic), colour = "#FA7800",size=1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title="Observed and permuted Moran's I",
       subtitle= "Observed Moran's I in red",
       x="Moran's I",
       y="Count") +
  plotTheme()

moranTesttable <- data.frame(Statistics = moranTest[["statistic"]][["statistic"]],
                    Pvalue = moranTest[["p.value"]])

kable(moranTesttable, 
      col.names = c("Moran's I Statistics", "P Value"),
      caption = "Moran's I") %>% 
  kable_styling(full_width = F)

ggplot(boulder.test, aes(lagPriceError, price.Error)) +
  geom_point(size = 0.75, colour = "black") +
  stat_smooth(data=boulder.test, aes(lagPriceError, price.Error),
              method = "lm", se = FALSE, size = 1, colour="#FA7800") +
  labs(title="Lag error of price as a function of error of price") +
  plotTheme()

#全部预测值的图
boulder <-
  boulder %>%
  mutate(logprice.Predict = predict(regboulder, boulder),
         price.Predict=exp(logprice.Predict))

ggplot() +
  geom_sf(data = tracts19, fill = "grey80") +
  geom_sf(data = boulder, aes(colour = q5(price.Predict)), show.legend = "point", size = .75) +
  scale_colour_manual(values = palette5,
                      labels = qBr(boulder, "price.Predict"),
                      name = "Quintile\nBreaks") +
  labs(title = "Predicted sale price for all data") +
  mapTheme()

#MAPE by neighborhood的图

boulder.test.neighbor <- 
  boulder.test %>% 
  group_by(NAME) %>% 
  summarize(price = mean(price),
            MAPE = mean(price.APE)) %>%
  st_set_geometry(NULL) %>%
  left_join(tracts19) %>%
  st_sf()

ggplot() +
  geom_sf(data = tracts19, fill = "grey80") +
  geom_sf(data = boulder.test.neighbor, aes(fill = q5(MAPE)), size = 1) +
  scale_fill_manual(values = palette5,
                    labels = qBr(boulder.test.neighbor, "MAPE", rnd = FALSE),
                    name = "Quintile\nBreaks") +
  labs(title = "MAPE by Neighborhood for Test Set") +
  mapTheme()

#MAPE by neighborhood 和mean price by neighborhood的散点图
ggplot(boulder.test.neighbor, aes(MAPE, price)) +
  geom_point(size = 2, colour = "black") +
  stat_smooth(data=boulder.test.neighbor, aes(MAPE, price),
              method = "lm", se = FALSE, size = 1, colour="#FA7800") +
  labs(title="MAPE by neighborhood as a function of mean price by neighborhood") +
  plotTheme()

#imcome分为High income 和low income 来检测模型的generalizability
tracts19income <- 
  get_acs(geography = "tract", variables = c("B06011_001E"), 
          year=2019, state=08, county=013, geometry=T, output="wide") %>%
  st_transform('ESRI:102254') %>%
  rename(Median_Income = B06011_001E) %>%
  mutate(incomeContext = ifelse(Median_Income > 40000, "High Income", "Low Income"))

ggplot() + 
  geom_sf(data = tracts19income, aes(fill = incomeContext)) +
  scale_fill_manual(values = c("#ef8a62", "#67a9cf"), name="Income Context") +
  labs(title = "Income Context") +
  mapTheme() + 
  theme(legend.position="bottom") 

boulder.test.income <-
  st_join(boulder.test, tracts19income) %>% 
    group_by(incomeContext) %>%
    summarize(MAE = mean(price.AbsError),
              MAPE = mean(price.APE)) %>%
    st_drop_geometry()

kable(boulder.test.income, caption = "Errors for test set sale price predictions by income contexts") %>%
  kable_styling("striped", full_width = F, position = "left") %>%
  row_spec(1, color = "black", background = "#ef8a62") %>%
  row_spec(2, color = "black", background = "#67a9cf")

#模型预测

topredict$logprice <- predict(regtrain, topredict)
topredict$price=exp(topredict$logprice)

topredictoutput<-
  topredict%>%
  dplyr::select(MUSA_ID,price)%>%
  st_drop_geometry()

write.csv(topredictoutput, file="Moran's eye.csv", row.names = FALSE)



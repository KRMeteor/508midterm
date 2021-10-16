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
  st_set_crs('EPSG:6427')%>%
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
sd.sf.rd<-subset(sd.sf,select=c(-toPredict,-status_cd,-UnitCount,-Stories,-Roof_CoverDscr,
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
                                             "B06012_002E"), 
          year=2019, state=08, county=013, geometry=T, output="wide") %>%
  st_transform('EPSG:6427') 

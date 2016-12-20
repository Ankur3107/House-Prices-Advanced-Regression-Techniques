train <- read.csv("train.csv")
test <- read.csv("test.csv")
train$ID <- NULL
train_continuous <- c("MSSubClass","LotFrontage","LotArea","OverallQual",
                      "OverallCond","YearBuilt","YearRemodAdd","MasVnrArea",
                      "BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF",
                      "X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea",
                      "BsmtFullBath","BsmtHalfBath","FullBath","HalfBath",
                      "BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces",
                      "GarageYrBlt","GarageCars","GarageArea","WoodDeckSF",
                      "OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch",
                      "PoolArea","MiscVal","MoSold","YrSold","SalePrice")

corrplot::corrplot(cor(train[,train_continuous[1:37]],use="pairwise.complete.obs"))

naCol <- function(train){
  return(colnames(train)[colSums(is.na(train)) > 0])
}
train_NA <- naCol(train)

head(train[,train_NA[1:19]])

# NA plotting with Saleprice


plot(train[,train_NA[1]],train$SalePrice)
plot(train[,train_NA[2]],train$SalePrice)
plot(train[,train_NA[3]],train$SalePrice)
plot(train[,train_NA[4]],train$SalePrice)
plot(train[,train_NA[5]],train$SalePrice)
plot(train[,train_NA[6]],train$SalePrice)
plot(train[,train_NA[7]],train$SalePrice)
plot(train[,train_NA[8]],train$SalePrice)
plot(train[,train_NA[9]],train$SalePrice)
plot(train[,train_NA[10]],train$SalePrice)
plot(train[,train_NA[11]],train$SalePrice)
plot(train[,train_NA[12]],train$SalePrice)
plot(train[,train_NA[13]],train$SalePrice)
plot(train[,train_NA[14]],train$SalePrice)
plot(train[,train_NA[15]],train$SalePrice)
plot(train[,train_NA[16]],train$SalePrice)
plot(train[,train_NA[17]],train$SalePrice)
plot(train[,train_NA[18]],train$SalePrice)
plot(train[,train_NA[19]],train$SalePrice)

naPlotting <- function(df,nadf){
  
  
  par(mfrow=c(3,3))
    for(j in 1:9)
    {
      plot(df[,nadf[j]],df$SalePrice)
    }
  par(mfrow=c(3,3))
    for(j in 10:18)
    {
      plot(df[,nadf[j]],df$SalePrice)
    }
    
  }
  
naPlotting(train,train_NA)

# Missing values imputation

summary(train[,train_NA[1:19]])

missingTypeVariable <- function(df,nadf,n=19){
  
        intType <- c()
        factorType <- c()
        for(i in 1:19)
        {
            if(class(df[,nadf[i]])=="integer")
              intType <- c(intType,nadf[i])
            else
              factorType <- c(factorType,nadf[i])
        }
        
        
        
        return (list(intType=intType,factorType=factorType))
        
}

train_NA_Missing_Type <- missingTypeVariable(train,train_NA)

train_NA_int_type <- unlist(train_NA_Missing_Type[1])
train_NA_factor_type <- unlist(train_NA_Missing_Type[2])

#integer type correlation with target

cor(train[,train_NA_int_type[1:3]],train$SalePrice,use="pairwise.complete.obs")

# 1.first missing continous variable 

summary(lm(LotFrontage ~ LotArea+X1stFlrSF+TotRmsAbvGrd+GarageArea+SalePrice,data = train))
 #temp 1
model1 <- lm(LotFrontage ~ LotArea+X1stFlrSF+TotRmsAbvGrd+GarageArea+SalePrice,data = train)
t <- train
t$LotFrontage <-NULL
x <- predict(model1,t)
t$LotFrontage <- ifelse(is.na(train$LotFrontage),x,train$LotFrontage)
train <- t
#########

cor(train[,train_continuous[1:37]],use="pairwise.complete.obs")

# 2. Second missing continous varibale

summary(lm(MasVnrArea~YearBuilt+TotalBsmtSF+GrLivArea+GarageArea+SalePrice,data = train)) 
#temp 2
model2 <- lm(MasVnrArea~YearBuilt+TotalBsmtSF+GrLivArea+GarageArea+SalePrice,data = train)
t <- train
t$MasVnrArea <-NULL
x <- predict(model2,t)
t$MasVnrArea <- ifelse(is.na(train$MasVnrArea),x,train$MasVnrArea)
train <- t

# 3. Third missing continous variable

summary(lm(GarageYrBlt~OverallCond+YearBuilt+YearRemodAdd+FullBath+GarageArea+SalePrice,data = train))

#temp 2
model3 <- lm(GarageYrBlt~OverallCond+YearBuilt+YearRemodAdd+FullBath+GarageArea+SalePrice,data = train)
t <- train
t$GarageYrBlt <-NULL
x <- predict(model3,t)
t$GarageYrBlt <- ifelse(is.na(train$GarageYrBlt),x,train$GarageYrBlt)
train <- t



#factor type correlation with target



ggplot(train,aes(train$SalePrice,train[,train_NA_factor_type[1]])) + geom_boxplot()



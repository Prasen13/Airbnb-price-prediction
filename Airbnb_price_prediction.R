#Uncomment to install the packages if needed
#install.packages(ggplot2)
#install.packages(ggpubr)
#install.packages(psych)
#install.packages(dplyr)
#install.packages(reshape2)
#install.packages(car)
#install.packages("MASS")
#install.packages("leaps")

#import libraries
library(ggplot2)
library(ggpubr)
library(psych)
library(dplyr)
library(reshape2)
library(car)
library(caret)
library(glmnet)
library(Metrics)
library(MASS)
library(gridExtra)
library(leaps)

#Read the dataset When prompted navigate and open listings.csv file
Airbnb_listing_df <- read.csv(file.choose(), sep=",", header = TRUE, stringsAsFactors = FALSE)

#Subsetting the original dataframe with required columns
Airbnb_listing_df1 <- Airbnb_listing_df[,c("price",
                                           "beds",
                                           "accommodates",
                                           "bedrooms",
                                           "bathrooms",
                                           "host_response_rate",
                                           "number_of_reviews",
                                           "property_type",
                                           "room_type",
                                           "neighbourhood",
                                           "zipcode")]         


#Exploratory data analysis
#converting price variable to numeric
Airbnb_listing_df1$price <- as.numeric(gsub("[$]","",Airbnb_listing_df1$price))
#converting response to numeric
Airbnb_listing_df1$host_response_rate <- as.numeric(gsub("[%]","",Airbnb_listing_df1$host_response_rate))
#converting property type to factor
Airbnb_listing_df1$property_type <- as.factor(Airbnb_listing_df1$property_type)
#converting room type to factor
Airbnb_listing_df1$room_type <- as.factor(Airbnb_listing_df1$room_type) 
#converting zipcode to factor
Airbnb_listing_df1$zipcode <- as.factor(Airbnb_listing_df1$zipcode) 
#converting neighbourhood to factor
Airbnb_listing_df1$neighbourhood <-as.factor(Airbnb_listing_df1$neighbourhood) 

#Summary Statistics of the boston airbnb dataset
summary(Airbnb_listing_df1) 

#Display the descriptive statistics of selected features 
des_Airbnb <- psych::describe(Airbnb_listing_df1)
#To display total number of records,mean,median,sd,minimum and max values of selected features 
des_Airbnb <- des_Airbnb %>% select(n,mean,median,sd,min,max)
des_Airbnb

#listing null values for each column
lapply(Airbnb_listing_df1,function(x) { length(which(is.na(x)))}) 

#Dropping the null value records
Airbnb_listing_df1<- na.omit(Airbnb_listing_df1) 

#Visualizations and descriptive analysis

#Histogram for Pricing distribution-Commented,as this is not used in the report
#hist(log(Airbnb_listing_df1$price),breaks = 100,xlab = "Price",
#     col = c("red","white"),main = "Histogram for Price Distribution")


#Identifying cost variation based on the neighborhood
#To find the unique neighborhoods in boston airbnb
unique(Airbnb_listing_df1$neighbourhood) 
par(mar=c(10,4,4,2))
#Plot for Number of listing for each neighborhood
barplot(table(Airbnb_listing_df1$neighbourhood), 
     las=2,col = c("red","white"),
     main = "Number of Listings for each Neighbourhood") 

dev.off()

#Average price for each neighbourhood
Neighbourhoods_pricing <-  Airbnb_listing_df1 %>% group_by(neighbourhood)%>%summarise(Avg_price=mean(price))%>%arrange(desc(Avg_price))
#top 10 neighborhood in the order of price
head(Neighbourhoods_pricing,10)
#Plot on Average Pricing for each Neighbourhood in Boston
ggplot(Neighbourhoods_pricing, aes(x=neighbourhood, y=Avg_price)) +
  geom_segment( aes(xend=neighbourhood, yend=0)) +
  geom_point( size=4, color="violetred") + coord_flip() +theme_bw() +
  labs(title ="Average Pricing for each Neighbourhood in Boston ",
       y="Average Price", x="Neighbourhood")+
  theme(plot.title = element_text( face="bold", colour="violetred", size=14,hjust = 0.5),
        axis.title.x = element_text(color="violetred", size=14, face="bold"),
        axis.title.y = element_text(color="violetred", size=14, face="bold"),
        plot.margin= margin(2, 1.5,2, 0.5, "cm"))

dev.off()
#Plot for to show the relationship between Reviews and ameneties related variables 
par(mfrow=c(2,2))
#Plot to show relationship of variables-beds and number of reviews 
plot(Airbnb_listing_df1$beds,Airbnb_listing_df1$number_of_reviews,
     xlim = c(0,9),xlab = "Beds",ylab = "Number of Reviews",col="violet")
#Plot to show relationship of variables-accomodates and number of reviews 
plot(Airbnb_listing_df1$accommodates,Airbnb_listing_df1$number_of_reviews,
     xlab = "Accomodates",ylab = "Number of Reviews",col="blue") 
#Plot to show relationship of variables-bathrooms and number of reviews 
plot(Airbnb_listing_df1$bathrooms,Airbnb_listing_df1$number_of_reviews,
     xlab = "Bathrooms",ylab = "Number of Reviews",col="green") 
#plot to show relationship of variables-bedrooms and number of reviews 
plot(Airbnb_listing_df1$bedrooms,Airbnb_listing_df1$number_of_reviews,
     xlab = "Bedrooms",ylab = "Number of Reviews",col="orange") 

dev.off()

#Security deposit, Price relation (no significance)
#Airbnb_listing_df$security_deposit <- as.numeric(gsub("[$]","",Airbnb_listing_df$security_deposit))
#plot(Airbnb_listing_df$security_deposit,Airbnb_listing_df$price)
#ggplot(Airbnb_listing_df,aes(security_deposit,price))+
#  geom_point()


#Plots to show the relationship between room type, pricing relationship
#Creation of plot for displaying number of properties for each room type
fig1 <- ggplot(Airbnb_listing_df1,aes(Airbnb_listing_df1$room_type))+
  geom_bar(fill="red")+
  labs(title = "Room Type listings", x="Room Type",y="Count")+
  theme(plot.margin= margin(2, 1.5,2, 0.5, "cm"),
        plot.title = element_text( face="bold", colour="violetred", size=14,hjust = 0.5),
        axis.title.x = element_text(color="violetred", size=14, face="bold"),
        axis.title.y = element_text(color="violetred", size=14, face="bold"))

#Creation of Plot for Displaying Price distribution for each room type
fig2 <- ggplot(Airbnb_listing_df1, aes(x=room_type,y=price))+
  geom_boxplot(fill="green")+
  labs(title = "Price Range of Each Room Type", x="Room Type",y="Price")+
  theme(plot.margin= margin(2, 1,2, 0, "cm"),
        plot.title = element_text( face="bold", colour="violetred", size=14,hjust = 0.5),
        axis.title.x = element_text(color="violetred", size=14, face="bold"),
        axis.title.y = element_text(color="violetred", size=14, face="bold"))

#Plotting both fig1, fig2 on same screen
ggarrange(fig1,fig2,ncol = 2, nrow = 1)
         
#Creation of Correlation Matrix
#Subsetting the numerical columns from airbnb dataset
Numerical_columns <- Airbnb_listing_df1[,!unlist(lapply(Airbnb_listing_df1, is.factor))] 
#Correlation matrix for 7 numerical variables
cor_matrix <- round(cor(Numerical_columns),2) 
cor_matrix

#Function to produce upper triangluar matrix
get_upper_tri <- function(cor_matrix){
  cor_matrix[lower.tri(cor_matrix)]<- NA
  return(cor_matrix)
}
#Calling upper triangular matrix on our Correlation matrix
upper_tri <- get_upper_tri(cor_matrix) 
upper_tri

#Creation of Plot for upper triangular correlation matrix for numerical variables of airbnb dataset
ggplot(data = melt(upper_tri, na.rm = TRUE), aes(Var2,Var1,fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
  limit = c(-1,1), space = "Lab",name="Pearson\nCorrelation") + theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,size = 12, hjust = 1))+
  coord_fixed()+  geom_text(aes(Var2, Var1,label = value), color = "black", size = 4)



#data partion
#TASK-1 train and test split
set.seed(1234)
train_rows <- createDataPartition(Airbnb_listing_df1$price, p=0.7,list = FALSE,times = 1 )
Train_data <- Airbnb_listing_df1[train_rows,]
Test_data <- Airbnb_listing_df1[-train_rows,]




#Creation of Regression model
#Building regression model to predict Price  using lm()
model1 <- lm(formula = price~., data =Train_data) 
#Summary
summary(model1) 
#Creation of four regression plots
plot(model1) 

#TRAIN SET PREDICTION

#TEST SET PREDICTIONS

#ERROR METRIC [RMSE]




#building regression model to predict Price using glm()
model2 <- glm(formula = price~.,data = Train_data) 
#Summary
summary(model2) 


#Correlation between number of beds and price and scatterplot shows the linear regression
scatterplot(price~beds, data = Airbnb_listing_df1)

#cross validation
#lasso regression 

train_x_l <- model.matrix(price~.,Train_data)[,-1]
test_x_l <- model.matrix(price~.,Test_data)[,-1]
train_y_l <- Train_data$price
test_y_l <- Test_data$price

#finding best values of lambda using crossvalidation
set.seed(1234)
cv.lasso <- cv.glmnet(train_x_l,train_y_l, nfolds=10)
#optimal value of lambda, minimises the prediction error
cv.lasso$lambda.min #lambda.min minimizes out of sample loss
cv.lasso$lambda.1se #lambda.1se largest value of lambda within one standard error of lambda min

#TASK-8 ploting the reults of cv.glmnet function
plot(cv.lasso)

#TASK-9
#fitting the model on training data using "lambda.min"
#here we are using alpha=1 for lasso regression
model.min_1 <-glmnet(train_x_l,train_y_l, alpha = 1,lambda = cv.lasso$lambda.min)
model.min_1
coef(model.min_1) #displaying regression coefficients
#fitting the model on training data using "lambda.1se"
model.1se_1 <-glmnet(train_x_l,train_y_l, alpha = 1,lambda = cv.lasso$lambda.1se)
model.1se_1
coef(model.1se_1) #displaying regression coefficients

#TASK-10
#train set predictions
preds.train_1_1se <- predict(model.1se_1, newx = train_x_l) #Using model.1se
train.rmse_1_1se <- rmse(train_y_l,preds.train_1_1se)
train.rmse_1_1se #Train RMSE Using model.1se
preds.train_1_min <- predict(model.min_1, newx = train_x_l) #Using model.min
train.rmse_1_min <- rmse(train_y_l,preds.train_1_min)
train.rmse_1_min #Train RMSE Using model.min

#TASK-11
#test set predictions 
preds.test_1_1se <- predict(model.1se_1, newx = test_x_l) #using model.1se
test.rmse_1_1se <- rmse(test_y_l,preds.test_1_1se)
test.rmse_1_1se #Test RMSE using model.1se
preds.test_1_min <- predict(model.min_1, newx = test_x_l) #using model.min
test.rmse_1_min <- rmse(test_y_l,preds.test_1_min)
test.rmse_1_min #Test RMSE using model.


set.seed(1234)
cv.ridge <- cv.glmnet(train_x_l,train_y_l, nfolds=10)
#TASK-9
#fitting the model on training data using "lambda.min"
#here we are using alpha=1 for lasso regression
model.min_r <-glmnet(train_x_l,train_y_l, alpha = 0,lambda = cv.ridge$lambda.min)
model.min_r
coef(model.min_r) #displaying regression coefficients
#fitting the model on training data using "lambda.1se"
model.1se_r <-glmnet(train_x_l,train_y_l, alpha = 0,lambda = cv.ridge$lambda.1se)
model.1se_r
coef(model.1se_r) #displaying regression coefficients

#TASK-10
#train set predictions
preds.train_r_1se <- predict(model.1se_r, newx = train_x_l) #Using model.1se
train.rmse_r_1se <- rmse(train_y_l,preds.train_r_1se)
train.rmse_r_1se #Train RMSE Using model.1se
preds.train_r_min <- predict(model.min_1, newx = train_x_l) #Using model.min
train.rmse_r_min <- rmse(train_y_l,preds.train_r_min)
train.rmse_r_min #Train RMSE Using model.min

#TASK-11
#test set predictions 
preds.test_r_1se <- predict(model.1se_r, newx = test_x_l) #using model.1se
test.rmse_r_1se <- rmse(test_y_l,preds.test_r_1se)
test.rmse_r_1se #Test RMSE using model.1se
preds.test_r_min <- predict(model.min_1, newx = test_x_l) #using model.min
test.rmse_r_min <- rmse(test_y_l,preds.test_r_min)
test.rmse_r_min #Test RMSE using model.


#lasso
#train RMSE
train.rmse_1_1se
train.rmse_1_min
#test RMSE
test.rmse_1_1se
test.rmse_1_1se
#Ridge
#train RMSE
train.rmse_r_1se
train.rmse_r_min
#test RMSE
test.rmse_r_1se
test.rmse_r_1se




#stepwise regression
stepwise_model1 <- stepAIC(model1, direction = "both",trace = FALSE)
summary(stepwise_model1)







#stepwise regression(Method 2 - using train())
#Task-13:- Performing Stepwise Regression(Both FOrward and BAckward)(Method-1)

# Fitting the full model 
full.model <- lm(price ~., data = Airbnb_listing_df1)
#Building a stepwise regression model
stepwise_model <- stepAIC(full.model, direction = "both", 
                          trace = FALSE)
summary(stepwise_model)



#





library(leaps)
#Task-13:- Performing Stepwise Regression(Both Forward and Backward)(Method 2 - using train())
# Set seed for reproducibility
set.seed(123)
# Setting up repeated k-fold cross-validation
train_control <- trainControl(method = "crossvalidation", number = 10)
#Fitting the model using train()
stepwise_model <- train(price ~., data =Airbnb_listing_df1 ,
                         method = "leapBackward", 
                         tuneGrid = data.frame(nvmax = 15:40),
                         trControl = train.control)

#Displaying the results
stepwise_model$results
#Displaying the best tuning values selected automatically
stepwise_model$bestTune
#Reporting the best set of variables
summary(stepwise_model$finalModel)
#Displaying the regression coefficient of final variable
coef(stepwise_model$finalModel, 5)



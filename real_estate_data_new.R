data_real_estate<- read.csv(file.choose(),header=T)
data_real_estate
head(data_real_estate)
data_real_estate$date
new_date<-as.Date(data_real_estate$date, "%Y%m%dT000000")
head(new_date)
data_real_estate[["date"]]<-new_date
colnames(data_real_estate)[colnames(data_real_estate)=="date"]<-"new_date"
head(data_real_estate)
df_data_real_estate = data_real_estate[,!(names(data_real_estate) %in% c("id"))]
df_data_real_estate
new_ID<- seq.int(nrow(data_real_estate))
new_ID

library(tibble)
add_column(df_data_real_estate,new_ID , .before = 1)
head(df_data_real_estate)
dim(df_data_real_estate)
colnames(df_data_real_estate)
summary(df_data_real_estate)
df_data_real_estate$condition[df_data_real_estate$condition==1]

##Visualization:
real_estate<-df_data_real_estate[ ,2:20]
real_estate_cor = cor(real_estate)
real_estate_cor = cor(real_estate_cor, method = c("spearman"))
install.packages("corrplot")
library(corrplot)
corrplot(real_estate_cor)
##Positive correlations are displayed in a blue scale while negative correlations are displayed in a red scale.


##To visualize relation between price and condition ,correlation
#matrix shows slightly negative correlation,let's dig little deep
data_real_estate_price<-data_real_estate$price/100000
price.conditions <- aggregate(data_real_estate_price ~ condition, 
                              data =data_real_estate ,
                              FUN = mean)
price.conditions

barplot(height = price.conditions$data_real_estate_price,
        names.arg = price.conditions$condition,
        xlab = "Price of  House",
        ylab = "Condition of the Houses",
        main = "Average House Condition  by Price",
        col = "orchid")

##barplot and correlation matrix shows contradictory results 
##plotting scatterplot to examine why


plot(price/1000000 ~ condition, data = data_real_estate,  type = "p", pch = 4,col="coral1", main="Plot",xlab="Condition",ylab="price")

abline(lm(price/100000 ~ condition, data = data_real_estate),col="blue")

modelrel <- lm(price/1000000  ~ condition, data = data_real_estate)
modelrel
summary(modelrel)
##Conclusion:The reason why there was slightly negatively  depicted by correlation matrix
## was because the relationship is no-linear.

###Heatmap
palette = colorRampPalette(c( "pink", "yellow"))(20)
heatmap(x = real_estate_cor, col = palette, symm = TRUE)

###PCA analysis 

##Principal component analysis (PCA) is a method used  for  dimensions reduction ,if the dataset
## has large number of variables.The results of this analysis helps interpreting 
##the  most significant components without loosing the vital information.This analysis 
## helps detect the outliers ,which can be really challenging to detect if the dataset is large.



Real <- cov(real_estate)
Real
sum(diag(Real))
r.eigen <- eigen(Real)
r.eigen
for (r in r.eigen$values) {
  print(r / sum(r.eigen$values))
}

plot(r.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
lines(r.eigen$values)
real_estate_pca <- prcomp(real_estate ,center = TRUE,scale. = TRUE)
real_estate_pca
summary(real_estate_pca)
library(ggfortify)
pca.plot <- autoplot(real_estate_pca, data = real_estate, colour ="red")
pca.plot
##A scree plot is a graphical tool used in the selection of the number of relevant components 
## by analyzing  eigenvalue  of your data.
#It dictates the number of  factors to be considered in a principal components analysis or a factor analysis.
##For this specific scree plot , first component shows the maximum cumlative percentage 

#PCA capture the essence of the data in a few principal components, which convey the most variation in the dataset.
# PC1+PC2+PC3+PC4+PC5+PC6+PC6+PC7+PC8=77%
##Generally in large datasets , there are just two or three components
##that is responsible for maximum variability , but this dataset has distributed variability ,meaning 

#the variation observed is spread across the dataset ,not just one or two variables.

library(factoextra)
fviz_pca_var(real_estate_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
###Potential outliers:

corEstate = cor(real_estate)
corEstate
scaled_real_estate = scale(real_estate)
eigenRes = eigen(corEstate)
eigenRes
outliers = -eigenRes$vectors[,1:2]
colnames(outliers) = c("PC1", "PC2")
row.names(outliers) = colnames(scaled_real_estate)
outliers
PC1 <- as.matrix(scaled_real_estate)%*% outliers[,1]
PC2 <- as.matrix(scaled_real_estate)%*% outliers[,2]
PC1
PC <- data.frame(Number = row.names(df_data_real_estate), PC1, PC2)
head(PC)
ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_text(aes(label = Number), size = 3) +
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components")

df_data_real_estate[12278,]
df_data_real_estate[9225,]
df_data_real_estate[7253,]
df_data_real_estate[3915,]
df_data_real_estate[8093,]
outliers_rows<- df_data_real_estate[c(12278,9225,7253,3915,8093),]
outliers_rows



##How to see variation in Price
p <- density(data_real_estate$price)
plot(p, main="Distribution of price",xlab="Prices")
polygon(p, col="aquamarine", border="blue")
s <- density(data_real_estate$grade)
plot(s, main="Distribution of grade",xlab="grade")
polygon(s, col="darkolivegreen4", border="blue")
dotchart(data_real_estate$sqft_lot,col="royalblue3" ,labels = "Lot Size")
max(data_real_estate$sqft_lot)
min(data_real_estate$sqft_lot)
mean(data_real_estate$sqft_lot)
head(data_real_estate$sqft_lot[order(-data_real_estate$sqft_lot)])
head(data_real_estate$sqft_lot[order(data_real_estate$sqft_lot)])
dotchart(data_real_estate$sqft_above,col="deeppink2" ,labels = "Sqft_above ")
max(data_real_estate$sqft_above)
min(data_real_estate$sqft_above)



dotchart(data_real_estate$ sqft_basement,col="coral1" ,labels = "Sqft_basement ")
dotchart(data_real_estate$sqft_living15,col="brown2" ,labels = "sqft_living15 ")
dotchart(data_real_estate$sqft_lot15,col="cadetblue1" ,labels = "sqft_lot15 ")
max(data_real_estate$sqft_living15)
tail(data_real_estate$sqft_living[order(data_real_estate$sqft_living)])
head(data_real_estate$sqft_living15[order(-data_real_estate$sqft_living15)])
tail(data_real_estate$sqft_living15[order(data_real_estate$sqft_living15)])


mean(data_real_estate$sqft_living)
median(data_real_estate$sqft_living)
dotchart(data_real_estate$price,col="darksalmon" ,labels = "Price")
min(data_real_estate$price)
abline(v=540088.1)
max(data_real_estate$price)
##The most expensive home in given dataset is $7700000
length(data_real_estate$price[data_real_estate$price > 540088.1])
##There are 30 houses above average house prices
head(data_real_estate$price[order(-data_real_estate$price)])
##The 6 most expensive house have the cost of 
## $7700000, $7062500, $6885000, $5570000, $5350000, $5300000
## which can be seen on the graph.

summary(data_real_estate$sqft_living)


######Exploring relation between bedroom and prices :
data_real_estate$price<-data_real_estate$price/100000


library(lattice)
plot(data_real_estate_price ~ bedrooms, data = data_real_estate, 
       type = "p", pch = 15,col="orchid3")
identify (data_real_estate_price ~data_real_estate$bedrooms )
##Looking into outliers and possible explanations:
data_real_estate[15871,]##one with 33 bedrooms
##Price is very low even with 33 bedrooms ,when condition=5 and grade =7
##Surprisingly ,bathroom = 1.75, and sqt_living =1620 and sqft_lot=6000
##Therefore, 33 semms to be typo error ,and there is probably only 3 bedrooms not 33
data_real_estate[7253,]##one with highest price
##This is most expensive house ,with bedrooms=6,bathrooms =8 and 
#good condition and grade.
data_real_estate[3915,]##one with second highest price
##Justified as every good condition ,grade and good no.of rooms
data_real_estate[9255,]##one with third highest price
###Justified as every good condition ,grade and good no.of rooms

every_bedroom_price<-tapply(data_real_estate$price,data_real_estate$bedrooms,sum)
every_bedroom_price
df_every_bedroom_price<-as.data.frame(every_bedroom_price)
max(df_every_bedroom_price)
##Conclusion:Most of the houses have three bedrooms.

##Regression Models 


full.model <- lm(price~ bedrooms +bathrooms   +
                   sqft_living   + sqft_lot +
                   condition + grade+sqft_above + sqft_basement+sqft_lot15+sqft_living15, 
                 data = data_real_estate)

full.model
summary(full.model)
#Price= -736500-45920*bedrooms-17890*bathrooms+256.7*sqft_living+58480*condition
# +105100*grade-61.38*sqft_above-0.6727*sqft_loft15+250.3*sqft_living15

##looking at the 
#p values ,almost all variables seems significant except sqft_lot
plot(x = data_real_estate$price, y = full.model$fitted.values)
abline(0,1)
#After removing the sqft_lot ,lets observe the fit 
full.model1 <- lm(price~ bedrooms +bathrooms   +
                   sqft_living    +
                   condition + grade+sqft_above + sqft_basement+, 
                 data = data_real_estate)

full.model1
summary(full.model1)
#Price= -736500-45920*bedrooms-17890*bathrooms+256.7*sqft_living+58480*condition
# +105100*grade-61.38*sqft_above-0.6727*sqft_loft15+250.3*sqft_living15

plot(x = data_real_estate$price, y = full.model1$fitted.values)

 ##After running the second model there is not much change in p values
##The fit of the line almost seems similar.

plot(density(rstudent(full.model1)))
##examining  the residuals to make sure our regression assumptions are satisfied
## and to spot problems with the fit
##Plot is bell shaped but skewed ??what does skewness show
##does it mean that error does not have constant variance

#### Residual plots for the  model, Ideally there should be *no
##pattern in the residuals

plot(full.model1$residuals ~ full.model1$fitted.values)
##  We observe funnel pattern  which indicates non-constant variance

plot(full.model1$residuals ~  data_real_estate$bedrooms)
plot(full.model1$residuals ~  data_real_estate$bathrooms)
plot(full.model1$residuals ~  data_real_estate$sqft_living)
plot(full.model1$residuals ~  data_real_estate$condition)
plot(full.model1$residuals ~  data_real_estate$grade)
plot(full.model1$residuals ~  data_real_estate$sqft_basement)
plot(full.model1$residuals ~  data_real_estate$sqft_lot15)
plot(full.model1$residuals ~  data_real_estate$sqft_living15)

##How to improve the model

full.model <- lm(price~ id+new_date+bedrooms +bathrooms   +
                   sqft_living   + sqft_lot +floors+waterfront+view+
                   condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                 data = data_real_estate)

full.model
summary(full.model)
#Multiple R-squared:  0.7011

##Removing few variables one by one ,first i will remove floors

full.model1 <- lm(price~ id+new_date+bedrooms +bathrooms   +
                   sqft_living   + sqft_lot +waterfront+view+
                   condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                 data = data_real_estate)

full.model1
summary(full.model1)

#Multiple R-squared:  0.7011,therefore not much change 

#Removing sqft_lot

full.model2 <- lm(price~ id+new_date+bedrooms +bathrooms   +
                    sqft_living   +waterfront+view+
                    condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                  data = data_real_estate)

full.model2
summary(full.model2)

#Multiple R-squared:  0.701,improved very little
#Removing id

full.model3 <- lm(price~ new_date+bedrooms +bathrooms   +
                    sqft_living   +waterfront+view+
                    condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                  data = data_real_estate)

full.model3
summary(full.model3)
#Multiple R-squared:  0.7009


##Removing outliers , analysis obtained from PCA analysis that row
##12278,9225,7253,3915,8093 were the potential outliers 

dim(data_real_estate)
new_data<- data_real_estate[c(-12278,-9225,-7253,-3915,-8093), ]
dim(new_data)


improved.model <- lm(price ~ new_date+bedrooms +bathrooms   +
                       sqft_living   +waterfront+view+
                       condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                     data = new_data)
improved.model
summary(improved.model)



##Multiple R-squared:  0.7023, the improvement in R-squared value
##is very small, hence outliers do not play play significant role
##in the variation of the dataset.
 

## Notice that bedrooms ,  sqft_lot ,zipcode, yr_built has negative coefficent 
## let's check for multicollinearity by making a correlation matrix

cor(data_real_estate[, c("bedrooms", "sqft_lot", "zipcode","yr_built")])

##The results shows that variables are not correlated ,hence 
#we don't need to drop them 

final.model<-lm(price ~ new_date+bedrooms +bathrooms   +
                 sqft_living   +waterfront+view+
                 condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
               data = new_data)

summary(final.model)
##Multiple R-squared:  0.7023


step.model <- step(final.model)

summary(step.model)
##Multiple R-squared:  0.7023
## Final model/improved model that we bulit shows same R squared values
##This shows that model we bulit was most efficient ,let's do futhur
##analysis by making sure that our model doesn't overfits the data
##We would do this analysis by using  lasso2 packages, we would come 
## up with different bounds to constrain the variables,till the most 
## important variables stand out and hold stable numbers.

library(lasso2)

sum(abs(full.model$coef[-1]))
###1630605
l1ce(price ~ new_date+bedrooms +bathrooms +sqft_living+waterfront+view+condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
     data = new_data,
     standardize = FALSE, absolute.t = TRUE, bound = 1640000) ## A large bound

l1ce(price ~ new_date+bedrooms +bathrooms   +sqft_living   +waterfront+view+condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
     data = new_data,
     standardize = FALSE, absolute.t = TRUE, bound = 1620000) ## A smaller bound

l1ce(price ~ new_date+bedrooms +bathrooms   +sqft_living   +waterfront+view+condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
     data = new_data,
     standardize = FALSE, absolute.t = TRUE, bound = 160000) ## Still smaller

l1ce(price ~ new_date+bedrooms +bathrooms   +sqft_living   +waterfront+view+condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
     data = new_data,
     standardize = FALSE, absolute.t = TRUE, bound = 1500000) ## Still smaller

l1ce(price ~ new_date+bedrooms +bathrooms   +sqft_living   +waterfront+view+condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
     data = new_data,
     standardize = FALSE, absolute.t = TRUE, bound = 1400000) ## Still smaller
###Since the coefficent value is very high , I would check and change
##the classes to "numeric" type.

colnames(new_data)
class(new_data$sqft_lot)
new_data$new_date<-as.numeric(new_data$new_date)
class(new_data$new_date)
new_data$bedrooms<-as.numeric(new_data$bedrooms)
class(new_data$bedrooms)
new_data$sqft_living<-as.numeric(new_data$sqft_living)
class(new_data$sqft_lot)
new_data$sqft_lot<-as.numeric(new_data$sqft_living)
class(new_data$sqft_lot)
new_data$waterfront<-as.numeric(new_data$waterfront)
class(new_data$waterfront)
new_data$view<-as.numeric(new_data$view)
class(new_data$view)
new_data$condition<-as.numeric(new_data$condition)
class(new_data$condition)
new_data$grade<-as.numeric(new_data$grade)
class(new_data$grade)
new_data$sqft_above<-as.numeric(new_data$sqft_above)
class(new_data$sqft_above)
new_data$sqft_basement<-as.numeric(new_data$sqft_basement)
class(new_data$sqft_basement)
new_data$yr_built<-as.numeric(new_data$yr_built)
class(new_data$yr_built)
new_data$yr_renovated<-as.numeric(new_data$yr_renovated)
class(new_data$yr_renovated)
new_data$zipcode<-as.numeric(new_data$zipcode)
class(new_data$zipcode)
new_data$sqft_living15<-as.numeric(new_data$sqft_living15)
class(new_data$sqft_living15)
new_data$sqft_lot15<-as.numeric(new_data$sqft_lot15)
class(new_data$sqft_lot15)
final.model<-lm(price ~ new_date+bedrooms +bathrooms   +
                  sqft_living   +waterfront+view+
                  condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                data = new_data)

summary(final.model)



sum(abs(final.model$coef[-1]))

##Even after changing the the classes , the coefficent is very high
##Hence, trying out different method , I used excel to randomly select 
##70000 rows out of 21000 rows, so that I can lower the sum of coeff.



kc_data_sample<- read.csv(file.choose(),header=T)

dim(kc_data_sample)

final1.model<-lm(price ~bedrooms +bathrooms   +
                  sqft_living   +waterfront+view+
                  condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
                data = kc_data_sample)

summary(final1.model)

sum(abs(final1.model$coef[-1]))



library(lasso2)

sum(abs(full.model$coef[-1]))

##1523413
##Even after reducing the data  I still get very high values of coeff.


l1ce(price ~ bedrooms +bathrooms +sqft_living+waterfront+view+condition + grade+sqft_above + yr_built+yr_renovated+zipcode+lat+long+sqft_lot15+sqft_living15, 
     data = new_data,
     standardize = FALSE, absolute.t = TRUE, bound) 



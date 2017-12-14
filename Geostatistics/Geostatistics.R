#install.packages("rgdal", dependencies = TRUE) install.packages("raster", dependencies = TRUE) install.packages("RStoolbox", dependencies = TRUE) install.packages("ggplot2", dependencies = TRUE) install.packages("grid", dependencies = TRUE) install.packages("gridExtra", dependencies = TRUE) install.packages("reshape", dependencies = TRUE) install.packages("rasterVis", dependencies = TRUE)
#install.packages("rgdal", dependencies = TRUE)
#install.packages("raster", dependencies = TRUE)
#install.packages("RStoolbox", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("grid", dependencies = TRUE)
#install.packages("gridExtra", dependencies = TRUE)
#install.packages("reshape", dependencies = TRUE)
#install.packages("rasterVis", dependencies = TRUE)

# Load the installed packages.
library(sp)
library(rgdal)
library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)
library(grid)
library(reshape)
library(rasterVis)

#setting the work folder
setwd("C:\\Users\\owner\\Desktop\\UT_ML_Lexture_Dec_2017\\Datasets\\Harare\\Single_Date22_06_84\\LandsatTM_Only")

#Create a list of raster bands that will be used for classification.
rasList=list.files(getwd(),pattern="img$", full.names=TRUE)

#Combine or stack the raster layers, which you listed before
rasVars<- stack(rasList)

#After loading and creating a raster stack, the next step is to 
#check the attributes and dimensions of the Landsat5 TM image. 
rasVars


#Plot the Landsat5 TM image as single bands using plot(). 
TM5_22_06_84 <- plot(rasVars)

#Display the Landsat TM image in false or true colour (Figure 2.1) using plotRGB(). > 
TM5_22_06_84 <- plotRGB(rasVars, r=5, g=4, b=3, stretch="lin")

#Load the point shapefile that contains the whole training datasets using readOGR().
ta_data <- readOGR(getwd(), "TA_1984")
str(ta_data)# Check the attributes of the point shapefile.

#Use the training point shape file to extract the Landsat reflectance values for each band. 
#This may take a bit of time depending on your computer memory. Be patient!

# Assign raster values to training data
ta<-as.data.frame(extract(rasVars, ta_data))
ta_data@data=data.frame(ta_data@data,ta[match(rownames(ta_data@data), rownames(ta)),])

#Take a look at the structure of the whole training dataset using str().
#This gives us an overview of the dataset.
str(ta_data@data)# Check the attributes of the training data

#Prepare training and test datasets
#First, let us start by installing and loading the following packages that we will use from now onwards.

# Install the following packages
#install.packages("caretEnsemble", dependencies = TRUE)
#install.packages("kknn", dependencies = TRUE)
#install.packages("ranger", dependencies = TRUE)
#install.packages("maptree", dependencies = TRUE)

library(caret)
library(caretEnsemble)
library(e1071)
library(kknn)
library(plyr)
library(ranger)
library(rpart)
library(nnet)
library(kernlab)
library(maptree)

#Generally, it is recommended to randomly partition the whole training dataset into training,
#validation and testing datasets. In this workbook, we will use the training and testing datasets only.
#First, we need to set a pre-defined value using set.seed() so that your results are repeatable.
#This means that another person can get the same results if he or she runs the script or model.
#Choose any number you like to set the seed.

hre_seed<- 27
set.seed(hre_seed)

#Next, we split the dataset into training/test datasets using createDataPartition() 
#from the Caret package. In this exercise, 60% of training datasest will be used for training,
#while 40% will be used for testing or accuracy assessment.
inTraining<- createDataPartition(ta_data@data$Cl1984, p = .60, list = FALSE)
training<- ta_data@data[ inTraining,]
testing <- ta_data@data[-inTraining,]

#Check the summary statistics (training and test datasets) Once again,
#let us check the distributions of the training and testing dataset using summary().

summary(training)
summary(testing)

#Visualization While summary() gives useful summary statistics of both the training and testing datasets,
#we need to further our training datasets before we proceed. Graphs provide a useful way of understanding
#our training dataset before we begin building the machine learning model or performing analysis our gives us.
#Here, we are going to use featurePlot() (which is a wrapper for different lattice plots) and ggplot2()
#in order to visualize the training dataset.

#First, let’s display density estimation plots (density plots) for each attribute by class value using 
#featurePlot(). The density plots summarize the distribution of the data. This highlights useful
#structures like linear separability of attribute values into classes

featurePlot(x = training[, 2:7], 
            y = training$Cl1984, 
            plot = "density", 
            labels=c("Reflectance", "Density distribution"), 
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            layout = c(3, 2), 
            auto.key = list(columns = 3))

#Second, let us display the box and whisker plots for each attribute by class value using featurePlot().
#This summarizes the spread of attributes. Note that box plots summarizes the distribution of a given 
#attribute by showing a box for the 25th and 75th percentile, a line in the box for the 50th
#percentile (median) and a dot for the mean. The whiskers show 1.5 times the height of
#the box (i.e., Inter Quartile Range) that indicate the expected range of the data.
#Data beyond those whiskers is assumed to be an outlier and marked with a dot

featurePlot(x = training[, 2:7], y = training$Cl1984, plot = "box",
            scales = list(y = list(relation = "free"),
                          x = list(rot = 90)),
            layout = c(3,2),
            auto.key = list(colmns = 2))

#Last but not least, let us check the correlation between two bands in relation to the training
#dataset using ggplot().
Band1_2<-ggplot(data = ta_data@data, aes(Hre_costz22_6_84.1, Hre_costz22_6_84.2)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 2")
Band1_3<-ggplot(data = ta_data@data, aes(Hre_costz22_6_84.1, Hre_costz22_6_84.3)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 3")
Band1_4<-ggplot(data = ta_data@data, aes(Hre_costz22_6_84.1, Hre_costz22_6_84.4)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 4")
Band1_5<- ggplot(data = ta_data@data, aes(Hre_costz22_6_84.1, Hre_costz22_6_84.5)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 5")
Band1_7<-ggplot(data = ta_data@data, aes(Hre_costz22_6_84.1, Hre_costz22_6_84.6)) + geom_point(aes(shape= Cl1984, colour= Cl1984)) + labs(x="Band 1", y = "Band 7")

#To display the correlation plots, use grid.arrange() so that all the plots appear on the same sheet
grid.arrange(Band1_2, Band1_3, Band1_4, Band1_5, Band1_7, ncol=2) #use package gridExtra

#Set-up model tuning parameters After that, we are going to use trainControl()
#from Caret package to evaluate -using resampling- the effect of model tuning parameters on performance.
#This will also choose the “optimal” model across these parameters, and estimate model performance from a
#training set.

fitControl<- trainControl(method = "repeatedcv", number = 3, repeats = 3)

#Note that the k-fold cross-validation repeats the construction of the model on different subsets of
#the available training data and then evaluates the model only on unseen data during model building

#Train the machine learning models.
#Now that we have finished setting up the model tuning parameters,
#let’s train our first model. We will start to train the k-Nearest Neighbors model as shown below:

set.seed(hre_seed)
knnFit<- train(Cl1984 ~ ., data = training,
               method = "kknn",
               trControl = fitControl)

#Use the print function to check the model results.
print(knnFit)

#After that, we can display the model training performance based on overall accuracy and kappa accuracy.
trellis.par.set(caretTheme()) # Overall accuracy
plot(knnFit)

trellis.par.set(caretTheme()) # Kappa accuracy
plot(knnFit, metric = "Kappa")

#Next, check the parameters of the best model and use it for prediction
knnFit$finalModel

#Next, let us test the model based on new unseen data (that is, the test data).
#First, we use the model to predict and then we build a confusion matrix.
pred_knnFit<- predict(knnFit, newdata = testing)

#Check the Confusion matrix (incl. confidence interval) on test data.
confusionMatrix(data = pred_knnFit, testing$Cl1984)

#Next, let's train the Artificial Neural Network (ANN) Model.
set.seed(hre_seed)
annFit <- train(Cl1984 ~ ., data = training,
                method = "nnet",
                trControl = fitControl)

#Use the print function to check the model results.
print(annFit)
#Neural Network

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were size = 5 and decay = 1e-04.

#After that, we can display the model training performance based on overall accuracy and kappa accuracy.
trellis.par.set(caretTheme()) # Overall accuracy
plot(annFit)

trellis.par.set(caretTheme()) # Kappa accuracy
plot(annFit, metric = "Kappa")

#Next, check the parameters of the best model and use it for prediction.
annFit$finalModel

#Next, let us test the model based on new unseen data (that is, the test data).
#First, we use the model to predict and then we build a confusion matrix.
pred_annFit<- predict(annFit, newdata = testing)

#Check the Confusion matrix (incl. confidence interval) on test data.
confusionMatrix(data = pred_annFit, testing$Cl1984)

#Next, train the Classification and Regression Trees (CART) model.
set.seed(hre_seed)
cart_model<-train(Cl1984~.,data=training, method="rpart",
                  trControl=fitControl)

#Use the print function to check the model results.
print(cart_model)
#CART

#After that, we can display the model training performance based on overall accuracy and kappa accuracy.
trellis.par.set(caretTheme()) # Overall accuracy
plot(cart_model)

trellis.par.set(caretTheme()) # Kappa accuracy
plot(cart_model, metric = "Kappa")

#Next, check the parameters of the best model and use it for prediction.
cart_model$finalModel

#Next, plot the decision trees using draw.tree().
draw.tree(cart_model$finalModel, cex=0.5, nodeinfo=TRUE, col=gray(0:8 / 8))

#Next, let us test the model based on new unseen data (that is, the test data).
#First, we use the model to predict and then we build a confusion matrix.
pred_cart<- predict(cart_model,newdata = testing, na.action = na.pass)
# Note predict willwill not work if there are missing values. Therefore use na.action = na.pass)

#Check the Confusion matrix (incl. confidence interval) on test data.
confusionMatrix(data = pred_cart, testing$Cl1984)

#Next,train the Support Vector Machine (SVM) model
set.seed(hre_seed)
svm_model<-train(Cl1984~.,data=training,
                 method = "svmRadial",
                 trControl = fitControl,
                 preProc = c("center", "scale"),
                 tuneLength = 3)

#Use the print function to check the model results.
print(svm_model)

#After that, we can display the model training performance based on overall accuracy and kappa accuracy.
trellis.par.set(caretTheme()) # Overall accuracy
plot(svm_model)

trellis.par.set(caretTheme()) # kappa accuracy
plot(svm_model, metric = "Kappa")

#Next, check the parameters of the best model and use it for prediction.
svm_model$finalModel

#You can also display variable importance using varImp().
svm_varImp <- varImp(svm_model, compete = FALSE)
plot(svm_varImp)

#Next, let us test the model based on new unseen data (that is, the test data).
#First, we use the model to predict and then we build a confusion matrix.
pred_svm<- predict(svm_model, newdata = testing)

#Check the confusion matrix (incl. confidence interval) on test data.
# Confusion matrix (incl. confidence interval) on test data 
confusionMatrix(data = pred_svm, testing$Cl1984)

#Finally, train the Random Forest (RF) model
set.seed(hre_seed)
rf_model<-train(Cl1984~.,data=training, method="rf",
                trControl=fitControl,
                prox=TRUE,
                fitBest = FALSE,
                returnData = TRUE)

#Use the print functions to check the model results.
print(rf_model)

#After that, we can display the model training performance based on overall accuracy and kappa accuracy.
trellis.par.set(caretTheme()) # Overall accuracy
plot(rf_model)

trellis.par.set(caretTheme()) # kappa accuracy
plot(rf_model,metric = "Kappa")

#Next, check the parameters of the best model and use it for prediction.
rf_model$finalModel

#You can also display variable importance using varImp().
rf_varImp <- varImp(rf_model, compete = FALSE)
plot(rf_varImp)

#Next, let us test the model based on new unseen data (that is, the test data).
#First, we use the model to predict and then we build a confusion matrix.
pred_rf<- predict(rf_model$finalModel,newdata = testing)

#Check the confusion matrix (incl. confidence interval) on test data.
confusionMatrix(data = pred_rf, testing$Cl1984)

#Now let's compare all models using resamples().
# Compare all models
resamps <- resamples(list(kknn = knnFit,
                          nnet = annFit,
                          rpart = cart_model,
                          kernlab = svm_model,
                          e1071 = rf_model))
resamps

#Use bwplot() in order to display the results in graphic form.
#Not this a comparison of the model training performance.
bwplot(resamps, layout = c(3, 1))

#Now, let's perform land use/cover classification using all models.
#Be patience, since it takes time to run all the classifications.
timeStart<- proc.time() # measure computation time
LC_knnFit_84 <-predict(rasVars,knnFit)

LC_ann_84 <-predict(rasVars,annFit)
LC_cart_84 <-predict(rasVars,cart_model)

LC_svm_84 <-predict(rasVars,svm_model)

LC_rf_84 <-predict(rasVars,rf_model)
proc.time() - timeStart # user time and system time

#Let's compare all the land use/cover classification based on different machine learning techniques.
#We are going to use the ggplot2() to display all land use/cover maps on the same sheet.
#Write the following script.

LC_knnFit_84a <- gplot(LC_knnFit_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"), name= "Land Cover") + ggtitle("K Nearest Neighbour Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()

LC_ann_84a <- gplot(LC_ann_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"), name= "Land Cover") + ggtitle("Artificial Neural Network Classification") + theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()

LC_cart_84a <- gplot(LC_cart_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Green Spaces", "Urban" )))) + scale_fill_manual(values = c("yellow", "green3", "red"), name= "Land Cover") + ggtitle("Decision Trees Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()

LC_svm_84a <- gplot(LC_svm_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) +scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"), name= "Land Cover") + ggtitle("Support Vector Machine Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()

LC_rf_84a <- gplot(LC_rf_84) + geom_raster(aes(fill = factor(value, labels=c("Agriculture", "Bareland", "Green Spaces", "Urban", "Water")))) + scale_fill_manual(values = c("yellow", "grey", "green3", "red", "blue3"), name= "Land Cover") + ggtitle("Random Forest Classification") +theme(plot.title = element_text(lineheight=.4, face="bold")) + coord_equal()

#Finally, let's us arrange the mapped results on the same sheet using grid.arrange().
grid.arrange(LC_knnFit_84a, LC_ann_84a, LC_cart_84a, LC_svm_84a, LC_rf_84a, ncol=2)
#use package gridExtra

# Library
library(jpeg)
library(randomForest)
library(caTools)
library(pROC)
library(adabag)
library(caret)
library(kernlab)
library(e1071)

# set working directory
setwd("C:/Users/Javier/Documents/2-D Especializacion en Ciencia de Datos/8 Practical Machine Learning/Unidad 4/Proyecto")
getwd()

# read files
training <- read.csv("train.csv")
dim(training)
testing <- read.csv("test.csv")
dim(testing)

# split files
set.seed(1)
sample <- sample.split(training, SplitRatio = 0.80)
train <- subset(training, sample == TRUE)
test <- subset(training, sample == FALSE)
validate <- testing
c(dim(train), dim(test), dim(validate))

# view correlation between independent variables
attach(train)

temp <- as.data.frame(cbind(roll_belt, pitch_belt, yaw_belt, total_accel_belt, gyros_belt_x, gyros_belt_y, gyros_belt_z,
        accel_belt_x, accel_belt_y, accel_belt_z, magnet_belt_x, magnet_belt_y, magnet_belt_z,
        roll_arm, pitch_arm, yaw_arm, total_accel_arm, gyros_arm_x, gyros_arm_y, gyros_arm_z, accel_arm_x,
        accel_arm_y, accel_arm_z, magnet_arm_x, magnet_arm_y, magnet_arm_z, roll_dumbbell, pitch_dumbbell,
        yaw_dumbbell, total_accel_dumbbell, gyros_dumbbell_y, gyros_dumbbell_z, accel_dumbbell_x,
        accel_dumbbell_y, accel_dumbbell_z, magnet_dumbbell_x, magnet_dumbbell_y, magnet_dumbbell_z,
        roll_forearm, pitch_forearm, yaw_forearm, gyros_forearm_x, gyros_forearm_y, gyros_forearm_z,
        accel_forearm_x, accel_forearm_y, accel_forearm_z, magnet_forearm_x, magnet_forearm_y, magnet_forearm_z))

jj <- readJPEG("cor.jpg",native=TRUE)
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(jj,0,0,1,1)

# Random Forest
RF <- randomForest(classe ~ 
      pitch_belt
    + gyros_belt_z
    + magnet_belt_y
    + roll_arm
    + pitch_arm
    + yaw_arm
    + gyros_arm_x
    + accel_arm_x
    + accel_arm_y
    + accel_arm_z
    + magnet_arm_x
    + magnet_arm_y
    + magnet_arm_z
    + roll_dumbbell
    + pitch_dumbbell
    + yaw_dumbbell
    + total_accel_dumbbell
    + accel_dumbbell_y
    + accel_dumbbell_z
    + magnet_dumbbell_y
    + magnet_dumbbell_z
    + roll_forearm
    + pitch_forearm
    + gyros_forearm_y
    + accel_forearm_x
    + accel_forearm_y
    + accel_forearm_z
    + magnet_forearm_x
    + magnet_forearm_y
    + magnet_forearm_z,
    data = train,
    ntree = 100,
    mtry = 1)

# importance(RF)

R <- as.character(as.factor(test$classe))
P_RF <- as.character(predict(RF, test, type = "class"))
tabla <- table(R,P_RF)
accuracy <- round(sum(diag(tabla)) / sum(tabla),4)
modelo <- "RandomForest"
modeloRF <- cbind(modelo, accuracy  )



# AdaBoost
      Ada <-  boosting(classe ~ 
               pitch_belt
             + gyros_belt_z
             + magnet_belt_y
             + roll_arm
             + pitch_arm
             + yaw_arm
             + gyros_arm_x
             + accel_arm_x
             + accel_arm_y
             + accel_arm_z
             + magnet_arm_x
             + magnet_arm_y
             + magnet_arm_z
             + roll_dumbbell
             + pitch_dumbbell
             + yaw_dumbbell
             + total_accel_dumbbell
             + accel_dumbbell_y
             + accel_dumbbell_z
             + magnet_dumbbell_y
             + magnet_dumbbell_z
             + roll_forearm
             + pitch_forearm
             + gyros_forearm_y
             + accel_forearm_x
             + accel_forearm_y
             + accel_forearm_z
             + magnet_forearm_x
             + magnet_forearm_y
             + magnet_forearm_z,
             data = train,
             mfinal = 10,
             coeflearn = "Breiman")
        
      R <- as.character(as.factor(test$classe))
      P_ADA <- predict(Ada, test)$class
      tabla <- table(R,P_ADA)
      accuracy <- round(sum(diag(tabla)) / sum(tabla),4)
      modelo <- "AdaBoost"
      modeloAda <- cbind(modelo, accuracy)
      
      
# Naive Bayes
            NB <-  naiveBayes(classe ~ 
                          pitch_belt
                        + gyros_belt_z
                        + magnet_belt_y
                        + roll_arm
                        + pitch_arm
                        + yaw_arm
                        + magnet_arm_y
                        + magnet_arm_z
                        + roll_dumbbell
                        + pitch_dumbbell
                        + yaw_dumbbell
                        + total_accel_dumbbell
                        + accel_dumbbell_y
                        + accel_dumbbell_z
                        + magnet_dumbbell_y
                        + magnet_dumbbell_z
                        + roll_forearm
                        + pitch_forearm
                        + accel_forearm_x
                        + accel_forearm_y
                        + accel_forearm_z
                        + magnet_forearm_x
                        + magnet_forearm_y
                        + magnet_forearm_z,
                        data = train)
      
            summary(Ada)
      
            R <- as.character(as.factor(test$classe))
            P_NB <- as.character(predict(NB, test))
            tabla <- table(R,P_NB)
            accuracy <- round(sum(diag(tabla)) / sum(tabla),4)
            modelo <- "Naive Bayes"
            modeloNB <- cbind(modelo, accuracy)
   
            
            
              

Y <- test$classe
X1 <- P_RF
X2 <- P_ADA
X3 <- P_NB
temp <- as.data.frame(cbind(Y,X1,X2,X3))
BAG <- randomForest(Y ~ X1 + X2 + X3, data = temp, ntree = 100, mtry = 1)

R <- as.character(as.factor(test$classe))
P <- as.character(predict(BAG, temp, type = "class"))
tabla <- table(R,P)
accuracy <- round(sum(diag(tabla)) / sum(tabla),4)
modelo <- "Bagging1"
modeloBag <- cbind(modelo, accuracy)

Y <- test$classe
X1 <- P_RF
X2 <- P_ADA
temp <- as.data.frame(cbind(Y,X1,X2))
BAG2 <- randomForest(Y ~ X1 + X2, data = temp, ntree = 100, mtry = 1)

R <- as.character(as.factor(test$classe))
P <- as.character(predict(BAG2, temp, type = "class"))
tabla <- table(R,P)
accuracy <- round(sum(diag(tabla)) / sum(tabla),4)
modelo <- "Bagging2"
modeloBag2 <- cbind(modelo, accuracy)

# Resumme

rbind(modeloRF, modeloAda, modeloNB, modeloBag, modeloBag2)




# Apply winning model to validation dataset
predict(RF, validate, type = "class")
table(predict(RF, validate, type = "class"))





  





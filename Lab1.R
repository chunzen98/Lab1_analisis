rm(list = ls()) # borrar todos los datos

# install.packages("ggplot2")
# install.packages("corrplot")
# install.packages("plyr")

library(ggplot2)
library(corrplot)
library(plyr)

# Se obtiene la data de heart disease desde https://archive.ics.uci.edu
processed.cleveland = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", dec=".", sep=",")
processed.hungarian = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.hungarian.data", dec=".", sep=",")


### Only 14 attributes used ###
# 1. age
# 2. sex
# 3. cp: chest pain type
# -- Value 1: typical angina
# -- Value 2: atypical angina
# -- Value 3: non-anginal pain
# -- Value 4: asymptomatic
# 4. trestbps: resting blood pressure (in mm Hg on admission to the hospital)
# 5. chol: serum cholestoral in mg/dl
# 6. fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)
# 7. restecg: resting electrocardiographic results
# -- Value 0: normal
# -- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
# -- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria
# 8. thalach: maximum heart rate achieved
# 9. exang: exercise induced angina (1 = yes; 0 = no)
# 10. oldpeak = ST depression induced by exercise relative to rest
# 11. slope: the slope of the peak exercise ST segment
# -- Value 1: upsloping
# -- Value 2: flat
# -- Value 3: downsloping
# 12. ca: number of major vessels (0-3) colored by flourosopy
# 13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
# 14. num: diagnosis of heart disease (angiographic disease status)
# -- Value 0: < 50% diameter narrowing
# -- Value 1: > 50% diameter narrowing

# Se agregan nombre a cada una de las columnas
names(processed.cleveland) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
names(processed.hungarian) <- c("age","sex","cp","trestbs","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")


# Se cambian los datos de la columna ca y thal con valor "?" a NA
processed.cleveland$ca[processed.cleveland$ca=="?"]<-NA;
processed.cleveland$thal[processed.cleveland$thal=="?"]<-NA;

# processed.cleveland<-transform(processed.cleveland, thal = as.numeric(thal), ca = as.factor(ca));

# Se borran los datos con NA's
processed.cleveland <- processed.cleveland[complete.cases(processed.cleveland), ]

# Duplican los datos para tener version factor y numerico
processed.cleveland.num <- processed.cleveland

# Se cambian los valores numericos a valores cualitativos
processed.cleveland$sex[processed.cleveland$sex==0]<-"female";
processed.cleveland$sex[processed.cleveland$sex==1]<-"male";

processed.cleveland$cp[processed.cleveland$cp==1] <- "typical angina";
processed.cleveland$cp[processed.cleveland$cp==2] <- "atypical angina";
processed.cleveland$cp[processed.cleveland$cp==3] <- "non-anginal pain";
processed.cleveland$cp[processed.cleveland$cp==4] <- "asymptomatic";

processed.cleveland$fbs[processed.cleveland$fbs==0] <- "fbs < 120 mg/dl";
processed.cleveland$fbs[processed.cleveland$fbs==1] <- "fbs > 120 mg/dl";

processed.cleveland$restecg[processed.cleveland$restecg==0] <- "normal";
processed.cleveland$restecg[processed.cleveland$restecg==1] <- "ST-T wave abnormality";
processed.cleveland$restecg[processed.cleveland$restecg==2] <- "Estes' criteria";

processed.cleveland$exang[processed.cleveland$exang==0] <- "no";
processed.cleveland$exang[processed.cleveland$exang==1] <- "yes";

processed.cleveland$slope[processed.cleveland$slope==1] <- "upsloping";
processed.cleveland$slope[processed.cleveland$slope==2] <- "flat";
processed.cleveland$slope[processed.cleveland$slope==3] <- "downsloping";

processed.cleveland$thal <- revalue(processed.cleveland$thal, c("3.0"="normal", "6.0"="fixed defect", "7.0"="reversable defect"));

processed.cleveland$ca <- revalue(processed.cleveland$ca, c("0.0" = "No major vessel", "1.0" = "1 major vessel", "2.0" = "2 major vessels", "3.0" = "3 major vessels"))

# Se reemplaza los valores de num > 0 a 1
processed.cleveland$num[processed.cleveland$num > 0]  <- 1;
processed.cleveland$num[processed.cleveland$num==1] <- "Present disease";
processed.cleveland$num[processed.cleveland$num==0] <- "No present disease";


# Se cambia el tipo de dato de la columna ca y thal a numericos
# processed.cleveland<-transform(processed.cleveland, ca = as.numeric(ca), thal = as.numeric(thal));
processed.cleveland<-transform(processed.cleveland, ca = factor(ca), thal = factor(thal), sex = factor(sex), 
                               cp = factor(cp), fbs = factor(fbs),restecg = factor(restecg), exang = factor(exang), slope = factor(slope), num = factor(num));


### Resumen de datos processed.cleveland ###
summary(processed.cleveland)




# Se agrega una seed
set.seed(112)

# Modelo regresion logistica multiple
modelo_glm <- glm(num ~ sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, 
                  data = processed.cleveland, family = "binomial")

# Resumen modelo regresion logistica
summary (modelo_glm)

# Matriz de correlacion de los datos de data
matriz_cor <- cor(processed.cleveland[, sapply(processed.cleveland, is.numeric)])
matriz_cor <- round(matriz_cor, 2)

# Grafico correlaciones
corrplot(matriz_cor, order = "hclust", 
         tl.col = "black", tl.srt = 45)


# Grafico prueba
ggplot(data = processed.cleveland, aes(x = as.factor(num), y = age, colour = as.factor(sex))) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom")



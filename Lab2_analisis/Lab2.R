rm(list = ls()) # borrar todos los datos

 # install.packages("ggplot2")
 # install.packages("corrplot")
 # install.packages("plyr")
 # install.packages("ggpubr")
 # install.packages("factoextra")
 # install.packages("cluster")

library(ggplot2)
library(corrplot)
library(plyr)
library(ggpubr)
library(factoextra)
library(cluster)

# Lectura de datos ---------------------------------

# Se obtiene la data de heart disease desde https://archive.ics.uci.edu
processed.cleveland = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", dec=".", sep=",")

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
processed.cleveland$num[processed.cleveland$num==0] <- "No present disease";
processed.cleveland$num[processed.cleveland$num==1] <- "Present disease";


# Se cambia el tipo de dato de la columna ca y thal a numericos
processed.cleveland<-transform(processed.cleveland, ca = factor(ca), thal = factor(thal), sex = factor(sex), 
                               cp = factor(cp), fbs = factor(fbs),restecg = factor(restecg), exang = factor(exang), slope = factor(slope), num = factor(num));


### Resumen de datos processed.cleveland ###
summary(processed.cleveland)



# Analisis de variables ---------------------------------

# Se agrega una seed
set.seed(112)

# Modelo regresion logistica multiple
modelo_glm <- glm(num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, 
                  data = processed.cleveland, family = "binomial")

# Resumen modelo regresion logistica
summary (modelo_glm)

# Matriz de correlacion de los datos de data
matriz_cor <- cor(processed.cleveland[, sapply(processed.cleveland, is.numeric)])
matriz_cor <- round(matriz_cor, 2)

# Grafico correlaciones
matriz_cor_plot <- corrplot(matriz_cor, order = "hclust", 
         tl.col = "black", tl.srt = 45)



# Grafico caja variables numericas
# Grafico age vs Num
ageVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = num,
                                y = age)) +
            geom_boxplot() + theme_bw() + labs(y = "Age", x = "Heart disease")

# Grafico testbps vs Num
testbpsVsNum <- ggplot(data = processed.cleveland,
                   mapping = aes(x = num,
                                 y = trestbps)) +
            geom_boxplot() + theme_bw() + labs(y = "Resting blood pressure", x = "Heart disease")

# Grafico chol vs Num
cholVsNum <- ggplot(data = processed.cleveland,
                   mapping = aes(x = num,
                                 y = chol)) +
            geom_boxplot() + theme_bw() + labs(y = "Serum cholestoral in mg/dl", x = "Heart disease")

# Grafico thalach vs Num
thalachVsNum <- ggplot(data = processed.cleveland,
                   mapping = aes(x = num,
                                 y = thalach)) +
            geom_boxplot() + theme_bw() + labs(y = "Maximum heart rate achieved", x = "Heart disease")

# Grafico oldpeak vs Num
oldpeakVsNum <- ggplot(data = processed.cleveland,
                   mapping = aes(x = num,
                                 y = oldpeak)) +
            geom_boxplot() + theme_bw() + labs(y = "ST depression induced by exercise relative to rest", x = "Heart disease")


# Grafico barra variables categoricas
#Grafico de Ca vs Num
caVsNum <- ggplot(data = processed.cleveland,
            mapping = aes(x = factor(ca),
                          fill = factor(num))) +
           geom_bar(position = 'dodge', stat = 'count') + theme_bw() + labs(x = "Number of major vessel", fill = "Heart disease")

#Grafico de cp vs Num
cpVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(cp),
                                fill = factor(num))) +
           geom_bar(position = 'dodge', stat = 'count') + theme_bw() + labs(x = "Chest pain", fill = "Heart disease")

#Grafico de sex vs Num
sexVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(sex),
                                fill = factor(num))) +
            geom_bar(position = 'dodge', stat = 'count')   + theme_bw() + labs(x = "Sex", fill = "Heart disease")

#Grafico de fbs vs Num
fbsVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(fbs),
                                fill = factor(num))) +
            geom_bar(position = 'dodge', stat = 'count')   + theme_bw() + labs(x = "Fasting blood sugar", fill = "Heart disease")

#Grafico de restcg vs Num
restecgVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(restecg),
                                fill = factor(num))) +
                geom_bar(position = 'dodge', stat = 'count')  + theme_bw() + labs(x = "Resting electrocardiographic results", fill = "Heart disease")

#Grafico de exang vs Num
exangVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(exang),
                                fill = factor(num))) +
              geom_bar(position = 'dodge', stat = 'count') + theme_bw() + labs(x = "Exercise induced angina", fill = "Heart disease")

#Grafico de slope vs Num
slopeVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(slope),
                                fill = factor(num))) +
              geom_bar(position = 'dodge', stat = 'count') + theme_bw() + labs(x = "Slope of the peak exercise ST segment", fill = "Heart disease")

#Grafico de thal vs Num
thalVsNum <- ggplot(data = processed.cleveland,
                  mapping = aes(x = factor(thal),
                                fill = factor(num))) +
             geom_bar(position = 'dodge', stat = 'count') + theme_bw() + labs(x = "thal", fill = "Heart disease")




# # Plot de los graficos caja (Variables numericas)
# plot(ageVsNum)
# plot(testbpsVsNum)
# plot(cholVsNum)
# plot(thalachVsNum)
# plot(oldpeakVsNum)
# 
# # Plot de los graficos barra (Variables categoricas)
# plot(caVsNum)
# plot(cpVsNum)
# plot(sexVsNum)
# plot(fbsVsNum)
# plot(restecgVsNum)
# plot(exangVsNum)
# plot(slopeVsNum)
# plot(thalVsNum)


# Test de chi-cuadrado con num y cada una de las variables, para verificar dependencia con variable discriminadora
chi_sexVsNum <- chisq.test(table(processed.cleveland$sex, processed.cleveland$num))
chi_cpVsNum <- chisq.test(table(processed.cleveland$cp, processed.cleveland$num))
chi_fbsVsNum <- chisq.test(table(processed.cleveland$fbs, processed.cleveland$num))
chi_restecgVsNum <- chisq.test(table(processed.cleveland$restecg, processed.cleveland$num))
chi_exangVsNum <- chisq.test(table(processed.cleveland$exang, processed.cleveland$num))
chi_slopeVsNum <- chisq.test(table(processed.cleveland$slope, processed.cleveland$num))
chi_caVsNum <- chisq.test(table(processed.cleveland$ca, processed.cleveland$num))
chi_thalVsNum <- chisq.test(table(processed.cleveland$thal, processed.cleveland$num))


# Clusterizacion ---------------------------------

# Primero se binarizan todas las variables categoricas.
processed.cleveland.bin <- cbind(age = scale(processed.cleveland$age), as.data.frame(cbind(with(processed.cleveland, model.matrix(~ sex + 0)))), as.data.frame(cbind(with(processed.cleveland, model.matrix(~ cp + 0)))), trestps = processed.cleveland$trestbps,  chol = processed.cleveland$chol, as.data.frame(cbind(with(processed.cleveland, model.matrix(~ fbs + 0)))), as.data.frame(cbind(with(processed.cleveland, model.matrix(~ restecg + 0)))), thalach = processed.cleveland$thalach, as.data.frame(cbind(with(processed.cleveland, model.matrix(~ exang + 0)))), oldpeak = processed.cleveland$oldpeak, as.data.frame(cbind(with(processed.cleveland, model.matrix(~ slope + 0)))), as.data.frame(cbind(with(processed.cleveland, model.matrix(~ ca + 0)))), as.data.frame(cbind(with(processed.cleveland, model.matrix(~ thal + 0)))), as.data.frame(cbind(with(processed.cleveland, model.matrix(~ num + 0)))));

# Se procede a crear una matriz de dissimilitud
matriz.dis = daisy(processed.cleveland.bin, metric = "gower", type = list(asymm = c(2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)))


# Para decidir la cantidad de cluster se utiliza el metodo silhouette
fviz_nbclust(processed.cleveland.bin, kmeans, method = "silhouette")

# Luego se genera el cluster utilizando la funcion pam, con un k = 2
clust = pam(matriz.dis, 2, diss = T)
clust$data = processed.cleveland.bin

#Se grafica el cluster
fviz_cluster(clust, palette = "jco", ggtheme = theme_minimal())

# ---------------------------------
###
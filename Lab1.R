rm(list = ls()) # borrar todos los datos

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

# Se cambia el tipo de dato de la columna ca y thal a numericos
processed.cleveland<-transform(processed.cleveland, ca = as.numeric(ca), thal = as.numeric(thal));
processed.cleveland<-transform(processed.cleveland, ca = as.numeric(ca), thal = as.numeric(thal));

# Se reemplazan los NA por el valor promedio de la columna correspondiente
processed.cleveland$ca[is.na(processed.cleveland$ca)]<-mean(processed.cleveland$ca, na.rm = TRUE);
processed.cleveland$thal[is.na(processed.cleveland$thal)]<-mean(processed.cleveland$thal, na.rm = TRUE);



### Resumen de datos processed.cleveland ###

summary(processed.cleveland)













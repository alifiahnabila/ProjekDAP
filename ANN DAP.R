#importing libraries
library(rmarkdown)
library(knitr)
library(dplyr)
library(ggplot2)
library(neuralnet)
library(iml)
library(rpart)
library(readr)
library(magrittr)
library(tibble)
library(skimr)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
library(DALEX)

#Loading Data
str(stroke_data)

#DATA FIKS
stroke_clean_data <- stroke_data[ , 2:12]
head(stroke_clean_data, 5)

#DATA CLEANING
##DATA TYPE CHECKING
###Interpretasi Data
####Edit Data 'BMI'
qty_na <- sum(stroke_data$bmi == "N/A")
qty_na
stroke_clean_data$bmi <- as.numeric(stroke_clean_data$bmi)
mean_value <- mean(stroke_clean_data$bmi, na.rm = TRUE)
print(mean_value)
stroke_clean_data$bmi[is.na(stroke_clean_data$bmi)] <- mean_value
head(stroke_clean_data$bmi, 20)
####Missing Value
anyNA(stroke_clean_data)
####Data Duplicate
duplicate_rows <- stroke_clean_data[duplicated(stroke_clean_data) | duplicated(stroke_clean_data, fromLast = TRUE), ]
print(duplicate_rows)

#DATA PROCESSING
##SAMPLING DATA
###Random Sampling
samplesize <- 0.60*nrow(stroke_clean_data)

set.seed(80)
index <- sample(seq_len(nrow(stroke_clean_data)), size = samplesize)

#membuat train dan test set
datatrain <- stroke_clean_data[index, ]
datatest <- stroke_clean_data[-index, ]

##SCALE DATA
###Scale data for NN
max <- apply(stroke_clean_data, 2, max)
min <- apply(stroke_clean_data, 2, min)
scaled <- as.data.frame(scale(stroke_clean_data, center = TRUE, scale = TRUE))
kable(head(scaled))
##error karna kategorikal##

###Dummy Data
####one-hot encoding pada kolom kategorikal
Dummy_data <- model.matrix(~ . - 1, data = stroke_clean_data)
head(Dummy_data, 5)

###Processing Data
Scaled2 <- as.data.frame(scale(Dummy_data, center = TRUE, scale = TRUE))
(head(Scaled2))

#MODELLING
##ANN dengan 'neuralnet'
###Partisi Data
trainNN <- Scaled2[index , ]
testNN <- Scaled2[-index , ]
head(trainNN, 5)
head(testNN, 5)
glimpse(trainNN)

##Membangun MOdel
set.seed(2)
tessssss <- neuralnet(stroke ~ ever_marriedYes + age43 + hypertension + avg_glucose_level + bmi + smoking_statussmokes, trainNN,
                      hidden = 3,
                      linear.output = T)
##Visualisasi ANN
plot(tessssss)

## PREDIKIS
### Prediction using neural network
predict_testNN1 <- compute(tessssss, testNN)
predict_testNN <- (predict_testNN1$net.result * (max(stroke_clean_data$stroke) - min(stroke_clean_data$stroke))) + min(stroke_clean_data$stroke)

##fig-height: 4
plot(datatest$stroke, predict_testNN, col='darkred', pch=16, ylab = "predicted rating", xlab = "STROKE")
abline(0,1)


## ROOT MEAN SQUARE
### Calculate Root Mean Square Error (RMSE)
RMSE.NN <- (sum((datatest$stroke - predict_testNN)^2) / nrow(datatest)) ^ 0.5
cat("Metrik RMSE: ", RMSE.NN)

`RMSE` memiliki nilai sekitar 0.384983. Ini berarti bahwa, secara rata-rata, perbedaan antara nilai prediksi model Anda dengan nilai aktual (dalam unit yang sama dengan data) adalah sekitar 0.384983. Semakin mendekati nol, semakin baik kinerja model yang di buat.

## CONCLUSION
### FEATURE INPORTANCE
#### fig-height: 4
X <- testNN[which(names(testNN) != "stroke")]
predictor <- Predictor$new(tessssss, data = X, y = testNN$stroke)

imp <- FeatureImp$new(predictor, loss = "mae")
plot(imp)

### RESULT IMP
imp$results

### SHAP
####fig-height: 4
shapley <- Shapley$new(predictor, x.interest = X[1, ])
shapley$plot()

## END

Terakhir.

#### Result IMP

`feature (fitur)`: Kolom ini berisi nama fitur-fitur yang dievaluasi dalam model. Ini adalah daftar fitur-fitur yang digunakan dalam model yang di buat.

`importance.05`: Ini adalah nilai penting fitur (feature importance) pada tingkat 5% (terendah) dalam distribusi. Nilai ini menunjukkan sejauh mana fitur tersebut penting dalam model saat melihat persentil terendah dari distribusi.

`importance`: Ini adalah nilai penting fitur pada tingkat tengah (median) dalam distribusi. Ini adalah ukuran pentingnya fitur dalam model saat melihat persentil tengah dari distribusi.

`importance.95`: Ini adalah nilai penting fitur pada tingkat 95% (tertinggi) dalam distribusi. Ini menunjukkan sejauh mana fitur tersebut penting dalam model saat melihat persentil tertinggi dari distribusi.

Dari table `IMP` di atas terlihat bahwa varible `ever_marriedYes` sangat penting dalam mempengaruhi model yang di buat, selanjutnya di ikuti `age` / Usia dan `BMI`.

#### Result SHEPLY

Dari nilai IMP di jelaskan bahwa `ever_marriedYes` dan `BMI` termasuk dalam bagian yang penting dalam model, namun di lihat dari Interpretasi plot `SHEPLY` kedua variable ini memiliki nilai yang negatif. artinya prediksi akan cenderung rendah.


Selanjutnya yang memiliki nilai shaply positif adalah variable : 
  
  - `avg_glucose_level`: average glucose level in blood

- `age`: age of the patient

maka dapat di simpulkan jika Client memiliki nilai Glucose (gula / Diabetes) dan usia yang cukup tinggi ini menjadi tanda untuk berdoa lebih banyak, karena peluang terserang stroke sangat tinggi.

#### KOREKSI

Dear Tutor,

jika ada kesalahan mohon kiranya di koreksi jika sempat.


Demikian di sampaikan, atas bantuan dan ilmunya di ucapkan terimakasih.

__REGARDS,__

`ALBANI`




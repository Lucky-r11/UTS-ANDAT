library(readr)
setwd("C:/Users/hp/Documents/R")
data <- read.csv("Data_UTSandat.csv")
data

nrow(data)
ncol(data)
str(data)
summary(data)

head(data)
tail(data)

nrow(data)       # Total rows
colSums(is.na(data)) # Missing values per column

# Pastikan semua library terpasang
install.packages(c("ggplot2", "dplyr", "tidyr"))  # Jika belum terpasang

# Memuat library yang diperlukan
library(ggplot2)
library(dplyr)
library(tidyr)

# Contoh dataset (gantikan dengan dataset Anda)
data <- data.frame(
  CourseCompletion = c(1, 0, 1, 1, 0, 1, 0, 0, 1, 0),
  TimeSpentOnCourse = c(12, 8, 15, 10, 5, 14, 6, 9, 11, 7),
  NumberOfVideosWatched = c(8, 4, 10, 6, 2, 9, 3, 5, 7, 4),
  NumberOfQuizzesTaken = c(3, 1, 4, 2, 0, 3, 1, 2, 3, 1)
)

# Mengubah data ke format panjang (long format)
data_long <- data %>%
  pivot_longer(
    cols = c("TimeSpentOnCourse", "NumberOfVideosWatched", "NumberOfQuizzesTaken"),
    names_to = "Variable",
    values_to = "Value"
  )

# Membuat plot gabungan
ggplot(data_long, aes(x = Value, y = CourseCompletion)) +
  geom_point(alpha = 0.7, color = "blue") +  # Titik scatter plot
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "red") +  # Regresi logistik
  facet_wrap(~ Variable, scales = "free_x", ncol = 3) +  # Panel berdasarkan variabel
  labs(
    title = "Hubungan Antara Variabel Independen dan Penyelesaian Kursus",
    x = "Nilai Variabel Independen",
    y = "Course Completion"
  ) +
  theme_minimal()  # Tema minimalis

model <- glm(CourseCompletion ~ TimeSpentOnCourse + NumberOfVideosWatched + NumberOfQuizzesTaken, 
             data = data, family = binomial)
summary(model)

# Statistik deskriptif dataset
summary(data)

# Distribusi variabel yang ingin divalidasi
hist(data$TimeSpentOnCourse, main = "Distribusi Waktu yang Dihabiskan", xlab = "Jam", col = "skyblue")
hist(data$NumberOfVideosWatched, main = "Distribusi Video Ditonton", xlab = "Jumlah Video", col = "lightgreen")
hist(data$NumberOfQuizzesTaken, main = "Distribusi Kuis Diambil", xlab = "Jumlah Kuis", col = "lightcoral")

# Ambil rata-rata waktu belajar dari dataset
avg_time <- mean(data$TimeSpentOnCourse)

# Validasi apakah rata-rata masuk akal dibandingkan dengan studi
if(avg_time >= 8 & avg_time <= 12) {
  print("Rata-rata waktu belajar sesuai dengan studi lain.")
} else {
  print("Rata-rata waktu belajar tidak sesuai dengan studi lain.")
}

# Statistik Deskriptif
summary(data$TimeSpentOnCourse)
summary(data$NumberOfVideosWatched)

# Standar Deviasi
sd(data$TimeSpentOnCourse)
sd(data$NumberOfVideosWatched)

# Models as Expectations
model <- glm(CourseCompletion ~ TimeSpentOnCourse + NumberOfVideosWatched + NumberOfQuizzesTaken, data = data, family = binomial)
# Ringkasan model
summary(model)

# Prediksi probabilitas
data$predicted <- predict(model, type = "response")
# Buat threshold untuk menentukan 'selesai' atau 'tidak selesai'
data$predicted_class <- ifelse(data$predicted > 0.5, 1, 0)
# Bandingkan hasil prediksi dengan data aktual
table(Predicted = data$predicted_class, Actual = data$CourseCompletion)

# Simulasi data sesuai dengan dataset yang diberikan
CourseCompletion <- c(1, 0, 1, 1, 1, 1, 0, 0, 0, 0)
# Histogram untuk variabel dependen (CourseCompletion)
par(mfrow = c(1, 2))  # Membuat 2 plot dalam satu baris
# Plot histogram CourseCompletion
hist(CourseCompletion,
     breaks = 2, 
     col = "skyblue", 
     border = "black",
     main = "Histogram of CourseCompletion",
     xlab = "CourseCompletion (0 = Not Completed, 1 = Completed)",
     ylab = "Frequency",
     xaxt = "n")  # Menghapus sumbu x default
axis(1, at = c(0, 1))  # Menambahkan sumbu x dengan nilai 0 dan 1

# Data normal untuk perbandingan
set.seed(123)  # Untuk reproduktibilitas
normal_data <- rnorm(1000, mean = 0.5, sd = 0.1)  # Mean 0.5, std dev 0.1

# Plot histogram normal distribution
hist(normal_data,
     breaks = 20, 
     col = "lightgreen", 
     border = "black",
     main = "Histogram of Normally Distributed Data",
     xlab = "Values",
     ylab = "Frequency")

# Mengembalikan layout ke default
par(mfrow = c(1, 1))




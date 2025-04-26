#??smail Tekin /21050251
#Gerekli k??t??phanelerin y??klenmesi
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(psych)
library(corrplot)

head(Country_data) #Veri setine bakal??m

# Say??sal s??tunlar?? i??eren yeni bir veri ??er??evesi olu??turuyoruz (ilk s??tun hari??)
Country_data_sayisal <- Country_data[, -1]

# Korelasyon matrisini hesapl??yoruz
cor_matrix <- cor(Country_data_sayisal)
print(cor_matrix)

# Korelasyon matrisini g??rselle??tiriyoruz
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

# Bartlett K??resellik Testi p<alpha ise H0 hipotezi reddedilir
cortest.bartlett(cor_matrix, n = nrow(Country_data_sayisal))

## Temel bile??en analizi
res.pca <- PCA(attitude, scale.unit = TRUE, graph = FALSE) 
res.pca
res.pca$eig # ??zde??erleri ,Her bir temel bile??enin a????klama y??zdesi ve de k??m??latif a????klama oran??n?? verir.



# Scree plot'u ??izin
plot(pca_sonucu$sdev^2, type = "b", 
     xlab = "Temel Bilesen Say??s??", 
     ylab = "Ozdegerler",
     main = "Scree Plot")
abline(h = 1, col = "red", lty = 2) 

#De??i??kenlerin temel bile??enlere katk??s?? 
res.pca$var$contrib




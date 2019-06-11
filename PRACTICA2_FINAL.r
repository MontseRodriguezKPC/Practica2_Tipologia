install.packages("ggplot2")
install.packages("VIM")
install.packages("gridExtra")
install.packages("tseries")
install.packages("car")


library(ggplot2)
library("gridExtra")
library(tseries)
library(car)
library(stats)

rm ()

#########################################################################################################################
# CARGA DE DATOS
#########################################################################################################################
setwd("C:/Users/monts/OneDrive/Escritorio/DATA SCIENCE/2do SEM (ene 19)/Tipologia y ciclo de vida de los datos/PRACTICA2") 
#setwd("~/Proyecto_Tipologia)
#wine <- read.csv(file ="C:/Users/RODRIGMO/Desktop/mine/winequality-white.csv", sep = ";")

wine <- read.csv("winequality-white.csv", sep=";")


#########################################################################################################################
## ANÁLISIS EXPLORATORIO
#########################################################################################################################
str(wine) 
head (wine)

#########################################################################################################################
## INTEGRACIÓN Y SELECCIÓN DE LOS DATOS 
#########################################################################################################################
# RECODIFICACION DE LA VARIABLE RESPUESTA
wine$quality <- factor(wine$quality, ordered = T)

# TRATAMIENTO DE VARIABLES
wine[,c(4,11)] <- lapply(wine[,c(4,11)], as.numeric)

# CREACIÓN DE NUEVA VARIABLE
wine$rating <- ifelse(wine$quality < 7, 'bad', 'good')
wine$rating <- ordered(wine$rating, levels = c('bad', 'good'))
head (wine,50)
summary(wine)

#########################################################################################################################
## LIMPIEZA DE LOS DATOS 
#########################################################################################################################


# ANALISIS UNIVARIANTE
#######################

ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))

ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))


windows()
grid.arrange(ggplot(wine, aes( x = 1, y = fixed.acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(4,14)),
             ggplot(data = wine, aes(x = fixed.acidity)) +
               geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) + 
               scale_x_continuous(lim = c(4,14)),ncol = 2)
summary(wine$fixed.acidity)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = volatile.acidity ) ) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,1)),
             ggplot(data = wine, aes(x = volatile.acidity)) +
               geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,1)), ncol = 2)
summary(wine$volatile.acidity)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = citric.acid )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = citric.acid)) +
               geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1)), ncol = 2)
summary(wine$citric.acid)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = residual.sugar )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(1,8)),
             ggplot(data = wine, aes(x = residual.sugar)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(1,8)), ncol = 2)
summary(wine$residual.sugar)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = chlorides )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,0.25)),
             ggplot(data = wine, aes(x = chlorides)) +
               geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,0.25)), ncol = 2)
summary(wine$chlorides)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = free.sulfur.dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,45)),
             ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
               geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
               scale_x_continuous(breaks = seq(0,80,5), lim = c(0,45)), ncol = 2)
summary(wine$free.sulfur.dioxide)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = total.sulfur.dioxide )) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0,180)),
             ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
               geom_histogram(binwidth = 5, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0,180)), ncol = 2)
summary(wine$total.sulfur.dioxide)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = density)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = density)) +
               geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange')), ncol = 2)
summary(wine$density)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = pH)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ),
             ggplot(data = wine, aes(x = pH)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')), ncol = 2)
summary(wine$pH)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = sulphates)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(0.3,1.6)),
             ggplot(data = wine, aes(x = sulphates)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(0.3,1.6)), ncol = 2)
summary(wine$sulphates)

windows()
grid.arrange(ggplot(wine, aes( x = 1, y = alcohol)) + 
               geom_jitter(alpha = 0.1 ) +
               geom_boxplot(alpha = 0.2, color = 'red' ) +
               scale_y_continuous(lim = c(8,14)),
             ggplot(data = wine, aes(x = alcohol)) +
               geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange')) +
               scale_x_continuous(lim = c(8,14)), ncol = 2)
summary(wine$alcohol)


# ANALISIS BIVARIANTE 
#######################
windows()
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,5)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,0.2)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,40)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0,150)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=density)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=sulphates)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  scale_y_continuous(lim = c(0.25,1)) +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)
windows()
ggplot(data=wine, aes(x=quality, y=alcohol)) +
  geom_jitter( alpha = .3) +
  geom_boxplot(alpha = .5,color = 'blue') +
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "red", 
               shape = 8, 
               size = 4)


## TRATAMIENTO DE VARIABLES
#wine$residual.sugar <- log(wine$residual.sugar)
# CREACIÓN DE NUEVA VARIABLE CATEGORICA
# discretizar alchol y density 

# wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
#  wine$quality < 7, 'average', 'good'))
# wine$rating <- ordered(wine$rating,
#                       levels = c('bad', 'average', 'good'))

#head (wine,50)
#summary(wine)


# BUSCAMOS INCONSISTENCIAS Y LAS TRATAMOS
#######################
# MISSING
colSums(is.na(wine))

suppressWarnings(suppressMessages(library(VIM)))
data$volatile.acidity <- kNN(data)$volatile.acidity
data$free.sulfur.dioxide <- kNN(data)$free.sulfur.dioxide
data$density <- kNN(data)$density
data$sulphates <- kNN(data)$sulphates

# OUTLIERS
outliers1 <- boxplot(wine$fixed.acidity,plot=FALSE)$out
outliers2 <- boxplot(wine$volatile.acidity,plot=FALSE)$out
outliers3 <- boxplot(wine$citric.acid,plot=FALSE)$out
outliers4 <- boxplot(wine$residual.sugar,plot=FALSE)$out
outliers5 <- boxplot(wine$chlorides,plot=FALSE)$out
outliers6 <- boxplot(wine$free.sulfur.dioxide,plot=FALSE)$out
outliers7 <- boxplot(wine$total.sulfur.dioxide,plot=FALSE)$out
outliers8 <- boxplot(wine$density,plot=FALSE)$out
outliers9 <- boxplot(wine$pH,plot=FALSE)$out
outliers10 <- boxplot(wine$sulphates,plot=FALSE)$out
outliers11 <- boxplot(wine$alcohol,plot=FALSE)$out

wine <- wine[-which(wine$fixed.acidity %in% outliers1),]
wine <- wine[-which(wine$volatile.acidity %in% outliers2),]
wine <- wine[-which(wine$citric.acid %in% outliers3),]
wine <- wine[-which(wine$residual.sugar %in% outliers4),]
wine <- wine[-which(wine$chlorides %in% outliers5),]
wine <- wine[-which(wine$free.sulfur.dioxide %in% outliers6),]
wine <- wine[-which(wine$total.sulfur.dioxide %in% outliers7),]
wine <- wine[-which(wine$density %in% outliers8),]
wine <- wine[-which(wine$pH %in% outliers9),]
wine <- wine[-which(wine$sulphates %in% outliers10),]
wine <- wine[-which(wine$alcohol %in% outliers11),]



# TEST DE NORMALIDAD PARA TARGET ORIGINAL
#######################
# MÉTODO GRÁFICO: HISTOGRAMA
hist(as.numeric(wine$quality))

# MÉTODO GRÁFICO: Q-Q-PLOT
ggtitle("Normal Q-Q Plot") 
qqnorm(as.numeric(wine$quality), pch = 19, col = "gray50")
qqline(as.numeric(wine$quality))

# CONTRASTE DE HIPÓTESIS: Test de Kolmogorov-Smirnov
ks.test(x = as.numeric(wine$quality),"pnorm", mean(as.numeric(wine$quality)), sd(as.numeric(wine$quality)))

# CONTRASTE DE HIPÓTESIS: Test de Jarque-Bera
jarque.bera.test(x = as.numeric(wine$quality))

# TEST DE CORRELACIONES
#######################
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")

for (i in 1:(ncol(wine) - 2)) {
  spearman_test <- cor.test(wine[,i], as.numeric(wine$quality),method="spearman")
  corr_coef = spearman_test$estimate
  p_val = spearman_test$p.value
  # Add row to matrix
  pair = matrix(ncol = 2, nrow = 1)
  pair[1][1] = corr_coef
  pair[2][1] = p_val
  corr_matrix <- rbind(corr_matrix, pair)
  rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(wine)[i]
}
print (corr_matrix)



# ESTIMACIÓN DEL MODELO
#######################
#m1 <- glm(as.factor(rating)~., data=wine, family="binomial")

# ajuste del modelo y significaciones (ANOVA)
summary(m1 <- lm(as.numeric(quality)~(.-rating),wine))
Anova(m1)
summary(m2 <- lm(as.numeric(quality)~(.-density-rating),wine))
Anova(m2)
summary(m3 <- lm(as.numeric(quality)~(.-density-pH-rating),wine))
Anova(m3)
summary(m4 <- lm(as.numeric(quality)~(.-density-pH-sulphates-rating),wine))
Anova(m4)

mfinal <- m4

# DETECCIÓN DE MULTICOLINEALIDAD
################################
vif(mfinal)


summary(mf <- lm(as.numeric(quality)~(.-density-pH-sulphates-free.sulfur.dioxide-rating),wine))
Anova(mf)
plot(mf)

summary(mt <- lm(as.numeric(quality)~(.-density-pH-sulphates-total.sulfur.dioxide-rating),wine))
Anova(mt)
plot(mt)

# selección del modelo final
modelo <- mt
modelo


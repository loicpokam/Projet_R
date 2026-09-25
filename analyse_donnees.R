#-----------------------------------------------------------------------------------
# Importation des donnees 
data <- read.csv("data.csv", sep = ",")
data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"), header = FALSE)

# Modification du nom des colonnes
colnames(data) <- c("age","Sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","target")

#Pretraitement de la colonne target

data$target[data$target==2] <- 1
data$target[data$target==3] <- 1
data$target[data$target==4] <- 1

#Traitement des valeurs manquantes 
#suppression des valeurs manquantes 
#data<- data[-1,]
#data<- data[,-2]
#data<- data[-88,]
valeurs_manquantes_ca <- which(data$ca%in%"?")
valeurs_manquantes_thal <- which(data$thal%in%"?")

valeurs_manquantes <- c(valeurs_manquantes_ca, valeurs_manquantes_thal)
valeurs_manquantes
data <- data[-valeurs_manquantes,]

#Verification du type de variable
str(data)
#Modification du type des variables 

## Variables qualitatives (fACTOR = variable qualitative)
data$Sex <-as.factor(data$Sex)
data$cp <-as.factor(data$cp)
data$fbs <-as.factor(data$fbs)
data$restecg <-as.factor(data$restecg)
data$exang <-as.factor(data$exang)
data$slope <-as.factor(data$slope)
data$thal <-as.factor(data$thal)
data$target <-as.factor(data$target)

## Variables quantitative (integer = variable qualitative)
data$age <- as.integer(data$age)
data$trestbps <- as.integer(data$trestbps)
data$chol <- as.integer(data$chol)
data$thalach <- as.integer(data$thalach)

##Recodage des variables 

levels(data$Sex)
levels(data$Sex) <- c("Femme","Homme")

levels(data$cp)
levels(data$cp) <- c("Angine Stage","Angine instable","Autres douleurs","Asymptomatique")
levels(data$fbs) <- c("Non","Oui")
levels(data$restecg) <- c("Normal","Anomalies","Hypertrophie")
levels(data$exang) <- c("Non","Oui")
levels(data$slope) <- c("En hausse","Stable","En baisse")
levels(data$ca) <- c("Absence d'anolamie","Faible","Moyen","Elevé")
levels(data$thal) <- c("Non","Thalassémie sous controle","Thalassémie instable")
levels(data$target) <- c("Non","Oui")

#Verification du type de variable
str(data)

#Verification des valeurs manquantes 
apply(data,2,anyNA)
#Verification ligne 
apply(data,1,anyNA)
#-----------------------------------------------------------------------------------




# Indicateurs cles(Variabkes qualitatives)

table(data$Sex) # Effectifs

prop.table(table(data$Sex)) # Frequences 
round(prop.table(table(data$Sex)), 4) # Frequences arrondies 
round(prop.table(table(data$Sex)), 4)*100 # Pourcentages 

# Variables cp
table(data$cp) # Effectifs
round(prop.table(table(data$cp)), 4)*100 # Pourcentages

# Variables fbs
table(data$fbs) # Effectifs
round(prop.table(table(data$fbs)), 4)*100 # Pourcentages

# Variables restcg
table(data$restecg) # Effectifs
round(prop.table(table(data$restecg)), 4)*100 # Pourcentages

# Variables exang
table(data$exang) # Effectifs
round(prop.table(table(data$exang)), 4)*100 # Pourcentages

# Variables slope
table(data$slope) # Effectifs
round(prop.table(table(data$slope)), 4)*100 # Pourcentages

# Variables ca
table(data$ca) # Effectifs
round(prop.table(table(data$ca)), 4)*100 # Pourcentages


# Variables thal
table(data$thal) # Effectifs
round(prop.table(table(data$thal)), 4)*100 # Pourcentages

# Variables target
table(data$target) # Effectifs
round(prop.table(table(data$target)), 4)*100 # Pourcentages


# Indicateurs cles(Variables quantitatives)
 #Variables age
# Minimum , quartiles, mediane, moyenne et maximun
summary(data$age)
summary(data$trestbps)
summary(data$chol)
summary(data$thalach)
summary(data$oldpeak)


# Variances et ecart_type

var(data$age)
sd(data$age)

var(data$trestbps)
sd(data$trestbps)

var(data$chol)
sd(data$chol)

var(data$thalach)
sd(data$thalach)


var(data$oldpeak)
sd(data$oldpeak)

#-------------------------------------------------------------------------------

# Visualisation des donnees 

# Diagrammes a barre (Variable qualitatives)

## Variables sex
graph1 <- plot(data$Sex, xlab = "Sex", ylab = "Effectifs", main = "Repartition des patients selon le sex",
     las = 1, 
     #horiz = "T"
     sub = "Donnees: Heart Diseases Data Set (UCI Machine Learning)",
     #names.arg = c("Féminin","Masculin"),
     #space = 2,
     #col = "red",
     col = c("blue","red"),
     #border = "blue",
     #densisty = 30
     cex.main = 1.8,
     cex.axis = 1.1,
     cwx.lab = 1.2,
     ylim = c(0,250)
     )

text( x = graph1, y = table(data$Sex) + 10, labels = as.character(table(data$Sex)), cex = 1.1, font = 3)


## variable cp
graph2 <- plot(data$cp, xlab = "Douleurs thoraciques",
                ylab = "Effectifs",
                main = "Repartition des patients selon les douleurs thoraciques",
                space = 0.3,
                col = c("#e63946","#f1faee","#a8dadc","#457b9d"),
                cex.main = 1.5,
                cwx.lab = 1.2,
                ylim = c(0,170)
                )

text( x = graph2, y = table(data$cp) + 7, labels = as.character(table(data$cp)), cex = 1.1, font = 3)



## Diagrammes circulaires (Variables qualitatives )
# Variable target

pie(table(data$target), main = "Repartition des patients selon l'apparition d'une maladie cardiovasculaire",
    clockwise = TRUE,
    col = c("blue","red"),
    cex.main = 1.2,
    )




#Boite a moustache (Variable quantiatives)

## Variage age 
boxplot(data$age, 
        ylab = "Age", main = "Boite a moustache de la population selon l'age",
        color ="#e63946",
        las = 1,
        cex.main = 1.7,
        cex.lab = 1.2,
        sub = "Donnees: Heart Disease Data Set(UCI Machine Learning)",
        notch = TRUE,
        ylim = c(20,80)
        )



#Histogramme (Variable quantitatives)

graph3 <- hist(data$trestbps,
     xlab = "Tension arterielle au repos",
     ylab = "Effectifs",
     main = "Repartition des patients selon la tension arterielle au repos",
     las = 1,
     sub = "Donnees: Heart Disease Data Set (UCI Machine Learning)",
     col ="lightslateblue",
     ylim = c(0,80),
     xlim = c(80,200),
     cex.main = 1.6,
     cex.lab = 1.2)


text(x= graph3$mids, graph3$counts, labels = graph3$counts, adj = c(0.5, -0.5))


### Graphiques croises
  ##Diagramme a barre croisse
graph4 <- barplot(table(data$target, data$Sex),
        beside = TRUE,
        col = c("#003049","#d62828"),
        xlab = "Sexe",
        ylab = "Patients",
        las = 1,
        main = "Repartition des patients selon la presence d'une maladie cardiovasculaire \n et le sexe",
        ylim = c(0,150),
        cex.main = 1.2,
        cex.lab = 1.2
        )
legend("top", legend = levels(data$target), fill = c("#003049","#d62828"), title = "Maladie cardiovasculaire",horiz = TRUE)
text(x = graph4, y = table(data$target, data$Sex) +10, labels = as.character(table(data$target,data$Sex)), cex = 1.1,font = 3 )


graph5 <- barplot(table(data$target, data$cp),
                  beside = TRUE,
                  col = c("blue","#d62828"),
                  xlab = "Sexe",
                  ylab = "Patients",
                  las = 1,
                  main = "Repartition des patients selon la presence d'une maladie cardiovasculaire \n et les douleurs thoraciques ",
                  ylim = c(0,150),
                  cex.main = 1.2,
                  cex.lab = 1.2
)
legend("top", legend = levels(data$target), fill = c("blue","#d62828"), title = "Maladie cardiovasculaire",horiz = TRUE)
text(x = graph5, y = table(data$target, data$cp) +7, labels = as.character(table(data$target,data$cp)), cex = 1.1,font = 3 )

### Boite a moustache croissee 

boxplot(data$age~data$target,
        main= "Boite a moustache de la population selon l'age et la presence \n de la maladie cardiovasculaire",
        xlab = "Presence d'une maladie cardiovasculaiew",
        ylab = "Age",
        col="Yellow",
        las = 1,
        ylim = c(20,80),
        cex.main = 1.2,
        cex.lan = 1.2)





boxplot(data$trestbps~data$target,
        main= "Boite a moustache de la population selon la tension arterielle au repos et la presence \n de la maladie cardiovasculaire",
        xlab = "Presence d'une maladie cardiovasculaiew",
        ylab = "Tension arterielle au repos",
        col="Yellow",
        las = 1,
        ylim = c(80,200),
        cex.main = 1.2,
        cex.lan = 1.2)


### Realiser des tests d'hypotheses/ tests statistiques

#Calcul des pourcentages 
table(data$Sex, data$target)
round(prop.table(table(data$Sex, data$target), margin = 1),4 )* 100

round(prop.table(table(data$cp, data$target), margin = 1),4 )* 100
round(prop.table(table(data$fbs, data$target), margin = 1),4 )* 100
round(prop.table(table(data$restecg, data$target), margin = 1),4 )* 100
round(prop.table(table(data$exang, data$target), margin = 1),4 )* 100
round(prop.table(table(data$slope, data$target), margin = 1),4 )* 100
round(prop.table(table(data$ca, data$target), margin = 1),4 )* 100
round(prop.table(table(data$thal, data$target), margin = 1),4 )* 100


## Test du khi-Deux(Uniquement sur les variables qualitatives)

## H0: Les deux variables sont independantes (si la p-value > 0,05)
## H1: les deux variables sont dependantes (si la p-value < 0,05)

chisq.test(data$Sex, data$target)
chisq.test(data$cp, data$target)
chisq.test(data$fbs, data$target)
chisq.test(data$restecg, data$target)
chisq.test(data$exang, data$target)
chisq.test(data$slope, data$target)
chisq.test(data$ca, data$target)
chisq.test(data$thal, data$target)

## Moyennes conditionnelles 
tapply(data$age, data$target, mean)
tapply(data$trestbps, data$target, mean)
tapply(data$chol, data$target, mean)
tapply(data$thalach, data$target, mean)
tapply(data$oldpeak, data$target, mean)


# Test de shapiro- wilk
### H0: L'echantillon suit une distribution normale (si p-value >0,05)
### H1: L'echantillon ne suis pas une distribution normale (si p-value < 0,05)

install.packages("dplyr")
library(dplyr)

shapiro.test(filter(data, target == "Oui")$age)# H1
shapiro.test(filter(data, target == "Oui")$trestbps) # H1
shapiro.test(filter(data, target == "Oui")$chol) # H0
shapiro.test(filter(data, target == "Oui")$thalach) # H0
shapiro.test(filter(data, target == "Oui")$oldpeak) # H1


## Test de Mann-Whitney (Les variables qui ne suivent pas une distribution normale )

### H0 : il n-y a pas de difference significative entre la moyenne des deux variables (si p-value >0,05)
### H1 : il y' a une difference signifiactive entre la noyenne des deux variables (si p-value < 0,05)

wilcox.test(data$age~data$target) # H1
wilcox.test(data$trestbps~data$target) # H1
wilcox.test(data$oldpeak~data$target) # H1 


## Test de student (Les variables qui suivent une distribution normale )
### H0 : il n-y a pas de difference significative entre la moyenne des deux variables (si p-value >0,05)
### H1 : il y' a une difference signifiactive entre la noyenne des deux variables (si p-value < 0,05)

t.test(data$chol~data$target) ## H0
t.test(data$thalach~data$target) ## H1 





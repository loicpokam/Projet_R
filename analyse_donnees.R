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


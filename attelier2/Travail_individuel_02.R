
#  title: "MBA-6005 TRAVAIL INDIVIDUEL 02"
#  author: Zouheyr Ayas
#  date: "Nov 15, 2020"

  #_______________________#
  # PRATIQUE - PARTIE 01  #
  #_______________________#
  
  ## Chargement de données
  
#install.packages("car")
library(car) 
data(Salaries)
head(Salaries,10)

## Exploration de la structure de données

str(Salaries)


## QUESTION 01: Nombre d'hommes et de femmes par grade


table(Salaries$rank,Salaries$sex)

### Proportions d'hommes et de femmes par grade:

#### Proportion globale:

prop.table(table(Salaries$rank,Salaries$sex))

#### Proportion par ligne:

prop.table(table(Salaries$rank,Salaries$sex),1)

#### Proportion par colonne:

prop.table(table(Salaries$rank,Salaries$sex),2)


## QUESTION 02 : Histogramme pour les années de service

hist(Salaries$yrs.service, freq = FALSE, 
     main="Années de service - Densité",col="lightgreen",
     xlab = "Années de services",probability = TRUE,
     labels = TRUE)

#Dessiner la ligne de densitE

lines(density(Salaries$yrs.service, bw=10),type="l",col="darkred",lwd=5)

## QUESTION 03 : Dessiner un box plot pour le salaire

boxplot_salaires <- boxplot(Salaries$salary, col="lightblue",main="Salaire",ylab="$$$")

## QUESTION 04: Dessiner un box plot pour le salaire par grade

#Spécification de colors

colors <- ifelse(levels(Salaries$rank)=="AsstProf" , rgb(0.1,0.1,0.7,0.5) , 
                 ifelse(levels(Salaries$rank)=="AssocProf", rgb(0.8,0.1,0.3,0.6),
                        "grey90"))

#Lancement de plot

boxplot(Salaries$salary  ~  (Salaries$rank), col=colors, 
        main="Les salaires par grade",xlab = "$$$", ylab = "Grade", 
        horizontal = TRUE)

### Plot de salaires par Sex:


#spécification des colors 

colors <- ifelse(levels(Salaries$sex)=="Male" , rgb(0.1,0.1,0.7,0.5) , 
                 ifelse(levels(Salaries$sex)=="Female", rgb(0.8,0.1,0.3,0.6),
                        "grey90" ) )
#Plotting

boxplot(Salaries$salary  ~  (Salaries$sex), col=colors, 
        main="Les salaires par sex",xlab = "Sex", ylab = "$$$")

# EXPREMENTATIONS:  

## 3.2.RELATION ENTRE LES VARIABLES

### 3.2.1.Chargement de données de CRM

cust.df<-read.csv("http://goo.gl/PmPkaG")
str(cust.df)


### 3.2.2.Converting data to factors

str(cust.df$cust.id)
cust.df$cust.id<-factor(cust.df$cust.id)
str(cust.df$cust.id)


plot(x=cust.df$age,y=cust.df$credit.score,xlab = "AGE", ylab = "CREDIT")


### 3.2.3. Add color, labels, and adjust axis limits

plot(cust.df$age, cust.df$credit.score,col="orange",xlim=c(15,55),
     ylim = c(500,900),main="Clients actifs en juin 2014",
     xlab = "Age du client(Années)",ylab = "Cote de crédit")

### 3.2.4. Regression

plot(cust.df$age, cust.df$credit.score,col="orange",xlim=c(15,55),
     ylim = c(500,900),main="Clients actifs en juin 2014",
     xlab = "Age du client(Années)",ylab = "Cote de crédit")
abline(lm(cust.df$credit.score ~ cust.df$age),col="red")


### 3.2.5. Ventes en ligne vs ventes en magasin

plot(cust.df$store.spend,cust.df$online.spend,
     xlab = "Ventes en magasin sur une année($)",
     ylab = "Ventes en ligne sur une année($)")


### 3.2.6.Histogramme des depenses en magasin:

histo_depenses_magasin <- hist(cust.df$store.spend,
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     xlab = "Vente en ligne sur une année")


### 3.2.7.Utilisation de la fonction logarithmique

plot(cust.df$store.spend+1,cust.df$online.spend+1,log = "xy")


### 3.2.8.Multi-panel plot

par(mfrow=c(2, 2))
with(cust.df, plot(distance.to.store, store.spend))
with(cust.df, plot(distance.to.store, online.spend))
with(cust.df, plot(distance.to.store, store.spend+1, log="xy"))
with(cust.df, plot(distance.to.store, online.spend+1, log="xy"))


### 3.2.9.ScatterPlot Matrix

pairs(formula = ~ age + credit.score + distance.to.store +
        online.spend + store.trans + store.spend, data=cust.df,lower.panel=panel.smooth,col="lightblue")


### 3.2.10.ScatterPlot Matrix with car data

library(car) 
scatterplotMatrix(formula = ~ age + credit.score +
                    distance.to.store + online.spend + store.trans +
                    store.spend, data=cust.df, diagonal="histogram")

### 3.2.11. Coefficient de correlation de Pearson

cor(cust.df$age,cust.df$credit.score)

### 3.2.12. Matrice de correlation

cor(cust.df[, c(2, 3, 5:12)]) 


### 3.2.13. Visualiser la matrice de corrélation

library(corrplot)
corrplot(corr=cor(cust.df[ , c(2, 3, 5:12)],
                  use="complete.obs"), method ="ellipse")


### 3.2.14. Transformation de données

cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/cust.df$distance.to.store, cust.df$store.spend)
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)


### 3.2.15.Exploration des associations dans des réponses d'enquête

plot (cust.df$sat.service, cust.df$sat.selection, xlab="Sat,
Service", ylab="Sat, Selection")


### 3.2.16.Exploration des associations dans des réponses d'enquête - suite

plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection),
     xlab="Customer Satisfaction with Service",
     ylab="Customer Satisfaction with Selection",
     main="Customers as of June 2014")


### 3.2.17.Exploration des associations dans des réponses d'enquête - suite

resp <- !is.na(cust.df$sat.service)
library(psych) 
polychoric(cbind(cust.df$sat.service[resp],
                 cust.df$sat.selection[resp]))

#_______________________#
# PRATIQUE - PARTIE 02  #
#_______________________#

## Chargement de données

library(car) # install.packages("car") if needed 
data(Salaries)
head(Salaries,10)


## QUESTION 01 : Les salaires par rapport aux années écoulées depuis le doctorat

logSalary <- log(Salaries$salary)
plot(Salaries$yrs.since.phd,Salaries$salary,
     main = "Salaires par rapport au nombre d'années apres le doctorat",
     xlab = "Temps aprés phd(année)",
     ylab = "salaires($)",
     col="red")


#Dessinons une droite sur le nuage de points
plot(Salaries$yrs.since.phd,Salaries$salary,
     main = "Salaires par rapport au nombre d'années aprés le doctorat",
     xlab = "Temps aprés phd(année)",
     ylab = "salaires($)",
     col="red")
model=lm(Salaries$salary ~ Salaries$yrs.since.phd)
abline(model,col="darkred")

## QUESTION 02 : Corrélation entre salaire et nombre d'années après le doctorat

#Il parait qu'il y a une linéarité entre les deux variables, Salaires et nombre d'années après le doctorat, calculons le coefficient de corrélation de Pearson pour bien évaluer le degré de dépendance

cor.test(Salaries$salary , Salaries$yrs.since.phd)
#Coefficient de Pearson = 0.42 , la dépendance est considérée **modérée**

### Corrélation entre la variable salaire et nombre d'années de service
plot(Salaries$yrs.service,Salaries$salary,
     main = "Salaires par rapport au nombre d'années de service",
     xlab = "Temps de service(année)",
     ylab = "salaires($)",
     col="blue")
model=lm(Salaries$salary ~ Salaries$yrs.service)
abline(model,col="darkblue")

### Coefficient de corrélation
cor.test(Salaries$salary , Salaries$yrs.service)
    #Coefficient de Pearson = 0.33 , la dépendance est considérée **faible**
  

### P-Value
    #On observe que dans les deux test le P-Value est de loins inférieur à 0.05,
    #alors les résultats sont significatives statistiquement


## QUESTION 03: Visualisation de toutes les relations bivariées

library(corrplot)
corrplot(corr=cor(Salaries[ , c(3, 4, 6)],
                  use="complete.obs"), method ="circle",
         sig.level =0.05,insig = "blank",)

# On observe une corrélation relativement faible entre la variable salaire et
# les deux autres variables "nombre d'années de service" et "nombre d'années 
# écoulées après le doctorat.











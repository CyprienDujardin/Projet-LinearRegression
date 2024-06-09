#0-Préalable
#Question 0
  #Léo JOURDAIN (25%), Paul-Antoine ARMANI(25%), Cyprien DUJARDIN(25%), Marine TOURET(25%)

#II-Données simulées
#On tire au hasard les valeurs de a0, b0 et s0

a0<-round(runif(1,3,15),2)  
b0<-round(runif(1,1,5),2)  
s0<-round(runif(1,1,3),2)

# Fonctions pour calculer sx^2, SxY en appliquant les formules en I.2
calc_sx2 <- function(X) {
  return(sum((X - mean(X))^2) / length(X))
}

calc_SxY <- function(X, Y) {
  return(sum((Y - mean(Y)) * (X - mean(X))) / length(X))
}

# Question 1
  #Question 1.a
n=500
X<- round(runif(n,0,5), 2)  #On crée n valeurs aléatoires suivant une loi uniforme dans [0,5] arrondies à 10^-2

  #Question 1.b
Y <- sapply(X, function(x) {  #On applique une fonction anonyme sur Y avec toutes les valeurs de X
  a <- a0 + b0 * x   #On calcule la valeur de la moyenne de la loi normale associée
  round(rnorm(1, mean = a, sd = s0),2) #On insère les valeurs dans Y arrondies à 10^-2
})    

# Question 2
plot(X,Y)     #On trace Y en fonction de X

# Question 3
#On ecrit les fonctions qui permettent de calculer b_estim, a_estim et sigma_estim grâce aux fontions calc_sx2 et calc_SxY précédemment créées et en appliquant les définitions de la partie I

b_estim <- function(X, Y) {
  sx2 = calc_sx2(X) #On utilise les fontions précedement définies pour calculer les estimateurs
  SxY = calc_SxY(X,Y)
  
  if (sx2 == 0) {
    return("Erreur, division par 0 !")
  }
  
  return(SxY / sx2)
}

a_estim <- function (X,Y){
  x = mean(X)
  y = mean(Y)
  b_ = b_estim(X, Y)
  
  return (y - b_ * x)
}

sigma_estim <- function (X,Y){
  a = a_estim(X, Y)
  b = b_estim(X,Y)
  
  return (sum((Y - a - b * X)^2) / (length(X)-2))
}

# Question 4
b_ = b_estim(X, Y)
a_ = a_estim (X, Y)
s_ = sigma_estim(X,Y)

print(c(a_,b_,s_))
# Question 5
# on ajoute une droite de régression linéaire dans le graphique
abline(a0, b0, col="red")   # on ajoute la droite d'équation y = a0 + b0*x
abline(a_,b_, col="blue")   #on ajoute la droite d'équation y = a_ + b_*x

# Question 6
#On applique la définition des Ei de l'énoncé pour définir la liste du bruit

Y_ = a_ + b_*X
Epsilon = Y-Y_
print(sum(Epsilon))
  #On trouve un résultat de l'ordre de 10^-14

# Question 7
yTemp = a_ + b_ * mean(X) # On calcule l'image de x barre (mean(x))
YTemp = mean(Y) # On affiche la moyenne de Y pour comparer
dif = abs(yTemp - YTemp)

if (dif == 0) {
  print("le couple (x barre, y barre) appartient à la doite des moindres carrés")
}

#Question 8
donnée<- data.frame(X,Y)
reg<-lm(Y ~ X, data = donnée)
summary(reg)

#On voit que les résidus ont pour médiane un nombre "proche" de 0 et des quartiles de + ou - 1,5
#On constate aussi que les valeurs des estimateurs de a et b sont exactement les valeurs trouvées aux questions précédentes

# Question 9
a_estimateur <- numeric(n)
b_estimateur <- numeric(n)

for (i in 2:n){
  X_temp<- round(runif(i,0,5), 2)
  Y_temp <- sapply(X_temp, function(x) {  #On applique une fonction anonyme sur Y avec toutes les valeurs de X
    a <- a0 + b0 * x   #On calcule la valeur de la moyenne de la loi normale associée
    round(rnorm(1, mean = a, sd = s0  ),2) #On insère les valeurs dans Y arrondi à 10^-2
  })    
  
  b_estimateur[i]= b_estim(X = X_temp,Y=Y_temp)
  a_estimateur[i]= mean(Y_temp) - b_estimateur[i]*mean(X_temp) 
}

plot(a_estimateur)
abline(h = a0, col = "red")

plot(b_estimateur)
abline(h=b0, col = "red")
  #On constate que le nuage de points converge vers les valeurs voulues

#Question 10
# Fonction pour calculer l'estimateur sigma2
Fctpivot <- function(){
  ListePivot = c()
  
  for (i in 1:150){
    X_pivot = round(runif(n,0,5), 2)  
    Y_pivot <- sapply(X_pivot, function(x) {  #On applique une fonction anonyme sur Y avec toutes les valeurs de X
      a <- a0 + b0 * x   #On calcule la valeur de la moyenne de la loi normale associée
      round(rnorm(1, mean = a, sd = s0  ),2) #On insère les valeurs dans Y arrondies à 10^-2
    })    
    
    b_ = b_estim(X_pivot, Y_pivot)
    a_ = mean(Y_pivot) - b_*mean(X_pivot)
    sx2 = calc_sx2(X_pivot)
    sig2 = sigma_estim(X_pivot, Y_pivot)
    Pivot = (a_ - a0)/ sqrt((sig2/n)*(1+(mean(X_pivot)^2)/sx2))
    ListePivot = append(ListePivot, Pivot)
  }
  
  return (ListePivot)
}

Rep = replicate(20, Fctpivot())
hist(Rep, freq = FALSE)
curve(dt(x, df = n - 1), add = TRUE)

#Question 11
# On utilise la formule en I.3 pour généré les trois d'intervalles de confiance
gen_IC_a <- function(X,Y,alpha) {
  sig2 = sigma_estim(X, Y) # On calcule sigma2 à l'aide de la fonction définie précédemment
  mean2 = mean(X)**2 
  sx2 = calc_sx2(X) # On calcule sx2 à l'aide de la fonction définie précédemment
  t_alpha = qt(1 - alpha / 2, df = n - 2) # On calcule le quantile de la loi de Student à n-2 de degrés de liberté
  borneInf = a_estim(X,Y)-t_alpha*sqrt((sig2/n)*(1+(mean2/sx2))) # On applique la formule pour la borne inférieure
  borneSup = a_estim(X,Y)+t_alpha*sqrt((sig2/n)*(1+(mean2/sx2))) # Et pour la borne supérieure
  
  return(c(borneInf, borneSup))
}

gen_IC_b <- function(X,Y,alpha) {
  sig2 = sigma_estim(X, Y) # On calcule sigma2 à l'aide de la fonction définie précédemment
  sx2 = calc_sx2(X) # On calcule sx2 à l'aide de la fonction définie précédemment   
  t_alpha = qt(1 - alpha / 2, df = n - 2) # On calcule le quantile de la loi de Student à n-2 de degrés de liberté
  borneInf = b_estim(X,Y)-t_alpha*sqrt(sig2/(n*sx2)) # On applique la formule pour la borne inférieure
  borneSup = b_estim(X,Y)+t_alpha*sqrt(sig2/(n*sx2)) # Et pour la borne supérieure
  
  return(c(borneInf, borneSup))
}

gen_IC_sigma <- function(X,Y,alpha) {
  sig2 = sigma_estim(X, Y)  # On calcule sigma2 à l'aide de la fonction définie précédemment
  fnInf = qchisq(1 - alpha / 2, df = n - 2) # On calcule le quantile de la loi du Khi2 à n-2 de degrés de liberté
  fnSup = qchisq(alpha / 2, df = n - 2) # On calcule le quantile de la loi du Khi2 à n-2 de degrés de liberté
  borneInf = ((n-2)*sig2)/fnInf # On applique la formule pour la borne inférieure
  borneSup = ((n-2)*sig2)/fnSup # Et pour la borne supérieure
  
  return(c(borneInf, borneSup))
}

gen_IC_a(X,Y,0.5)
gen_IC_b(X,Y,0.5)
gen_IC_sigma(X,Y,0.5)

# Question 12
alpha=0.05 # On prend un alpha petit
  # On fait 100 intervalles de confiance en s'assurant de regénérer le couple de variables aléatoires
a100 = replicate(100, gen_IC_a(X_IC<- round(runif(n,0,5), 2) ,Y_IC <-sapply(X_IC, function(x) {  
  a <- a0 + b0 * x
  round(rnorm(1, mean = a, sd = s0),2) 
})  ,alpha))

# On fait 100 intervalles de confiance en s'assurant de regénérer le couple de variables aléatoires
b100 = replicate(100, gen_IC_b(X_IC<- round(runif(n,0,5), 2),Y_IC <-sapply(X_IC, function(x) {  
  a <- a0 + b0 * x
  round(rnorm(1, mean = a, sd = s0),2) 
}) ,alpha))

# On fait 100 intervalles de confiance en s'assurant de regénérer le couple de variables aléatoires
sigma100 = replicate(100, gen_IC_sigma(X_IC<- round(runif(n,0,5), 2),Y_IC <-sapply(X_IC, function(x) {  
  a <- a0 + b0 * x
  round(rnorm(1, mean = a, sd = s0),2) 
}),alpha))

# Question 13
source("/Users/leojourdain/School/GI02/AC04/ProjetAC04/utils.R") 
plot_ICs(a100, mu = a0, main = "Intervalle de confiance pour a") # Permet d'afficher les intervalles de confiance pour a
plot_ICs(b100, mu = b0,main = "Intervalle de confiance pour b") # Permet d'afficher les intervalles de confiance pour b
plot_ICs(sigma100, mu = s0^2, main = "Intervalle de confiance pour sigma") # Permet d'afficher les intervalles de confiance pour sigma
  # On peut conclure que pour un alpha petit il y a très peu de fois où a0/b0/s0^2 ne se trouvent pas dans un intervalle de confiance

#III-Homoscédasticité,indépendance et normalité des résidus
#Question 14
library(ggplot2) #package permettant de créer des visualisations de données sur R
library(Rmisc) #package permettant d'utiliser la fonction multiplot pour afficher sur une même fenêtre les 4 graphiques qui seront tracés à chaque question
  #On crée le jeu de données Anscombe qui contient 4 data frame possédant les mêmes propriétés statistiques (moyenne, variance...) mais qui sont à la fois très éloignés (surtout en terme de représentation graphique) 
anscombe<-data.frame(
  x1 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
  y1 = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
  x2 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
  y2 = c(9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74),
  x3 = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5),
  y3 = c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73),
  x4 = c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8),
  y4 = c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89)
)
  #On crée leur représentation graphique en nuages de points grâce à la fonction ggplot
j1 <- ggplot(anscombe, aes(x1, y1)) + geom_point()+labs(title="Jeu de données 1")
j2 <- ggplot(anscombe, aes(x2, y2)) + geom_point()+labs(title="Jeu de données 2") 
j3 <- ggplot(anscombe, aes(x3, y3)) + geom_point()+labs(title="Jeu de données 3") 
j4 <- ggplot(anscombe, aes(x4, y4)) + geom_point()+labs(title="Jeu de données 4") 

multiplot(j1,j2,j3,j4,cols=2)

#Question 15
#Régressions linéaires sur les 4 jeux de données
lm1<-lm(y1~x1,data=anscombe)
lm2<-lm(y2~x2,data=anscombe)
lm3<-lm(y3~x3,data=anscombe)
lm4<-lm(y4~x4,data=anscombe)

#Afficher les résultats des régressions
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

#Visualisation graphique des régressions (fonction geom_smooth = affiche une droite de tendance)
j1 <- ggplot(anscombe, aes(x1, y1)) + geom_point()+geom_smooth(method="lm",se=FALSE,color="green")+labs(title="Jeu de données 1")
j2 <- ggplot(anscombe, aes(x2, y2)) + geom_point()+geom_smooth(method="lm",se=FALSE,color="green")+labs(title="Jeu de données 2") 
j3 <- ggplot(anscombe, aes(x3, y3)) + geom_point()+geom_smooth(method="lm",se=FALSE,color="green")+labs(title="Jeu de données 3") 
j4 <- ggplot(anscombe, aes(x4, y4)) + geom_point()+geom_smooth(method="lm",se=FALSE,color="green")+labs(title="Jeu de données 4")
multiplot(j1,j2,j3,j4,cols=2)
  #Interprétation des résultats :
    #Les 4 jeux de données présentent un R^2 très proches (environ 0,67) donc 67% de la variance des données est expliquée par un modèle linéaire. Or, graphiquement on observe que un modèle linéaire n'est pas adapté à ces 4 jeux de données (surtout le 2 et le 4). 
    #De ce fait, les coefficients de régressions linéaires tendent à prouver l'adéquation des données à une régression linéaire alors que graphiquement cela ne se voit pas. Ces données ne suffisent donc pas à conclure.

#Question 16
#Calcul des résidus
residuals(lm1)
residuals(lm2)
residuals(lm3)
residuals(lm4)

#Vérification de l'hypothèse de linéarité (graphique résidus en fonction des valeurs prédites)
library(olsrr) #permet la création de modèles en lien avec la régression
resid_fit1<- ols_plot_resid_fit(lm1)
resid_fit2<- ols_plot_resid_fit(lm2)
resid_fit3<-ols_plot_resid_fit(lm3)
resid_fit4<-ols_plot_resid_fit(lm4)
multiplot(resid_fit1,resid_fit3,resid_fit2,resid_fit4,cols=2) #Lecture des graphiques en ligne : resid_fit1 en haut à gauche, resid_fit2 en haut à droite, resid_fit3 en bas à gauche...)
  #Ici, les résidus des jeux de données 2, 3 et 4 ne sont pas placés de manière aléatoire autour de 0 donc il n'y a pas de relation de linéarité et la moyenne des résidus n'est pas égale à 0.
  #Le jeu de données 1 présente des résidus qui se dispersent aléatoirement mais pas autour de 0 donc il existerait une relation de linéarité mais la moyenne des résidus n'est pas égale à 0.

#Vérification de l'hypothèse de normalité de la distribution des résidus (diagramme quantile-quantile)
qq1<- ols_plot_resid_qq(lm1)
qq2<- ols_plot_resid_qq(lm2)
qq3<- ols_plot_resid_qq(lm3)
qq4<- ols_plot_resid_qq(lm4)
multiplot(qq1,qq3,qq2,qq4,cols=2) #Ordre de lecture des graphiques pareille que dans la représentation précédente
  #Seul le jeu de données 4 possède des quantiles qui se semblent se rapprocher des quantiles d'une loi normale N(0,sigma^2)
  #Pour les jeux 2 et 3, certains quantiles sont très éloignés de la distribution normale donc il est assez difficile de conclure pour ceux-là.

#Histogramme résidus corrigés superposés à une densité de loi normale
hist1<- ols_plot_resid_hist(lm1)
hist2<- ols_plot_resid_hist(lm2)
hist3<- ols_plot_resid_hist(lm3)
hist4<- ols_plot_resid_hist(lm4)
multiplot(hist1,hist3,hist2,hist4,cols=2)
  #Conclusions difficiles à tirer avec ces graphiques mais il semblerait qu'aucun résidus des jeux de données 1-2-3-4 ne suivent pas une loi normale. Nous allons le vérifier avec un test statistique de la normalité.

#Test Shapiro-Wilk permet de tester l'hypothèse de normalité d'une population :
  #Hypothèses -> H0 : les résidus suivent une loi normale ; H1 : les résidus ne suivent pas une loi normale
library(nortest)#Package contenant des tests pour tester la normalité dont Shapiro-Wilk
shapiro.test(residuals(lm1))
shapiro.test(residuals(lm2))
shapiro.test(residuals(lm3))
shapiro.test(residuals(lm4))
  #Seul lm3 donne une p-value<0,05, donc on rejette H0 donc seul lm3 présente des résidus qui ne suivent pas une loi normale.
  #Néanmoins, au vu des représentations graphiques précédentes, il semblerait difficile de conclure que les résidus des jeux de données 1-2-3 suivent une loi normale.

#Vérification de l'homoscédasticité (test Breshch-Pagan) (homoscédasticité = la dispersion des résidus doit être homogène et constante) 
#Hypothèses -> H0 : la variance des résidus est homogène (homoscédasticité) ; H1 : la variance des résidus n'est pas homogène (hétéroscédasticité)
library(lmtest) #permet d'utiliser des tests liés à la régression linéaire
bptest(lm1)
  #p-value>0,05 donc on accepte H0, il y a homoscédasticité des résidus
bptest(lm2)
  #p-value>0,05 donc on accepte H0, il y a homoscédasticité des résidus
bptest(lm3)
  #p-value>0,05 donc on accepte H0, il y a homoscédasticité des résidus
bptest(lm4)
  #p-value>0,05 donc on accepte H0, il y a homoscédasticité des résidus

#Vérification de l'indépendance des résidus (Test Durbin-Watson) (Indépendance = la valeur d'un résidus ne doit pas être liée à la valeur du résidu précédent et ainsi de suite) 
#Hypothèses -> H0 : résidus indépendants ; H1 : résidus dépendants 
library(lmtest)
dwtest(lm1,alternative = "two.sided")
  #Pour lm1, p-value<0,05 donc on rejette H0 donc les résidus sont dépendants, ce qui est cohérent puisque c'est le seul modèle dont la régression linéaire semble appropriée.
dwtest(lm2,alternative = "two.sided")
  #Pour lm2, p-value>0,05 donc on accepte H0 donc les résidus sont indépendants 
dwtest(lm3,alternative = "two.sided")
  #Pour lm3, p-value>0,05 donc on accepte H0 donc les résidus sont indépendants 
dwtest(lm4,alternative = "two.sided")
  #Pour lm4, p-value>0,05 donc on accepte H0 donc les résidus sont indépendants


#IV-Jeu de données réelles "Real_Data"
#IV.1-Quelques éléments de statistique descriptive
# Question 17
#On charge le fichier et les package utiles
happiness <- read.csv("happiness_csv.csv")
library(ggplot2)
library(Rmisc)

summary(happiness) #Affiche les résumés numériques pour chaque variable (moyenne, quantiles, médiane, min et max)

#On calcule les coefficients de corrélation entre les variables par rapport au score de happiness
cor_eco_hap <- cor(happiness$Happiness.Score, happiness$Economy..GDP.per.Capita.)
cor_sant_hap <- cor(happiness$Happiness.Score, happiness$Health..Life.Expectancy.)
cor_lib_hap <- cor(happiness$Happiness.Score, happiness$Freedom)
cor_fam_hap <- cor(happiness$Happiness.Score, happiness$Family)
cor_corr_hap <- cor(happiness$Happiness.Score, happiness$Trust..Government.Corruption.)
cor_gene_hap <- cor(happiness$Happiness.Score, happiness$Generosity)
 #Interprétation : 4 des paramètres étudiés en fonction du score de happiness de ce jeu de données semblent suivre la même tendance. En effet, les nuages de points semblent se regrouper de telle manière qu'il est possible de tracer une droite linéaire de tendance. De plus, à part freedom, les coefficients de corrélation sont proches de 1 montrant une tendance vers la linéarité.

print("Corrélation entre le bonheur et l'économie : ")
print(cor_eco_hap)
print("Corrélation entre le bonheur et la santé : ")
print(cor_sant_hap)
print("Corrélation entre le bonheur et la liberté : ")
print(cor_lib_hap)
print("Corrélation entre le bonheur et la famille : ")
print(cor_fam_hap)
print("Corrélation entre le bonheur et la corruption : ")
print(cor_corr_hap)
print("Corrélation entre le bonheur et la générosité : ")
print(cor_gene_hap)

#On créer les graphiques de dispersion pour visualiser les relations
p1 <- ggplot(happiness, aes(Economy..GDP.per.Capita., Happiness.Score)) + geom_point() + labs(title="Bonheur par rapport à l'économie", x="Economie", y="Score du bonheur")
p2 <- ggplot(happiness, aes(Health..Life.Expectancy., Happiness.Score)) + geom_point() + labs(title="Bonheur par rapport à santé", x="Santé", y="Score du bonheur")
p3 <- ggplot(happiness, aes(Freedom, Happiness.Score)) + geom_point() + labs(title="Bonheur par rapport à la liberté", x="Liberté", y="Score du bonheur")
p4 <- ggplot(happiness, aes(Family, Happiness.Score)) + geom_point() + labs(title="Bonheur par rapport à la famille", x="Famille", y="Score du bonheur")
p5 <- ggplot(happiness, aes(Trust..Government.Corruption., Happiness.Score)) + geom_point() + labs(title="Bonheur par rapport à la corruption", x="Corruption", y="Score du bonheur")
p6 <- ggplot(happiness, aes(Generosity, Happiness.Score)) + geom_point() + labs(title="Bonheur par rapport à la générosité", x="Générosité", y="Score du bonheur")
multiplot(p1,p2,p3,p4,p5,p6,cols=2)
  #Economy semble être celui qui suit le plus une droite de régression, les points sont moins dispersés et suivent globalement la même tendance. On va donc garder ce paramètre pour la suite des questions.

#IV.2-Estimation ponctuelle et par IC des paramètres
#Question 18
#Estimation des paramètres de régression
lm_eco <- lm(Happiness.Score~Economy..GDP.per.Capita., data=happiness)
    #Interprétation des résultats du modèle:
    #lm_eco: Les valeurs p associées aux coefficients sont toutes très faibles (inférieures à 0.001). Cela indique une forte significativité statistique des variables dans le modèle.   
          #Le R-carré est estimé à 0.6099, ce qui signifie que le modèle explique environ 60.99% de la variation du "Happiness Score". Cela indique une bonne adéquation du modèle aux données.
          #La F-statistique est de 243.9 avec une valeur p très faible (< 2.2e-16). Cela suggère que le modèle de régression dans son ensemble est statistiquement significatif. 
          #Donc le modèle de régression suggère qu'il existe une relation significative et positive entre le "Happiness Score" et la variable "Economy (GDP per Capita)". Une augmentation de l'économie (PIB par habitant) est associée à une augmentation du score de bonheur. Le modèle explique également une part importante de la variation du score de bonheur.       
          #lm_eco semble être pertinent pour le modèle de régression linéaire.
  
#Visualisation graphique des régressions
pb5 <- ggplot(happiness, aes(Economy..GDP.per.Capita., Happiness.Score)) + geom_point() + geom_smooth(method="lm",se=FALSE,color="green") + labs(title="Bonheur par rapport à l'économie", x="Economie", y="Score du bonheur")
plot(pb5)
  #Ces données sont pertinentes pour une regression linéaire car on peut observer une faible dispersion des points autour de la droite des moindres carrés.

#Question 19
#Détermination d'un intervalle de confiance des paramètres de la droite de régression pour un alpha à 5%
confint(lm_eco)
    #lm_eco : ICintercept=[3,236;3,7616] et ICeconomy=[1,9376;2,4987]
    #lm_eco présente un petit intervalle de confiance : on a donc moins de chance, au risque 5%, d'accepter à tord les fausses valeurs des paramètres de la droite de régression, prouvant encore que la régression linéaire est juste sur ce paramètre.  

#IV.3-Qualité de l'ajustement
#Question 20
#Le R2 se trouve dans le summary de la fonction lm (interprétation : plus R2 est proche de 1 et plus le modèle est adapté aux données):
summary(lm_eco)
  #lm_eco: Le R-carré est estimé à 0.6099, ce qui signifie que le modèle explique environ 60.99% de la variation du "Happiness Score". Cela indique une bonne adéquation du modèle aux données.
  #lm_eco semble bien adapté pour la régression linéaire.

#Question 21
tol <- 1e-10
  #Le carré de la correlation empirique etre le bonheur et l'argent, et le R^2 de la regression linéaire sont égaux: 
sprintf("Corelation empirique au carré: %s, R carré: %s, égal?: %s", cor_eco_hap^2, summary(lm_eco)$r.squared, abs(cor_eco_hap^2-summary(lm_eco)$r.squared) <= tol)

#Question 22
summary(lm_eco) 
  #Pente estimée à 2,218. Pour déterminer si cette valeur est différente de 0, on effectue un test de student, les résultats de ce test étant déjà indiqué dans la fonction summary(lm_eco).
  #Ici la valeur du t test est de 15,62 et la p-value associée est de 2e-16, très inférieure à 0,05. Donc on rejette H0 et donc le coefficient de la pente est significativement différent de 0.
  #Il existe donc une relation de linéarité significative entre la variable "Economy" et le "Hapiness Score" dans ce modèle de régression. 

#IV.4-Validation des hypothèses
#Question 23
#Calcul des résidus
residus <- residuals(lm_eco)

#Observation de la normalité des résidus corrigés
qqnorm(residus, main="Diagramme quantile-quantile des résidus", ylab="Résidus", xlab="Quantiles théoriques")
qqline(residus)
  #Les points de ce graphique tombent approximativement sur la qqline, cela indique que les résidus sont proches d'une distribution normale.

#Question 24
#Calcul des résidus
residus <- residuals(lm_eco)

#Tracer le graphique des résidus versus la variable explicative
plot(happiness$Economy..GDP.per.Capita., residus, xlab = "Economie", ylab = "Résidus", main="Graphique des résidus vs économie")
abline(h=0, col="red")
  #On peut observer que les résidus sont répartis de façon homogène par rapport à l'axe x=0 ce qui confirme notre hypothèse d’homoscédasticité et l'indépendance des résidus

#IV.5-Prévision
#Question 25
#On prend deux valeurs pour x
newdata <- data.frame(Economy..GDP.per.Capita. = c(1.2, 0.5))

#On calcule les intervalles de confiance
conf <- predict(lm_eco, newdata, interval = "confidence", level = 0.95)
print(conf)
#D'après notre modèle, quand le score Economy..GDP.per.Capita. vaut 1.2, le score du bonheur se situe en moyenne entre 6 et 6.3
#D'après notre modèle, quand le score Economy..GDP.per.Capita. vaut 0.5, le score du bonheur se situe en moyenne entre 4,6 et 4.7


pred <- predict(lm_eco, newdata, interval = "prediction", level = 0.95)
print(pred)
#D'après notre modèle, quand le score Economy..GDP.per.Capita. vaut 1.2, le score du bonheur de 95% des pays se situe en entre 4.7 et 7.5
#D'après notre modèle, quand le score Economy..GDP.per.Capita. vaut 0.5, le score du bonheur de 95% des pays se situe en entre 3.2 et 6

#IV.6-Conclusion
#Question 26
    #Au sein du jeu de données "Hapiness", une relation de linéarité a été mise en évidence entre le score de happiness et la variable economy. En effet, les données se dispersent très peu autours de la droite des moindres carrés.
    #De plus, les résidus sont proches d'une distribution normale et remplissent les conditions d'homoscédasticité et d'indépendance.
    #Ces quatres éléments permettent donc de valider le modèle de régression linéaire entre les variables happiness.score et economy




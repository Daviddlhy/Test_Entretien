#Fichier comportants les fonctions pour l'étude 

# Import des packages necessaires----------------------------------------------------
import_packages <- function(){
  library(ggplot2)
  library(plotly)
  library(dbplyr)
  library(cowplot)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(corrplot)
  library(ggthemes)
  library(caret)
  library(MASS)
  library(randomForest)
  library(party)
  library(car)
  library(cowplot)
  library(readr)
  library(doParallel)
  library(pROC)
  library(data.table)
}
import_packages()



# Fonction qui renvoie le nb de Na par variable en pourcentage-----------------------------
nb_NA <- function(df){
  sort(sapply(df,function(x) sum(is.na(x))/dim(df)[1] *100))
}

# Fonction qui changent les "" en NA lors de l'import du dataset avec fread---------------------------------
add_na <- function(df){
  data.frame(lapply(df, function(column) ifelse(column == "", NA,column)), stringsAsFactors = FALSE)}



# Fonction qui recode les characters en factors --------------------------------------------
recode_character <- function(df){
  for (i in 1:ncol(df)) {
    if(is.character(unlist(df[,i]))) {
      df[,i] <- as.factor(unlist(df[,i]))
    }
  }
  return(df);
}



# Fonction permettant de récupérer l'AUC d'un modèle
test_roc <- function(model, data) {
  
  roc(data$embauche,
      predict(model, data, type = "prob")[, "X1"])
  
}


# Fonction qui nettoie le jeu de données 

Clear_dataset <- function(){
  # L'ajout de dplyr devant select permet de lever des crash avec le select du package Mass
  data <- data %>% dplyr::select(-index) %>% recode_character() %>% mutate(
    age = as.numeric(age),embauche = as.factor(embauche), exp = as.numeric(exp)) %>% filter(
      age >= 18 & note <= 100
    ) %>% na.omit()
  # On enlève les candidats qui n'ont pas ont des années d'expériences mais qui viennent
  data <- data[-which(data$age < data$exp),]
  data <- data[-which(data$diplome =="bac" & (data$age < 18 + data$exp )),]
  data <- data[-which(data$diplome =="licence" & (data$age < 21 + data$exp)), ]
  data <- data[-which(data$diplome =="master" & (data$age < 2 + data$exp )), ]
  data <- data[-which(data$diplome =="doctorat" & (data$age < 26 + data$exp )),]
  return(data)
}





# Fonction qui dessine un boxplot avec 3 niveau------------------------------------------------------------------
graph <- function(df, variable1, variable2,variable3 ,xnom="" ,ynom="", titre="", fillnom=""){
  ggplot(data = df, aes(x = df[[variable1]], y= df[[variable2]], fill = df[[variable3]])) + geom_boxplot() + labs(x = xnom, y= ynom, title = titre , fill = fillnom)
}


# Fonction qui réponds à la 1 ère question------------------------------------------------------------------------
question1 <- function(){
  ### Renvoie un diagramme en barre avec un fill selon le sexe
  graph1 <- ggplot(data = data) + geom_bar(aes(x = specialite, fill = sexe), color="black", position = "dodge") +
    labs(x = "Spécialité", y = "Count", title = "Diagramme Spécialité ~ Sexe")
  
  print(graph1)
  ### Renvoie un tableau de contingence que l'on stock
  tableau <- table(data$sexe, data$specialite) # on stock la table dans un tableau
  ### Application d'un test du Khi-Deux, pour tester l'indépendance
  chisq.test(tableau)
  return(list("test_khideux" = chisq.test(tableau)))
  
}



# Fonction qui réponds à la question 3-----------------------------------------------------------------------
question3 <- function(){
  ### Traçage 
  graphe1 <- ggplot(data = data, aes(x = note, y = exp)) + geom_point(color = "steelblue", size = 2, alpha = 0.2) +               labs( x = "Note Test", y = "Expérience Professionelle", title= "Nuage de points")
  print(graphe1)
  
  ### Méthode du QQ-Plot
  par(mfrow = c(1,2))
  qqnorm(data$note, pch = 1, frame = FALSE, main = "QQ-plot Note")
  qqline(data$note, col = "steelblue", lwd = 2)
  
  qqnorm(data$exp, pch = 1, frame = FALSE, main = "QQ-plot Expérience")
  qqline(data$exp, col = "steelblue", lwd = 2)
  
  #### Test de Shapiro pour tester la normalité des variables (H0 la variable suit une loi normale)
  test_note <- shapiro.test(unlist(sample(data$note, size = 5000, replace = FALSE))) # On ne rejette pas H0
  test_exp <- shapiro.test(unlist(sample(data$exp, size = 5000, replace = FALSE))) # On rejette H0
  
  ### test de corrélation avec la méthode de Kendall 
  test <- cor.test(data$exp, data$note, method="kendall")
  return(list("Note" = test_note, "Expérience" = test_exp, "Test_corrélation" = test))
}


# Fonction qui plot la variable Age -------------------------------------------------------------------------
plot_age <- function(){
  
  a <- ggplot(data = data, aes( x = age,y=..density..)) + 
    geom_histogram(colour="black", fill="lightblue") +
    geom_density(alpha=.3, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed", size=1) +
    labs(x = "Age", y = "Density", title = "Distribution Age")
  
  b <- ggplot(data = data, aes(, y= age)) + 
    geom_boxplot(fill = "lightblue") + labs(y= "Age", title = "Age")
  
  c <- ggplot(data = data, aes(x = embauche, y= age)) + 
    geom_boxplot(fill = "lightblue") + labs(x = "Embauche",y= "Age", title = "Age ~ Embauche")
  
  d <- ggplot(data = data, aes(x = diplome, y= age)) + 
    geom_boxplot(fill = "lightblue") + 
    labs(x = "Diplôme",y= "Age", title = "Age ~ Diplôme")
  
  e <- ggplot(data = data, aes(x = sexe, y= age)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Sexe",y= "Age", title = "Age ~ Sexe")
  
  f <- ggplot(data = data, aes(x = cheveux, y= age)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Couleur",y= "Age", title = "Age ~ Cheveux") 
  
  plot_grid(a,b,c,d,e,f, ncol = 3, nrow = 2)
}
plot_note <- function(){
  
  a <- ggplot(data = data, aes( x = note,y=..density..)) + 
    geom_histogram(colour="black", fill="lightblue") +
    geom_density(alpha=.3, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(note)), color="blue", linetype="dashed", size=1) +
    labs(x = "Note", y = "Density", title = "Note")
  
  b <- ggplot(data = data, aes(, y = note)) + 
    geom_boxplot(fill = "lightblue") + labs(y= "Note test", title = "Note")
  
  c <- ggplot(data = data, aes(x = embauche, y = note)) + 
    geom_boxplot(fill = "lightblue") + labs(x = "Embauche",y= "Note", title = "Note ~ Embauche")
  
  d <- ggplot(data = data, aes(x = diplome, y = note)) + 
    geom_boxplot(fill = "lightblue") + 
    labs(x = "Diplôme",y= "Note test", title = "Note ~ Diplôme")
  
  e <- ggplot(data = data, aes(x = sexe, y = note)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Sexe",y= "Note test", title = "Note ~ Sexe")
  
  f <- ggplot(data = data, aes(x = cheveux, y = note)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Couleur",y= "Note", title = "Note ~ Cheveux") 
  
  plot_grid(a,b,c,d,e,f, ncol = 3, nrow = 2)
}
plot_salaire <- function(){
  a <- ggplot(data = data, aes( x = salaire, y =..density..)) + 
    geom_histogram(colour="black", fill="lightblue") +
    geom_density(alpha=.3, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(salaire)), color="blue", linetype="dashed", size=1) +
    labs(x = "Salaire demandé", y = "Density", title = "Salaire")
  
  b <- ggplot(data = data, aes(, y = salaire)) + 
    geom_boxplot(fill = "lightblue") + labs(y= "Salaire demandé", title = "Salaire")
  
  c <- ggplot(data = data, aes(x = embauche, y = salaire)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Sexe",y = "Salaire demandé", title = "Salaire ~ Embauche")
  
  d <- ggplot(data = data, aes(x = diplome, y = salaire)) + 
    geom_boxplot(fill = "lightblue") + 
    labs(x = "Diplôme",y= " Salaire demandé", title = "Salaire ~ Diplôme")
  
  e <- ggplot(data = data, aes(x = sexe, y = salaire)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Embauche", y= "Salaire demandé", title = "Salaire ~ Sexe")
  
  f <- ggplot(data = data, aes(x = cheveux, y = salaire)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Couleur", y= "Salaire demandé", title = "Salaire ~ Cheveux") 
  
  plot_grid(a,b,c,d,e,f, ncol = 3, nrow = 2) 
}
plot_exp <- function(){
  
  a <- ggplot(data = data, aes( x = exp,y=..density..)) + 
    geom_histogram(colour="black", fill="lightblue") +
    geom_density(alpha=.3, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(exp)), color="blue", linetype="dashed", size=1) +
    labs(x = "exp", y = "Density", title = "Expérience")
  
  b <- ggplot(data = data, aes(, y = exp)) + 
    geom_boxplot(fill = "lightblue") + labs(y= "Expérience", title = "Expérience")
  
  c <- ggplot(data = data, aes(x = embauche,y = exp)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Embauche",y= "Expérience", title = "Expérience ~ Embauche")
  
  d <- ggplot(data = data, aes(x = diplome, y = exp)) + 
    geom_boxplot(fill = "lightblue") + 
    labs(x = "Diplôme",y= "Expérience", title = "Expérience ~ Diplôme")
  
  e <- ggplot(data = data, aes(x = sexe, y = exp)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="embauche",y= "Expérience", title = "Expérience ~ Embauche")
  
  f <- ggplot(data = data, aes(x = cheveux, y = exp)) + 
    geom_boxplot(fill = "lightblue") + labs(x ="Couleur",y= "Expérience", title = "Expérience ~ Cheveux" ) 
  
  plot_grid(a,b,c,d,e,f, ncol = 3, nrow = 2)
}


# Fonction traçant la variable embauche en fonction des autres variables du dataset
plot_embauche <- function(){
  a <- ggplot(data = data, aes(x = embauche,)) + geom_bar() + labs(x = "Embauche", y = "Count", title = "Candidature        Admise")
  
  b <- ggplot(data = data, aes(x = embauche,)) + geom_bar(aes(fill = cheveux), position = "dodge",col ="black") +           labs(x =   "Embauche", y = "Count", title = "Embauche ~ Couleur Cheveux")
  
  c <- ggplot(data = data, aes(x = embauche,)) + geom_bar(aes(fill = sexe), position = "dodge",col ="black") + labs(x =      "Embauche", y = "Count", title = "Embauche ~ Sexe")
  
  d <- ggplot(data = data, aes(x = embauche,)) + geom_bar(aes(fill = diplome), position = "dodge",col ="black") +           labs(x =   "Embauche", y = "Count", title = "Embauche ~ Diplôme")
  
  e <- ggplot(data = data, aes(x = embauche,)) + geom_bar(aes(fill = specialite), position = "dodge",col ="black") +        labs(x = "Embauche", y = "Count", title = "Embauche ~ Spécialité")
  
  
  f <- ggplot(data = data, aes(x = embauche,)) + geom_bar(aes(fill = dispo), position = "dodge",col ="black") + labs(x      = "Embauche", y = "Count", title = "Embauche ~ Disponibilité")
  plot_grid(a,b,c,d,e,f, ncol = 3, nrow = 2)
}



# Fonction qui utilise les fonctions précédente de graphique-------------------------------------------------
graphique <- function(variable1){
  variable1 <- as.character(variable1)
  if(variable1=="age"){
    return(plot_age())
  }
  if(variable1=="note"){
    return(plot_note())
  }
  if(variable1=="exp"){
    return(plot_exp())
  }
  if(variable1=="salaire"){
    return(plot_salaire())
  }
  if(variable1=="embauche"){
    return(plot_embauche())
  }
}

# Import le dataset------------------------------------------------------------------------------------------ 
data <- fread("Data/Data.csv", na.strings = "NA")

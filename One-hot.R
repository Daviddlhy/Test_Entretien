## Maintenant avec un on hot encoding

# Utilisaiton du on Hot encoding
dummies <- dummyVars(embauche ~ diplome+specialite+note+dispo , data = data)
data2 <- predict(dummies, newdata = data)
data2 <- data.frame(data2)
data2 <- data %>% dplyr::select(embauche, exp, age, salaire) %>% cbind(data2)
rm(dummies)
gc()
head(data2)


#Mettre les colonnes 1 et 2 à la même échelle
data2[,c(2,3,4,15)] = scale(data2[,c(2,3,4,15)])

# Mise en factor des variables : 
data2 <- data2 %>% mutate(
  diplome.bac = factor(diplome.bac),
  diplome.licence = factor(diplome.licence),
  diplome.master = factor(diplome.master),
  specialite.archeologie = factor(specialite.archeologie),
  specialite.forage = factor(specialite.forage),
  specialite.geologie = factor(specialite.geologie),
  dispo.oui = factor(dispo.oui)
)


data2 <- data2 %>% dplyr::select(-diplome.doctorat, -specialite.detective, -dispo.non)


### Partitionemment en jeu d'entrainement et de test pour le dataset 2
intrain<- createDataPartition(data$embauche,p=0.7,list=FALSE) # On prend 70% en jeu d'entrainement 
training2<- data2[intrain,]
testing2<- data2[-intrain,]



model_poids2 = ifelse(training2$embauche == "X1", 
                      nrow(training2) / (2 * table(training2$embauche)[2]),
                      nrow(training2) / (2 * table(training2$embauche)[1]))



training2 <- training2  %>% 
  mutate(embauche = factor(embauche, 
                           labels = make.names(levels(embauche))))

testing2 <- testing2  %>% 
  mutate(embauche = factor(embauche, 
                           labels = make.names(levels(embauche))))


ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     repeats = 5,
                     allowParallel = TRUE)

set.seed(5627)

cl <- makePSOCKcluster(5)
registerDoParallel(cl)

orig_fit2 <- train(embauche ~ .,
                  data = training2,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

orig_fit2 %>% test_roc(data = testing2) %>% 
  auc()

ctrl$seeds <- orig_fit$control$seeds

### Random Forest 

rf_default2 <- train( embauche~., data= training2, method="rf", metric = "ROC", trControl = ctrl)

rf_default2 %>%
  test_roc(data = testing2) %>%
  auc()



# Sous-Echantillonage
ctrl$sampling <- "down"

down_fit2 <- train(embauche ~ .,
                  data = training2,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)


down_fit2 %>%
  test_roc(data = testing2) %>%
  auc()




# Sur-Echantillonage

ctrl$sampling <- "up"

up_fit2 <- train(embauche ~ .,
                data = training2,
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)


up_fit2 %>%
  test_roc(data = testing2) %>%
  auc()





# Smote Echantillonage

ctrl$sampling <- "smote"

smote_fit2 <- train(embauche ~ .,
                   data = training2,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)
smote_fit2 %>%
  test_roc(data = testing2) %>%
  auc()


weighted_fit2 <- train(embauche ~ .,
                      data = training2,
                      method = "gbm",
                      verbose = FALSE,
                      weights = model_poids,
                      metric = "ROC",
                      trControl = ctrl)

weighted_fit2 %>% test_roc(data = testing2) %>% 
  auc()

stopCluster(cl)





model_list2 <- list(random_forest = rf_default2,
                   original = orig_fit2,
                   weighted = weighted_fit2,
                   down = down_fit2,
                   up = up_fit2,
                   SMOTE = smote_fit2)

model_list_roc2 <- model_list2 %>%
  map(test_roc, data = testing2)

model_list_roc2 %>%
  map(auc)




results_list_roc2 <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc2){
  
  results_list_roc2[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc2 <- bind_rows(results_list_roc2)

# Affichage courbe ROC pour les 5 modèles.

custom_col <- c("#3399FF","#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

a <- ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc2) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

ggplotly(a)



### Affichage des variables importantes par ordre décroissant du modèle
plot(varImp(rf_default2), main ="Importance des variables pour le modèle 3")
plot(varImp(rf_default))

plot(varImp(orig_fit2))

summary(orig_fit, las = 2)
summary(up_fit2, las = 2)

rm(list=ls())


library(readr)
library(corrplot)
library(gridExtra)
library(grid)





load_meteo_data = function(data_path) {
  meteo_data <- read.csv2(data_path, sep = ",", dec = ".")
  
  if ('pluie.demain' %in% names(meteo_data)) {
    meteo_data$pluie.demain <- as.integer(as.logical(meteo_data$pluie.demain))
  }
  # 
  # meteo_data <- subset(meteo_data, select = -c(Hour, Minute, Day, X, Year, Month))
  
  
  return(meteo_data)
}

# Chargement des données
meteo.train = load_meteo_data("C:/Users/asus2/OneDrive/Desktop/Big Data/01-MLG/Projet/meteo.train.csv")
meteo.test = load_meteo_data("C:/Users/asus2/OneDrive/Desktop/Big Data/01-MLG/Projet/meteo.test.csv")

# Visuation des premieres lignes
head(meteo.train)
summary(meteo.train)
summary(meteo.train$pluie.demain)
hist(meteo.train$pluie.demain)


# Affichage des correlations sous forme de corrplot
pluie.demain_TRUE <- meteo.train$pluie.demain == "TRUE"
columns_excluded <- c("X","Hour", "Minute", "pluie.demain")
data_for_corr <- meteo.train[, !(names(meteo.train) %in% columns_excluded)]
data_for_corr <- cbind(data_for_corr, pluie.demain_TRUE)
cor_matrix <- cor(data_for_corr, use = "complete.obs")
corrplot(
  cor_matrix,
  # cor(meteo.train[, 7:46]),
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 90,
  cl.cex = 0.7,
  tl.cex = 0.6,
  mar = c(0, 0, 1, 0),
  title = "Correlation Matrix"
)



# Fonction qui affiche les correlations au dela d'un threshold
diplay_correl_above_threshold = function(meteo_df, threshold)
{
  cat("\n col1:col2:correl")
  for (i in 7:45) {
    k = i + 1
    for (j in k:46) {
      cor_ij = cor(meteo_df[, i], meteo_df[, j])
      if (is.na(cor_ij)) {
        cor_ij = 0
      }
      if (abs(cor_ij) >= threshold) {
        cat("\n", colnames(meteo_df[i]))
        cat(" : ", colnames(meteo_df[j]))
        cat(" : ", cor_ij)
      }
    }
  }
}

diplay_correl_above_threshold(meteo_df = meteo.train, 0.85)



# Affichage des correlations avec 'pluie.demain'
for (col in colnames(meteo.train))
{
  if (!(col %in% c('pluie.demain', 'X', 'Hour', 'Minute'))){
    correl = cor(meteo.train[, col], meteo.train[, 'pluie.demain'], use = "pair")
    cat("\n correl [", col, ":", "pluie.demain:]", correl)
  }
}


# Affichage des correlations avec 'pluie.demain_TRUE'
for (col in colnames(data_for_corr))
{
  if (!(col %in% c('pluie.demain_TRUE', 'X', 'Hour', 'Minute'))){
    correl = cor(data_for_corr[, col], data_for_corr[, 'pluie.demain_TRUE'], use = "pair")
    cat("\n correl [", col, ":", "pluie.demain_TRUE:]->", correl)
  }
}

# display box plot
display_box_plot = function(meteo_df, variables, plotTitle)  {
  plot_list <- list()
  for (var in variables) {
    Bplot <- ggplot(meteo_df, aes_string(x = "pluie.demain", y = var, fill = "pluie.demain")) + geom_boxplot() + scale_fill_manual(values = c('TRUE' = "steelblue", 'FALSE' = "grey")) + labs(title = var, x = "pluie.demain", y = var) + theme_minimal() + theme(legend.position = "none", plot.title = element_text(size = 8)) 
            #
            # + labs(title = var, x = "pluie.demain", y = var) 
            # + theme_minimal() 
            # + theme(legend.position = "none", plot.title = element_text(size = 8))
    plot_list[[var]] <- Bplot
  }
  
  title <- textGrob(plotTitle,
                    gp = gpar(fontsize = 15, fontface = "bold"))
  grid.arrange(arrangeGrob(grobs = plot_list, ncol = 3), top = title)
}

# Cas des variables liées à la nébulosité
display_box_plot(
  meteo.train,
  c(
    "High.Cloud.Cover.daily.max..high.cld.lay.", #y
    "High.Cloud.Cover.daily.mean..high.cld.lay.", #y
    "High.Cloud.Cover.daily.min..high.cld.lay.",
    "Medium.Cloud.Cover.daily.max..mid.cld.lay.", #y
    "Medium.Cloud.Cover.daily.mean..mid.cld.lay.", #y
    "Medium.Cloud.Cover.daily.min..mid.cld.lay.", 
    "Low.Cloud.Cover.daily.max..low.cld.lay.", 
    "Low.Cloud.Cover.daily.mean..low.cld.lay.", #y
    "Low.Cloud.Cover.daily.min..low.cld.lay.",
    "Total.Cloud.Cover.daily.max..sfc.", 
    "Total.Cloud.Cover.daily.mean..sfc.", #y
    "Total.Cloud.Cover.daily.min..sfc."
  ),
  "Box plot des variables liées à la nébulosité par rapport à pluie.demain"
)

# Cas des variables liées à la température
display_box_plot(
  meteo.train,
  c(
    "Temperature.daily.max..2.m.above.gnd.", #y
    "Temperature.daily.mean..2.m.above.gnd.", #y
    "Temperature.daily.min..2.m.above.gnd." #y
  ),
  "Box plot des variables liées à la température par rapport à pluie.demain"
)

# Cas des variables liées au vent
display_box_plot(
  meteo.train,
  c(
    "Wind.Gust.daily.max..sfc.", #y
    "Wind.Gust.daily.mean..sfc.",
    "Wind.Gust.daily.min..sfc.",
    "Wind.Speed.daily.max..10.m.above.gnd.",
    "Wind.Speed.daily.mean..10.m.above.gnd.",
    "Wind.Speed.daily.min..10.m.above.gnd.",
    "Wind.Speed.daily.max..80.m.above.gnd.",
    "Wind.Speed.daily.mean..80.m.above.gnd.", #y
    "Wind.Speed.daily.min..80.m.above.gnd.",
    "Wind.Speed.daily.max..900.mb.", #y
    "Wind.Speed.daily.mean..900.mb.",
    "Wind.Speed.daily.min..900.mb.",
    "Wind.Direction.daily.mean..10.m.above.gnd.",
    "Wind.Direction.daily.mean..80.m.above.gnd.",
    "Wind.Direction.daily.mean..900.mb." #y
  ),
  "Box plot des variables liées au vent par rapport à pluie.demain"
)


# Cas des variables liées au taux d’humidité
display_box_plot(
  meteo.train,
  c(
    "Relative.Humidity.daily.max..2.m.above.gnd.",
    "Relative.Humidity.daily.mean..2.m.above.gnd.",
    "Relative.Humidity.daily.min..2.m.above.gnd."
  ),
  "Box plot des variables liées au taux d’humidité par rapport à pluie.demain"
)

# Cas des variables liées à la pression 

display_box_plot(
  meteo.train,
  c(
    "Mean.Sea.Level.Pressure.daily.max..MSL.", #y
    "Mean.Sea.Level.Pressure.daily.mean..MSL.", #y
    "Mean.Sea.Level.Pressure.daily.min..MSL." #y
  ),
  "Box plot des variables liées à la pression par rapport à pluie.demain"
)


display_box_plot(
  meteo.train,
  c(
    "Total.Precipitation.daily.sum..sfc.", #y
    "Shortwave.Radiation.daily.sum..sfc.",
    "Snowfall.amount.raw.daily.sum..sfc.",
    "Sunshine.Duration.daily.sum..sfc." #y
  ),
  "Box plot des variables par rapport à pluie.demain"
)


display_box_plot(
  meteo.train,
  c(
    "High.Cloud.Cover.daily.max..high.cld.lay.", #y
    "High.Cloud.Cover.daily.mean..high.cld.lay.", #y
    "High.Cloud.Cover.daily.min..high.cld.lay.",
    "Medium.Cloud.Cover.daily.max..mid.cld.lay.", #y
    "Medium.Cloud.Cover.daily.mean..mid.cld.lay.", #y
    "Medium.Cloud.Cover.daily.min..mid.cld.lay.", 
    "Low.Cloud.Cover.daily.max..low.cld.lay.", 
    "Low.Cloud.Cover.daily.mean..low.cld.lay.", #y
    "Low.Cloud.Cover.daily.min..low.cld.lay.",
    "Total.Cloud.Cover.daily.max..sfc.", 
    "Total.Cloud.Cover.daily.mean..sfc.", #y
    "Total.Cloud.Cover.daily.min..sfc."
  ),
  "Box plot des variables liées à pluie.demain"
)

display_box_plot(
  meteo.train,
  c(
    "High.Cloud.Cover.daily.max..high.cld.lay.", #y
    "High.Cloud.Cover.daily.mean..high.cld.lay.", #y
    "Medium.Cloud.Cover.daily.max..mid.cld.lay.", #y
    "Medium.Cloud.Cover.daily.mean..mid.cld.lay.", #y
    "Low.Cloud.Cover.daily.mean..low.cld.lay.", #y
    "Total.Cloud.Cover.daily.mean..sfc.", #y
    "Temperature.daily.max..2.m.above.gnd.", #y
    "Temperature.daily.mean..2.m.above.gnd.", #y
    "Temperature.daily.min..2.m.above.gnd.", #y
    "Wind.Gust.daily.max..sfc.", #y
    "Wind.Speed.daily.mean..80.m.above.gnd.", #y
    "Wind.Speed.daily.max..900.mb.", #y
    "Wind.Direction.daily.mean..900.mb.", #y
    "Mean.Sea.Level.Pressure.daily.max..MSL.", #y
    "Mean.Sea.Level.Pressure.daily.mean..MSL.", #y
    "Mean.Sea.Level.Pressure.daily.min..MSL.", #y
    "Total.Precipitation.daily.sum..sfc.", #y
    "Sunshine.Duration.daily.sum..sfc." #y
  ),
  "Box plot des variables qui semblent avoir un effet sur notre variable d'interet"
)


# Précipitations : Total.Precipitation.daily.sum..sfc.
boxplot(meteo.train$Total.Precipitation.daily.sum..sfc. ~ meteo.train$pluie.demain)
# la valeur totalmeteo_df = # la valeur totale de precipitations est lié a la pluie le jour d'apres

#	Minutes d’ensoleillement : Sunshine.Duration.daily.sum..sfc.
boxplot(meteo.train$Sunshine.Duration.daily.sum..sfc. ~ meteo.train$pluie.demain)
# la valeur totale de minutes d'ensoleillement est lié a la pluie le jour d'apres

#	Rayonnement solaire : Shortwave.Radiation.daily.sum..sfc.
boxplot(meteo.train$Shortwave.Radiation.daily.sum..sfc. ~ meteo.train$pluie.demain)
# ce n'est pas tres clair si le rayonnement solaire est lié a la pluie le jour d'apres




# on définit donc un premier modele qui contient toutes les variables à 
# l'exception de celle qui n'apportent aucune information
model_0 = glm(pluie.demain ~  . -X -Day -Hour - Minute - Year, data = meteo.train, family = binomial)
summary(model_0)

# choix automatique du model
model_step_forward = step(model_0, direction = "forward")
model_step_backward = step(model_0, direction = "backward")
model_step_mixed = step(model_0, direction = "both")

summary(model_step_forward)
summary(model_step_backward)
summary(model_step_mixed)

# analyse de la deviance de notre modele model_step_forward
summary(model_step_forward)
# Pour la déviance, la sortie indique
# Null deviance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1238.5  on 1138  degrees of freedom
# Le test par rapport au modèle sans covariable donne
pchisq(1635.4 - 1238.5, 1179 - 1138, lower = F)
# p-valeur très faible : on rejette le modèle sans covariable
# Le test par rapport au modèle saturé donne
pchisq(1238.5, 1138, lower = F)
# p-valeur <5% : on rejette notre modèle
anova(model_step_forward)

# analyse de la deviance de notre modele model_step_backward/model_step_mixed
summary(model_step_backward)
# Pour la déviance, la sortie indique
# Null deviance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1249.6  on 1162  degrees of freedom
# Le test par rapport au modèle sans covariable donne
pchisq(1635.4 - 1249.6, 1179 - 1162, lower = F)
# p-valeur très faible : on rejette le modèle sans covariable
# Le test par rapport au modèle saturé donne
pchisq(1249.6, 1162, lower = F)
# p-valeur <5% : on rejette notre modèle
anova(model_step_backward)

car::vif(model_0)

# Gestion de la colinearité
model_0_vif = glm(pluie.demain ~  . -X -Day -Hour - Minute - Year - Temperature.daily.mean..2.m.above.gnd., data = meteo.train, family = binomial)

car::vif(model_0_vif)

model_0_vif = glm(pluie.demain ~  . -X -Day -Hour - Minute - Year 
                  - Temperature.daily.mean..2.m.above.gnd.
                  - Mean.Sea.Level.Pressure.daily.mean..MSL., data = meteo.train, family = binomial)
car::vif(model_0_vif)
model_0_vif = glm(pluie.demain ~  . -X -Day -Hour - Minute - Year 
                  - Temperature.daily.mean..2.m.above.gnd.
                  - Mean.Sea.Level.Pressure.daily.mean..MSL.
                  - Wind.Speed.daily.mean..10.m.above.gnd.
                  - Wind.Speed.daily.mean..900.mb., data = meteo.train, family = binomial)
car::vif(model_0_vif)
model_0_vif = glm(pluie.demain ~  . -X -Day -Hour - Minute - Year 
                  - Temperature.daily.mean..2.m.above.gnd.
                  - Mean.Sea.Level.Pressure.daily.mean..MSL.
                  - Wind.Speed.daily.mean..10.m.above.gnd.
                  - Wind.Speed.daily.mean..900.mb.
                  - Wind.Direction.daily.mean..80.m.above.gnd., data = meteo.train, family = binomial)
car::vif(model_0_vif)
model_0_vif = glm(
  pluie.demain ~  . - X - Day - Hour - Minute - Year
  - Temperature.daily.mean..2.m.above.gnd.
  - Mean.Sea.Level.Pressure.daily.mean..MSL.
  - Wind.Speed.daily.mean..10.m.above.gnd.
  - Wind.Speed.daily.mean..900.mb.
  - Wind.Direction.daily.mean..80.m.above.gnd.
  - Total.Cloud.Cover.daily.mean..sfc.
  - Wind.Speed.daily.mean..80.m.above.gnd.
  - Temperature.daily.max..2.m.above.gnd.
  - Relative.Humidity.daily.mean..2.m.above.gnd.
  - Wind.Gust.daily.mean..sfc.
  - Wind.Speed.daily.max..10.m.above.gnd.
  - Wind.Speed.daily.min..10.m.above.gnd.
  - Sunshine.Duration.daily.sum..sfc.,
  data = meteo.train,
  family = binomial
)
car::vif(model_0_vif)


# choix automatique du model
model_step_vif_cleaned_forward = step(model_0_vif, direction = "forward")
model_step_vif_cleaned_backward = step(model_0_vif, direction = "backward")
model_step_vif_cleaned_mixed = step(model_0_vif, direction = "both")

summary(model_step_vif_cleaned_forward)
summary(model_step_vif_cleaned_backward)
summary(model_step_vif_cleaned_mixed)



summary(model_step_vif_cleaned_forward)
# Pour la déviance, la sortie indique
# Null deviance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1274.7  on 1151  degrees of freedom
# Le test par rapport au modèle sans covariable donne
pchisq(1635.4 - 1274.7, 1179 - 1151, lower = F)
# p-valeur très faible : on rejette le modèle sans covariable
# Le test par rapport au modèle saturé donne
pchisq(1274.7, 1151, lower = F)
# p-valeur <5% : on rejette notre modèle

summary(model_step_vif_cleaned_mixed)
# Pour la déviance, la sortie indique
# Null deviance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1282.1  on 1165  degrees of freedom
# Le test par rapport au modèle sans covariable donne
pchisq(1635.4 - 1282.1, 1179 - 1165, lower = F)
# p-valeur très faible : on rejette le modèle sans covariable
# Le test par rapport au modèle saturé donne
pchisq(1282.1, 1165, lower = F)
# p-valeur <5% : on rejette notre modèle

# choix manuel du model
# Approche manuel
model_manuel = glm(
  pluie.demain ~  High.Cloud.Cover.daily.max..high.cld.lay.
  + High.Cloud.Cover.daily.mean..high.cld.lay.
  + Medium.Cloud.Cover.daily.max..mid.cld.lay.
  + Medium.Cloud.Cover.daily.mean..mid.cld.lay.
  + Low.Cloud.Cover.daily.mean..low.cld.lay.
  + Total.Cloud.Cover.daily.mean..sfc.
  + Temperature.daily.mean..2.m.above.gnd.
  + Wind.Gust.daily.max..sfc.
  + Wind.Speed.daily.mean..80.m.above.gnd.
  + Wind.Speed.daily.max..900.mb.
  + Wind.Direction.daily.mean..900.mb.
  + Mean.Sea.Level.Pressure.daily.max..MSL.
  + Total.Precipitation.daily.sum..sfc.
  + Sunshine.Duration.daily.sum..sfc.,
  data = meteo.train,
  family = binomial
)

summary(model_manuel)
# Pour la déviance, la sortie indique
# Null deviance: 1635.4  on 1179  degrees of freedom
# Residual deviance: 1300.2  on 1165  degrees of freedom
# Le test par rapport au modèle sans covariable donne
pchisq(1635.4 - 1300.2, 1179 - 1165, lower = F)
# p-valeur très faible : on rejette le modèle sans covariable
# Le test par rapport au modèle saturé donne
pchisq(1300.2, 1165, lower = F)
# p-valeur <5% : on rejette notre modèle


anova(model_step_forward, model_0, test="LRT")
anova(model_step_backward, model_0, test="LRT")

anova(model_manuel, model_0, test="LRT")
anova(model_step_vif_cleaned_mixed, model_0, test="LRT")
anova(model_step_vif_cleaned_forward, model_0, test="LRT")




# Comparaison entre le modele  model_step_forward et le modele  model_step_backward
AIC(model_step_forward)
AIC(model_step_backward)
AIC(model_manuel)
AIC(model_step_vif_cleaned_mixed)
AIC(model_step_vif_cleaned_forward)


library(ROCR)


pred_model_0 = predict(model_0, type = "response") >= 0.5
pred_model_step_forward = predict(model_step_forward, type = "response") >= 0.5
pred_model_step_backward = predict(model_step_backward, type = "response") >= 0.5
pred_model_manuel = predict(model_manuel, type = "response") >= 0.5
pred_model_step_vif_cleaned_mixed = predict(model_step_vif_cleaned_mixed, type = "response") >= 0.5
pred_model_step_vif_cleaned_forward = predict(model_step_vif_cleaned_forward, type = "response") >= 0.5


mean(pred_model_0 == meteo.train$pluie.demain)
mean(pred_model_step_forward == meteo.train$pluie.demain)
mean(pred_model_step_backward == meteo.train$pluie.demain)
mean(pred_model_manuel == meteo.train$pluie.demain)
mean(pred_model_step_vif_cleaned_mixed == meteo.train$pluie.demain)
mean(pred_model_step_vif_cleaned_forward == meteo.train$pluie.demain)


# Validation croisée k-fold
k = 10
index = sample(1:k, nrow(meteo.train), replace=T)

res.model_0.logistique = rep(NA, k)
res.model_0.probit = rep(NA, k)
res.model_step_forward.logistique = rep(NA, k)
res.model_step_forward.probit = rep(NA, k)
res.model_step_backward.logistique = rep(NA, k)
res.model_step_backward.probit = rep(NA, k)
res.model_manuel.logistique = rep(NA, k)
res.model_manuel.probit = rep(NA, k)
res.model_step_vif_cleaned_mixed.logistique = rep(NA, k)
res.model_step_vif_cleaned_mixed.probit = rep(NA, k)
res.model_step_vif_cleaned_forward.logistique = rep(NA, k)
res.model_step_vif_cleaned_forward.probit = rep(NA, k)


ComputeReg = function(reg_model, link_function, i) {
  reg = glm(formula(reg_model),
            family = binomial(link = link_function),
            data = meteo.train[index != i, ])
  
  pred.reg = predict(reg, newdata=meteo.train[index == i, ],
                            type="response")
  
  
  erreur_l1 = mean(abs(pred.reg - meteo.train[index==i, "pluie.demain"]), na.rm = T)
  
  return(erreur_l1)
}


for(i in 1:k){
  
  res.model_0.logistique[i] = ComputeReg(model_0, "logit", i)
  res.model_0.probit[i] = ComputeReg(model_0, "probit", i)
 
  res.model_step_forward.logistique[i] = ComputeReg(model_step_forward, "logit", i)
  res.model_step_forward.probit[i] = ComputeReg(model_step_forward, "probit", i)
  
  res.model_step_backward.logistique[i] = ComputeReg(model_step_backward, "logit", i)
  res.model_step_backward.probit[i] = ComputeReg(model_step_backward, "probit", i)
  
  res.model_manuel.logistique[i] = ComputeReg(model_manuel, "logit", i)
  res.model_manuel.probit[i] = ComputeReg(model_manuel, "probit", i)
 
  res.model_step_vif_cleaned_mixed.logistique[i] = ComputeReg(model_step_vif_cleaned_mixed, "logit", i)
  res.model_step_vif_cleaned_mixed.probit[i] = ComputeReg(model_step_vif_cleaned_mixed, "probit", i) 
  
  res.model_step_vif_cleaned_forward.logistique[i] = ComputeReg(model_step_vif_cleaned_forward, "logit", i)
  res.model_step_vif_cleaned_forward.probit[i] = ComputeReg(model_step_vif_cleaned_forward, "probit", i) 
  
}

mean(res.model_0.logistique)
mean(res.model_0.probit)
mean(res.model_step_forward.logistique)
mean(res.model_step_forward.probit)
mean(res.model_step_backward.logistique)
mean(res.model_step_backward.probit)
mean(res.model_manuel.logistique)
mean(res.model_manuel.probit)
mean(res.model_step_vif_cleaned_mixed.logistique)
mean(res.model_step_vif_cleaned_mixed.probit)
mean(res.model_step_vif_cleaned_forward.logistique)
mean(res.model_step_vif_cleaned_forward.probit)


# Determination du seuil optimal
seuil = seq(0, 1, by=.01)
pred.model_step_backward = predict(model_step_backward, newdata = meteo.train, type = "response")



res = rep(NA, length(seuil))
for(i in 1:length(seuil)){
  pred2 = (pred.model_step_backward  >= seuil[i])
  res[i] = 1 * sum(pred2 & meteo.train$pluie.demain==0) + 
    1 * sum(!pred2 & meteo.train$pluie.demain==1)
}

plot(seuil, res, type="l")
seuil[which.min(res)]


# Generation des predictions
pred.meteo.test = predict(model_step_backward, newdata = meteo.test, type = "response")
pluie.demain = (pred.meteo.test>=seuil[which.min(res)])
prediction <- cbind(meteo.test, pluie.demain)
View(prediction)
write.table(prediction,"prediction_RAHMANI_Rachid.csv",sep=";",col.names=TRUE)


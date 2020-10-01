###### Joziani Mota Vieira
  
## Pacotes e funções

rm(list=ls())
options(OutDec=",")
if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
if(!require(caret)){install.packages('caret'); require(caret)} 
if(!require(ROCR)){install.packages('ROCR'); require(ROCR)}

tab <- function(x){
  t1 <- cbind(table(x), round(prop.table(table(x))*100, 2))
}

basic <- function(x, more=F) {
  stats <- list()
  stats$N <- length(x)
  stats$Media <- round(mean(x),2)
  stats$D.P <- round(sd(x),2)
  
  t1 <- unlist(stats)
  names(t1) <- c("N", "Média", "D.P.")
  t1
}

metricas <- function(mod, mod.class, mod.probs){
  #predicting the test data
  mod.labels <- as.factor(Teste$DEATH_EVENT)
  mod.confusion <- confusionMatrix(mod.labels, mod.class)
  mod.accuracy <- round(as.numeric(mod.confusion$overall[1]),3)
  mod.sensitivity <- round(as.numeric(mod.confusion$byClass[1]),3)
  mod.specificity <- round(as.numeric(mod.confusion$byClass[2]),3)
  
  #roc analysis for test data
  mod.prediction <- prediction(mod.probs, mod.labels)
  mod.performance <- performance(mod.prediction,"tpr","fpr")
  mod.auc <- round(performance(mod.prediction,"auc")@y.values[[1]],3)
  
  tabela <- data.frame(Acurácia = mod.accuracy,
                       Sensibilidade  = mod.sensitivity,
                       Especificidade = mod.specificity,
                       AUC = mod.auc)
  
  return(list(medida=tabela, performace=mod.performance))
}


## Leitura dos dados

df <- readxl::read_xlsx('dados/Dados R.xlsx')
df


## Análises Descritivas

sum(is.na(df) == T)

tabela <- t(apply(df %>% dplyr::select(DEATH_EVENT,
                                       anaemia,
                                       diabetes,
                                       high_blood_pressure,
                                       sex,
                                       smoking), 2, tab))
colnames(tabela) <- c('N - 0', 'N - 1', '% - 0', '% - 1')

tabela


t(apply(df %>% dplyr::select(age,
                             creatinine_phosphokinase,
                             ejection_fraction,
                             platelets,
                             serum_creatinine,
                             serum_sodium), 2, basic))


## Correlação

corelacao <- cor(as.matrix(df), method = 'spearman')

p.mat <- ggcorrplot::cor_pmat(as.matrix(df))

ggcorrplot::ggcorrplot(corelacao, hc.order = TRUE, 
                       type = 'lower', 
                       lab = TRUE, 
                       lab_size = 3, 
                       method = 'circle', 
                       colors = c('tomato2', 'white', 'springgreen3'), 
                       title= 'Correlograma entre variáveis', 
                       p.mat = p.mat,
                       ggtheme = theme_bw)


## Método Holdout

set.seed(123)
id_sample <-  1:(dim(df)[1]) %>% 
  sample(size = 209, replace = F)

Treino <- data.frame(df[id_sample,]) # 70%
Treino %>% dim()

Teste <- data.frame(df[-id_sample,]) # 30%
Teste %>% dim()

# verificar se existe preditores com variância zero
length(nearZeroVar(Treino))
length(nearZeroVar(Teste))

# Enumera e resolve as combinações lineares em uma matriz numérica
findLinearCombos(Treino %>% dplyr::select(-DEATH_EVENT) %>% 
                   data.matrix)
findLinearCombos(Teste %>% dplyr::select(-DEATH_EVENT) %>% 
                   data.matrix)


## Modelos

### Regressão Logística

f <- as.formula(paste('DEATH_EVENT ~',
                      paste(names(Treino %>% dplyr::select(-DEATH_EVENT)),
                            collapse =' + ')))
f

mod_reg <- glm(f, 
               family = binomial, 
               data = Treino)

cutoff_reg <- lapply(as.list(seq(0.3,0.7,0.01)),
                     function(x){
                       table(ifelse(predict(mod_reg, Treino, 
                                            type = 'response') >= x, 1, 0), 
                             Treino$DEATH_EVENT) %>% 
                         prop.table() %>% 
                         diag() %>% 
                         sum()})

ponto_corte_reg <- seq(0.3,0.7,0.01)[which.max(cutoff_reg)]

mod.probs_reg <- predict(mod_reg, Teste, type = 'response')

mod.class_reg <- as.factor(ifelse(mod.probs_reg >= ponto_corte_reg, 1, 0))

medidas_reg <- metricas(mod_reg, mod.class_reg, mod.probs_reg)$medida

performace_reg <- metricas(mod_reg, mod.class_reg, mod.probs_reg)$performace


### Random Forest

set.seed(123)
mod_rf <- randomForest::randomForest(f, data = Treino,
                                     importance = TRUE)

cutoff_rf <-  lapply(as.list(seq(0.3,0.7,0.01)),
                     function(x){
                       table(ifelse(predict(mod_rf, Treino, 
                                            type = 'response')>=x, 1, 0), 
                             Treino$DEATH_EVENT) %>% 
                         prop.table() %>% 
                         diag() %>% 
                         sum()})

ponto_corte_rf <-  seq(0.3,0.7,0.01)[which.max(cutoff_rf)]

mod.probs_rf <- predict(mod_rf, Teste, type = 'response')

mod.class_rf <- as.factor(ifelse(mod.probs_rf >= ponto_corte_rf, 1, 0))

medidas_rf <- metricas(mod_rf, mod.class_rf, mod.probs_rf)$medida

performace_rf <- metricas(mod_rf, mod.class_rf, mod.probs_rf)$performace


### Support Vector Machines

mod_svm <-  e1071::svm(f, data = Treino,
                       kernel = 'radial', 
                       type = 'C-classification',
                       cost = 10, gamma = 0.01, coef0 = 1,
                       probability = TRUE)

mod.class_svm <- predict(mod_svm, Teste)

mod.probs_svm <- attr(predict(mod_svm, Teste, probability = TRUE), 
                      'probabilities')[,2]

medidas_svm <- metricas(mod_svm, mod.class_svm, mod.probs_svm)$medida

performace_svm <- metricas(mod_svm, mod.class_svm, mod.probs_svm)$performace


### Medidas dos Modelos

medidas_tab <- data.frame(rbind(medidas_reg,
                                medidas_rf,
                                medidas_svm))
rownames(medidas_tab) <- c('Regressão',
                           'Random Forest',
                           'SVM')
t(medidas_tab)


### Curva ROC

plot(performace_reg, col = 'red', lwd = 2, 
     xlab = 'Taxa de falso positivo',
     ylab = 'Taxa de verdadeiro positivo')
plot(performace_rf, add = TRUE, col = 'blue', lwd = 2)
plot(performace_svm, add = TRUE, col = 'green', lwd = 2)
title(main='Curva ROC para os 3 modelos', font.main = 4)
plot_range <- range(0,0.5,0.5)
legend(0.5, plot_range[2], c('Regressão Logística', 'Random Forest', 'SVM'), 
       cex = 0.8, col = c('red','green','blue'), pch = 21:22, lty = 1:2)


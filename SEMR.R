library(lavaan)
library(lavaanPlot)
library(seminr)


#https://cran.r-project.org/web/packages/seminr/vignettes/SEMinR.html
#https://github.com/gastonstat/plspm
#https://cran.microsoft.com/snapshot/2015-07-29/web/packages/plspm/vignettes/plspm_introduction.pdf
#https://sem-in-r.github.io/seminr/#7_Moderation_analysis_(Chapter_8)
#https://sem-in-r.github.io/seminr/#1_Introduction

setwd("C:/Users/Kevin Palomino/OneDrive - Universidad del Norte/TESIS/BERTHA VILLALOBOS/SEM-Model")

data= read.csv('BD_PLS2.csv')


##### Escenario 1
myModel1 <- '
             # Measurement part 
             
              Apoyo_gobierno =~ AGC.1.2 + AGC.1.3 + AGC.1.4 + AGC.1.5 + AGC.1.6 + AGC.1.7 + AGC.1.8 + AGC.1.9
              Entorno =~ EC.3.1.1 +  EC.3.1.2 + EC.3.1.3 + EC.3.1.4 + EC.3.1.5 + EC.3.1.6
              Capital_humano =~ CHD.3.3.1 + CHD.3.3.6 + CHD.3.3.7
              Capital_social =~ CSD.5.1.1 + CSD.5.1.2 + CSD.5.1.3
              Innovacion =~ ID.4.1.4 + ID.4.1.5 + ID.4.1.6 + ID.4.1.7 + ID.4.1.8 + ID.4.1.9 + ID.4.1.11
              Medios_financieros =~ MFD.2.1.1 + MFD.2.1.2 + MFD.2.1.3 + MFD.2.1.4 + MFD.2.1.5 + MFD.2.1.6 + MFD.2.1.7 + MFD.2.1.8
              Desempeño_social =~ desempeno_social1F.2.6.3 + desempeno_social1F.2.6.4 + desempeno_social1F.2.6.5 + desempeno_social1F.2.6.6
              
              # Structural part
              
              Entorno ~ Apoyo_gobierno
              Innovacion ~ Capital_social +Capital_humano
              Desempeño_social ~ Entorno + Medios_financieros + Innovacion
              Desempeno_econo2 ~ Entorno + Medios_financieros + Innovacion
            '

fit <- sem(myModel1, 
           data = data, 
           estimator = "DWLS",
           meanstructure = TRUE, #intercept formula
           ordered = c("AGC.1.1","AGC.1.2","AGC.1.3","AGC.1.4","AGC.1.5", "AGC.1.6","AGC.1.7","AGC.1.8", "AGC.1.9",
                       "EC.3.1.1", "EC.3.1.2", "EC.3.1.3", "EC.3.1.4", "EC.3.1.5", "EC.3.1.6",
                       "CHD.3.3.1", "CHD.3.3.6", "CHD.3.3.7",
                       "CSD.5.1.1", "CSD.5.1.2", "CSD.5.1.4",
                       "ID.4.1.4", "ID.4.1.5","ID.4.1.6","ID.4.1.7", "ID.4.1.8","ID.4.1.9","ID.4.1.11" ,
                       "MFD.2.1.1","MFD.2.1.2","MFD.2.1.3","MFD.2.1.4","MFD.2.1.5","MFD.2.1.6","MFD.2.1.7","MFD.2.1.8",
                         "desempeno_social1F.2.6.3", "desempeno_social1F.2.6.4","desempeno_social1F.2.6.5","desempeno_social1F.2.6.6")
)

summary(fit, fit.measures = TRUE,standardized=TRUE)
fitmeasures(fit, "all")
inspect(fit,'r2')
PE <- parameterEstimates(fit, rsquare = TRUE)
PE$est[PE$op == "r2"]

##### Escenario 2
myModel2 <- '
             # Measurement part 
             
              Apoyo_gobierno =~ AGC.1.2 + AGC.1.3 + AGC.1.4 + AGC.1.5 + AGC.1.6 + AGC.1.7 + AGC.1.8 + AGC.1.9
              Entorno =~ EC.3.1.1 +  EC.3.1.2 + EC.3.1.3 + EC.3.1.4 + EC.3.1.5 + EC.3.1.6
              Capital_humano =~ CHD.3.3.1 + CHD.3.3.6 + CHD.3.3.7
              Capital_social =~ CSD.5.1.1 + CSD.5.1.2 + CSD.5.1.3
              Innovacion =~ ID.4.1.4 + ID.4.1.5 + ID.4.1.6 + ID.4.1.7 + ID.4.1.8 + ID.4.1.9 + ID.4.1.11
              Medios_financieros =~ MFD.2.1.1 + MFD.2.1.2 + MFD.2.1.3 + MFD.2.1.4 + MFD.2.1.5 + MFD.2.1.6 + MFD.2.1.7 + MFD.2.1.8
              Desempeño_social =~ desempeno_social1F.2.6.3 + desempeno_social1F.2.6.4 + desempeno_social1F.2.6.5 + desempeno_social1F.2.6.6
              
              # Structural part
              
              Entorno ~ Apoyo_gobierno
              Innovacion ~ Capital_social +Capital_humano
              Desempeño_social ~ Entorno + Medios_financieros + Innovacion
              Desempeno_econo2 ~ Entorno + Medios_financieros + Innovacion
            '

fit <- sem(myModel2, 
           data = data, 
           estimator = "ML",
           meanstructure = TRUE, #intercept formula

)

summary(fit, fit.measures = TRUE,standardized=TRUE)
fitmeasures(fit, "all")
inspect(fit,'r2')


#### Escenario 3

pls_data= data[c('AGC.1.2','AGC.1.3', 'AGC.1.4', 'AGC.1.5', 'AGC.1.6', 'AGC.1.7', 'AGC.1.8','AGC.1.9',
       'EC.3.1.1', 'EC.3.1.2', 'EC.3.1.3', 'EC.3.1.4', 'EC.3.1.5','EC.3.1.6',
       'CHD.3.3.1', 'CHD.3.3.6', 'CHD.3.3.7',
       'CSD.5.1.1', 'CSD.5.1.2', 'CSD.5.1.3', 
       'ID.4.1.4','ID.4.1.5', 'ID.4.1.6', 'ID.4.1.7', 'ID.4.1.8', 'ID.4.1.9', 'ID.4.1.11',
       'MFD.2.1.1', 'MFD.2.1.2','MFD.2.1.3', 'MFD.2.1.4', 'MFD.2.1.5', 'MFD.2.1.6', 'MFD.2.1.7','MFD.2.1.8',  
       'desempeno_social1F.2.6.3','desempeno_social1F.2.6.4', 'desempeno_social1F.2.6.5','desempeno_social1F.2.6.6',
       'Desempeno_econo1',  'Desempeno_econo2')]

colnames(pls_data)<-c('APO1','APO2', 'APO3', 'APO4', 'APO5', 'APO6', 'APO7','APO8',
                      'ENT1', 'ENT2', 'ENT3', 'ENT4', 'ENT5','ENT6',
                      'CAPH1', 'CAPH2', 'CAPH3',
                      'CAPS1', 'CAPS2', 'CAPS3', 
                      'INN1','INN2', 'INN3', 'INN4', 'INN5', 'INN6', 'INN7',
                      'MF1', 'MF2','MF3', 'MF4', 'MF5', 'MF6', 'MF7','MF8',  
                      'VS1','VS2', 'VS3','VS4',
                      'VE1',  'VE2')

measurements <- constructs(
  composite("Apoyo del gobierno",        multi_items("APO", 1:8)),
  composite("Entorno",  multi_items("ENT", 1:6)),
  composite("Capital Humano",        multi_items("CAPH", 1:3)),
  composite("Capital Social",        multi_items("CAPS", 1:3)),
  composite("Innovacion", multi_items("INN", 1:7)),
  composite("medios financieros", multi_items("MF", 1:8)),
  composite("Valor Social", multi_items("VS", 1:4)),
  composite("VE2", single_item("VE2"))
)


structural <- relationships(
  paths(from = c("Apoyo del gobierno"), to = c("Entorno")),
  paths(from = c("Entorno"), to = c("Valor Social","VE2")),
  paths(from = c("Capital Humano","Capital Social"), to = c("Innovacion")),
  paths(from = c("Innovacion"), to = c("Valor Social","VE2")),
  paths(from = c("medios financieros"), to = c("Valor Social","VE2"))
  )

modelPLS <-estimate_pls(data = pls_data,
             measurement_model = measurements,
             structural_model  = structural,
             inner_weights = path_weighting,
             missing = mean_replacement,
             missing_value =  "-99")

res <-summary(modelPLS)

res$paths
res$total_effects
res$total_indirect_effects
res$loadings
res$reliability
res$it_criteria
plot(res$reliability)

boot_model <- bootstrap_model(seminr_model = modelPLS,
                                 nboot = 1000,
                                 cores = NULL,
                                 seed = 123)



sum_resB <- summary(boot_model)



sum_resB$bootstrapped_paths
sum_resB$bootstrapped_loadings
sum_resB$bootstrapped_HTMT
sum_resB$bootstrapped_weights
sum_resB$bootstrapped_total_paths
plot(boot_model, title = "Bootstrapped Model")

predict_modelpls <- predict_pls(
  model = boot_model,
  technique = predict_DA,
  noFolds = 10,
  reps = 10)

sum_predict <- summary(predict_modelpls)

sum_predict


#ESCENARIO 4

myModel4 <- '
             # Measurement part 
             
              Apoyo_gobierno =~ AGC.1.2 + AGC.1.3 + AGC.1.4 + AGC.1.5 + AGC.1.6 + AGC.1.7 + AGC.1.8 + AGC.1.9
              Entorno =~ EC.3.1.1 +  EC.3.1.2 + EC.3.1.3 + EC.3.1.4 + EC.3.1.5 + EC.3.1.6
              Capital_humano =~ CHD.3.3.1 + CHD.3.3.6 + CHD.3.3.7
              Capital_social =~ CSD.5.1.1 + CSD.5.1.2 + CSD.5.1.3
              Innovacion =~ ID.4.1.4 + ID.4.1.5 + ID.4.1.6 + ID.4.1.7 + ID.4.1.8 + ID.4.1.9 + ID.4.1.11
              Medios_financieros =~ MFD.2.1.1 + MFD.2.1.2 + MFD.2.1.3 + MFD.2.1.4 + MFD.2.1.5 + MFD.2.1.6 + MFD.2.1.7 + MFD.2.1.8
              Desempeño_social2 =~ desempeno_social2F.2.7.1 + desempeno_social2F.2.7.2 + desempeno_social2F.2.7.3 + desempeno_social2F.2.7.4
              
              # Structural part
              
              Entorno ~ Apoyo_gobierno
              Innovacion ~ Capital_social +Capital_humano
              Desempeño_social2 ~ Entorno + Medios_financieros + Innovacion
              Desempeno_econo1 ~ Entorno + Medios_financieros + Innovacion
            '

fit <- sem(myModel4, 
           data = data, 
           estimator = "DWLS",
           meanstructure = TRUE, #intercept formula
           ordered = c("AGC.1.1","AGC.1.2","AGC.1.3","AGC.1.4","AGC.1.5", "AGC.1.6","AGC.1.7","AGC.1.8", "AGC.1.9",
                       "EC.3.1.1", "EC.3.1.2", "EC.3.1.3", "EC.3.1.4", "EC.3.1.5", "EC.3.1.6",
                       "CHD.3.3.1", "CHD.3.3.6", "CHD.3.3.7",
                       "CSD.5.1.1", "CSD.5.1.2", "CSD.5.1.4",
                       "ID.4.1.4", "ID.4.1.5","ID.4.1.6","ID.4.1.7", "ID.4.1.8","ID.4.1.9","ID.4.1.11" ,
                       "MFD.2.1.1","MFD.2.1.2","MFD.2.1.3","MFD.2.1.4","MFD.2.1.5","MFD.2.1.6","MFD.2.1.7","MFD.2.1.8",
                       "desempeno_social2F.2.7.1", "desempeno_social2F.2.7.2","desempeno_social2F.2.7.3","desempeno_social2F.2.7.4")
)


summary(fit, fit.measures = TRUE,standardized=TRUE)
fitmeasures(fit, "all")
inspect(fit,'r2')

##### Escenario 5
myModel5 <- '
             # Measurement part 
             
              Apoyo_gobierno =~ AGC.1.2 + AGC.1.3 + AGC.1.4 + AGC.1.5 + AGC.1.6 + AGC.1.7 + AGC.1.8 + AGC.1.9
              Entorno =~ EC.3.1.1 +  EC.3.1.2 + EC.3.1.3 + EC.3.1.4 + EC.3.1.5 + EC.3.1.6
              Capital_humano =~ CHD.3.3.1 + CHD.3.3.6 + CHD.3.3.7
              Capital_social =~ CSD.5.1.1 + CSD.5.1.2 + CSD.5.1.3
              Innovacion =~ ID.4.1.4 + ID.4.1.5 + ID.4.1.6 + ID.4.1.7 + ID.4.1.8 + ID.4.1.9 + ID.4.1.11
              Medios_financieros =~ MFD.2.1.1 + MFD.2.1.2 + MFD.2.1.3 + MFD.2.1.4 + MFD.2.1.5 + MFD.2.1.6 + MFD.2.1.7 + MFD.2.1.8
              Desempeño_social2 =~ desempeno_social2F.2.7.1 + desempeno_social2F.2.7.2 + desempeno_social2F.2.7.3 + desempeno_social2F.2.7.4
              
              # Structural part
              
              Entorno ~ Apoyo_gobierno
              Innovacion ~ Capital_social +Capital_humano
              Desempeño_social2 ~ Entorno + Medios_financieros + Innovacion
              Desempeno_econo1 ~ Entorno + Medios_financieros + Innovacion
            '

fit <- sem(myModel5, 
           data = data, 
           estimator = "ML",
           meanstructure = TRUE, #intercept formula
           
)

summary(fit, fit.measures = TRUE,standardized=TRUE)
fitmeasures(fit, "all")
inspect(fit,'r2')


#### escenario 6

pls_data= data[c('AGC.1.2','AGC.1.3', 'AGC.1.4', 'AGC.1.5', 'AGC.1.6', 'AGC.1.7', 'AGC.1.8','AGC.1.9',
                 'EC.3.1.1', 'EC.3.1.2', 'EC.3.1.3', 'EC.3.1.4', 'EC.3.1.5','EC.3.1.6',
                 'CHD.3.3.1', 'CHD.3.3.6', 'CHD.3.3.7',
                 'CSD.5.1.1', 'CSD.5.1.2', 'CSD.5.1.3', 
                 'ID.4.1.4','ID.4.1.5', 'ID.4.1.6', 'ID.4.1.7', 'ID.4.1.8', 'ID.4.1.9', 'ID.4.1.11',
                 'MFD.2.1.1', 'MFD.2.1.2','MFD.2.1.3', 'MFD.2.1.4', 'MFD.2.1.5', 'MFD.2.1.6', 'MFD.2.1.7','MFD.2.1.8',  
                 'desempeno_social2F.2.7.1','desempeno_social2F.2.7.2', 'desempeno_social2F.2.7.3','desempeno_social2F.2.7.4',
                 'Desempeno_econo1',  'Desempeno_econo2')]

colnames(pls_data)<-c('APO1','APO2', 'APO3', 'APO4', 'APO5', 'APO6', 'APO7','APO8',
                      'ENT1', 'ENT2', 'ENT3', 'ENT4', 'ENT5','ENT6',
                      'CAPH1', 'CAPH2', 'CAPH3',
                      'CAPS1', 'CAPS2', 'CAPS3', 
                      'INN1','INN2', 'INN3', 'INN4', 'INN5', 'INN6', 'INN7',
                      'MF1', 'MF2','MF3', 'MF4', 'MF5', 'MF6', 'MF7','MF8',  
                      'VS1','VS2', 'VS3','VS4',
                      'VE1',  'VE2')

measurements <- constructs(
  composite("Apoyo del gobierno",        multi_items("APO", 1:8)),
  composite("Entorno",  multi_items("ENT", 1:6)),
  composite("Capital Humano",        multi_items("CAPH", 1:3)),
  composite("Capital Social",        multi_items("CAPS", 1:3)),
  composite("Innovacion", multi_items("INN", 1:7)),
  composite("medios financieros", multi_items("MF", 1:8)),
  composite("Valor Social", multi_items("VS", 1:4)),
  composite("VE1", single_item("VE1"))
)


structural <- relationships(
  paths(from = c("Apoyo del gobierno"), to = c("Entorno")),
  paths(from = c("Entorno"), to = c("Valor Social","VE1")),
  paths(from = c("Capital Humano","Capital Social"), to = c("Innovacion")),
  paths(from = c("Innovacion"), to = c("Valor Social","VE1")),
  paths(from = c("medios financieros"), to = c("Valor Social","VE1"))
)

modelPLS <-estimate_pls(data = pls_data,
                        measurement_model = measurements,
                        structural_model  = structural,
                        inner_weights = path_weighting,
                        missing = mean_replacement,
                        missing_value =  "-99")

res <-summary(modelPLS)

res$paths
res$total_effects
res$total_indirect_effects
res$loadings
res$reliability
res$it_criteria
plot(res$reliability)

boot_model <- bootstrap_model(seminr_model = modelPLS,
                              nboot = 1000,
                              cores = NULL,
                              seed = 123)



sum_resB <- summary(boot_model)



sum_resB$bootstrapped_paths
sum_resB$bootstrapped_loadings
sum_resB$bootstrapped_HTMT
sum_resB$bootstrapped_weights
sum_resB$bootstrapped_total_paths
plot(boot_model, title = "Bootstrapped Model")



#ESCENARIO 7

myModel7 <- '
             # Measurement part 
             
              Apoyo_gobierno =~ AGC.1.2 + AGC.1.3 + AGC.1.4 + AGC.1.5 + AGC.1.6 + AGC.1.7 + AGC.1.8 + AGC.1.9
              Entorno =~ EC.3.1.1 +  EC.3.1.2 + EC.3.1.3 + EC.3.1.4 + EC.3.1.5 + EC.3.1.6
              Capital_humano =~ CHD.3.3.1 + CHD.3.3.6 + CHD.3.3.7
              Capital_social =~ CSD.5.1.1 + CSD.5.1.2 + CSD.5.1.3
              Innovacion =~ ID.4.1.4 + ID.4.1.5 + ID.4.1.6 + ID.4.1.7 + ID.4.1.8 + ID.4.1.9 + ID.4.1.11
              Medios_financieros =~ MFD.2.1.1 + MFD.2.1.2 + MFD.2.1.3 + MFD.2.1.4 + MFD.2.1.5 + MFD.2.1.6 + MFD.2.1.7 + MFD.2.1.8
              
              
              # Structural part
              
              Entorno ~ Apoyo_gobierno
              Innovacion ~ Capital_social +Capital_humano
              desempeno_social3 ~ Entorno + Medios_financieros + Innovacion
              Desempeno_econo3 ~ Entorno + Medios_financieros + Innovacion
            '

fit <- sem(myModel7, 
           data = data, 
           estimator = "DWLS",
           meanstructure = TRUE, #intercept formula
           ordered = c("AGC.1.1","AGC.1.2","AGC.1.3","AGC.1.4","AGC.1.5", "AGC.1.6","AGC.1.7","AGC.1.8", "AGC.1.9",
                       "EC.3.1.1", "EC.3.1.2", "EC.3.1.3", "EC.3.1.4", "EC.3.1.5", "EC.3.1.6",
                       "CHD.3.3.1", "CHD.3.3.6", "CHD.3.3.7",
                       "CSD.5.1.1", "CSD.5.1.2", "CSD.5.1.4",
                       "ID.4.1.4", "ID.4.1.5","ID.4.1.6","ID.4.1.7", "ID.4.1.8","ID.4.1.9","ID.4.1.11" ,
                       "MFD.2.1.1","MFD.2.1.2","MFD.2.1.3","MFD.2.1.4","MFD.2.1.5","MFD.2.1.6","MFD.2.1.7","MFD.2.1.8"
                       )
)


summary(fit, fit.measures = TRUE,standardized=TRUE)
fitmeasures(fit, "all")
inspect(fit,'r2')



#ESCENARIO 8

myModel8 <- '
             # Measurement part 
             
              Apoyo_gobierno =~ AGC.1.2 + AGC.1.3 + AGC.1.4 + AGC.1.5 + AGC.1.6 + AGC.1.7 + AGC.1.8 + AGC.1.9
              Entorno =~ EC.3.1.1 +  EC.3.1.2 + EC.3.1.3 + EC.3.1.4 + EC.3.1.5 + EC.3.1.6
              Capital_humano =~ CHD.3.3.1 + CHD.3.3.6 + CHD.3.3.7
              Capital_social =~ CSD.5.1.1 + CSD.5.1.2 + CSD.5.1.3
              Innovacion =~ ID.4.1.4 + ID.4.1.5 + ID.4.1.6 + ID.4.1.7 + ID.4.1.8 + ID.4.1.9 + ID.4.1.11
              Medios_financieros =~ MFD.2.1.1 + MFD.2.1.2 + MFD.2.1.3 + MFD.2.1.4 + MFD.2.1.5 + MFD.2.1.6 + MFD.2.1.7 + MFD.2.1.8
              
              
              # Structural part
              
              Entorno ~ Apoyo_gobierno
              Innovacion ~ Capital_social +Capital_humano
              desempeno_social3 ~ Entorno + Medios_financieros + Innovacion
              Desempeno_econo3 ~ Entorno + Medios_financieros + Innovacion
            '

fit <- sem(myModel8, 
           data = data, 
           estimator = "ML",
           meanstructure = TRUE, #intercept formula
           
)

summary(fit, fit.measures = TRUE,standardized=TRUE)
fitmeasures(fit, "all")
inspect(fit,'r2')


#### escenario 9

pls_data= data[c('AGC.1.2','AGC.1.3', 'AGC.1.4', 'AGC.1.5', 'AGC.1.6', 'AGC.1.7', 'AGC.1.8','AGC.1.9',
                 'EC.3.1.1', 'EC.3.1.2', 'EC.3.1.3', 'EC.3.1.4', 'EC.3.1.5','EC.3.1.6',
                 'CHD.3.3.1', 'CHD.3.3.6', 'CHD.3.3.7',
                 'CSD.5.1.1', 'CSD.5.1.2', 'CSD.5.1.3', 
                 'ID.4.1.4','ID.4.1.5', 'ID.4.1.6', 'ID.4.1.7', 'ID.4.1.8', 'ID.4.1.9', 'ID.4.1.11',
                 'MFD.2.1.1', 'MFD.2.1.2','MFD.2.1.3', 'MFD.2.1.4', 'MFD.2.1.5', 'MFD.2.1.6', 'MFD.2.1.7','MFD.2.1.8',  
                 'desempeno_social3',
                 'Desempeno_econo3')]

colnames(pls_data)<-c('APO1','APO2', 'APO3', 'APO4', 'APO5', 'APO6', 'APO7','APO8',
                      'ENT1', 'ENT2', 'ENT3', 'ENT4', 'ENT5','ENT6',
                      'CAPH1', 'CAPH2', 'CAPH3',
                      'CAPS1', 'CAPS2', 'CAPS3', 
                      'INN1','INN2', 'INN3', 'INN4', 'INN5', 'INN6', 'INN7',
                      'MF1', 'MF2','MF3', 'MF4', 'MF5', 'MF6', 'MF7','MF8',  
                      'VS3',
                      'VE3')

measurements <- constructs(
  composite("Apoyo del gobierno",        multi_items("APO", 1:8)),
  composite("Entorno",  multi_items("ENT", 1:6)),
  composite("Capital Humano",        multi_items("CAPH", 1:3)),
  composite("Capital Social",        multi_items("CAPS", 1:3)),
  composite("Innovacion", multi_items("INN", 1:7)),
  composite("medios financieros", multi_items("MF", 1:8)),
  composite("Valor Social", single_item("VS3")),
  composite("Valor economino", single_item("VE3"))
)


structural <- relationships(
  paths(from = c("Apoyo del gobierno"), to = c("Entorno")),
  paths(from = c("Entorno"), to = c("Valor Social","Valor economino")),
  paths(from = c("Capital Humano","Capital Social"), to = c("Innovacion")),
  paths(from = c("Innovacion"), to = c("Valor Social","Valor economino")),
  paths(from = c("medios financieros"), to = c("Valor Social","Valor economino"))
)

modelPLS <-estimate_pls(data = pls_data,
                        measurement_model = measurements,
                        structural_model  = structural,
                        inner_weights = path_weighting,
                        missing = mean_replacement,
                        missing_value =  "-99")

res <-summary(modelPLS)

res$paths
res$total_effects
res$total_indirect_effects
res$loadings
res$reliability
res$it_criteria
plot(res$reliability)

boot_model <- bootstrap_model(seminr_model = modelPLS,
                              nboot = 1000,
                              cores = NULL,
                              seed = 123)



sum_resB <- summary(boot_model)



sum_resB$bootstrapped_paths
sum_resB$bootstrapped_loadings
sum_resB$bootstrapped_HTMT
sum_resB$bootstrapped_weights
sum_resB$bootstrapped_total_paths
plot(boot_model, title = "Bootstrapped Model")




#############################################

labels = list(Apoyo_gobierno = "Apoyo del gobierno", 
              Entorno = "Entorno", 
              Capital_humano = "Capital humano",
              Capital_social ="Capital social",
              Innovacion= "Innovación",
              Medios_financieros="Medios financieros",
              Desempeño_social="Desempeño social",
              Desempeno_econo2="Desempeño economico"
              )

lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE,  stand = TRUE,stars = "latent")

library(polycor)
polychor()
#polyserial(data$AICH04,data$ArticulosP)
lavInspect(fit, "cov.lv")

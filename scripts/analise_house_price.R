library(tidyverse)
library(ggplot2)
# Leitura da base de dados
df <- read.csv('data/precos_imoveis.csv', stringsAsFactors = FALSE) %>% 
  select(-Id) %>% 
  mutate(MSSubClass = as.factor(MSSubClass))


# Estrutura do banco
dim(df)
colnames(df)

# Estudando as características das variáveis

table(sapply(df, class)) ## O banco tem 44 variáveis categóricas e 36 variáveis numéricas

##' A base de dados tem 3 categorias de assuntos os quais as variáveis abordam.
##' 1- vaiáveis que abordam a construção, ou seja, características físicas de onde está localizada
##'  a moradia Ex.: Overall Quality, Overall Condition, Foundation...
##' 2- variáveis referentes a residência em si, ou seja, propriedades da casa Ex.: área do lote,
##' tamanho total do porão...
##' 3- variáveis relacionadas a vizinhança Ex.: proximidade a lugares, rua pavimentada a zona onde
##' se localiza o lugar.

# Análise de dados faltantes


non_na <- sapply(df, function(x) mean(is.na(x))) %>% 
  sort(decreasing = TRUE) %>% 
  round(3)
non_na[non_na > 0.0]

##' Agora serão imputados valores para as variáveis que têm muitos NA's
##' Como as variáveis se repetem em categorias ordinais, iremos também transformar essas variáveis
quali <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
df <- df %>% 
  mutate(PoolQC = ifelse(is.na(PoolQC), 'None', PoolQC), # Qualidade Piscina
         MiscFeature = ifelse(is.na(MiscFeature), 'None', MiscFeature),
         Alley = ifelse(is.na(Alley), 'None', Alley),
         Fence = ifelse(is.na(Fence), 'None', Fence),
         FireplaceQu = ifelse(is.na(FireplaceQu), 'None', FireplaceQu),
         GarageType = ifelse(is.na(GarageType), 'None', GarageType),
         GarageFinish = ifelse(is.na(GarageFinish), 'None', GarageFinish),
         GarageQual = ifelse(is.na(GarageQual), 'None', GarageQual),
         GarageCond = ifelse(is.na(GarageCond), 'None', GarageCond))

df <- df %>% 
  mutate(PoolQC = as.integer(revalue(PoolQC, quali)), # Qualidade Piscina
         FireplaceQu = as.integer(revalue(FireplaceQu, quali)),
         GarageFinish = as.integer(revalue(GarageFinish, c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3))),
         GarageQual = as.integer(revalue(GarageQual, quali)),
         GarageCond = as.integer(revalue(GarageCond, quali)),
         OverallQual = )

##' Análise de correlação das categóricas. Foi escolhido utilizar o método da ANOVA para verifi-
##' car correlação entre categórica e continua. 
aov_models <- df[ , -which(sapply(df, is.numeric))] %>%
  map(~ anova(lm(df$SalePrice ~ .x)))
aov_pval <- sapply(aov_models, function(x) x$`Pr(>F)`[1])
length(aov_pval[aov_pval < 0.1])

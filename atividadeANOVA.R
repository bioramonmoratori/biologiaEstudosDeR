# ATIVIDADE ANOVA

dadosANOVA  <- read.table("dados_anova.csv", header=T, sep=",")
dadosANOVA

# Tratamento dos Dados

str(dadosANOVA) # 8 variaveis, sendo 6 do tipo int e 2 char

# Renomeando colunas
colnames(dadosANOVA) <- c("grupos", "var1", "var2", "var3", "var4", "var5", "var6", "sexo")

# Renomeando as categorias da variavel grupos
dadosANOVA$grupos[dadosANOVA$grupos == "Control"] <- "Controle"
dadosANOVA$grupos[dadosANOVA$grupos == "Diet"] <- "Dieta"
dadosANOVA$grupos[dadosANOVA$grupos == "DietEx"] <- "DietaExtra"

# Transformar as variáveis categóricas em fator
dadosANOVA$grupos <- as.factor(dadosANOVA$grupos)

# Precisamos atender aos pressupostos da ANOVA
# Variavel categorica com varias categorias
# Homogeneidade de variâncias
# Normalidade dos resíduos

# Teste de homogeneidade de variâncias
# Para ser homogeneo, precisa ter uma alta probabilidade das diferencas 
# serem ao acaso (p > 0,05)

# Vamos fazer um loop para identificar quais variaveis sao homogeneas com relacao a grupos
library(car)

# Vamos armazenar os valores em listas de chave valor
resultados_homogeneidade <- list()
resultados_residuos <- list()
resultados_shapiro <- list()
resultados_tukey <- list()

for (coluna in names(dadosANOVA)) {

    # Teste apenas para variaveis numericas
    if (is.numeric(dadosANOVA[[coluna]])) {
        
        # Realizando o teste de Levene
        resultado_levene <- leveneTest(dadosANOVA[[coluna]] ~ grupos, data = dadosANOVA)
        
        # Armazene o resultado na lista de resultados
        resultados_homogeneidade[[coluna]] <- resultado_levene$'Pr(>F)'[1]

        # Sigo para o teste de residuos apenas se a variavel for homogenea
        if(as.numeric(resultado_levene$'Pr(>F)'[1]) > 0.05) {

            # Fazendo ANOVA e armazenando os residuos
            resultados_residuos[[coluna]] <- aov(dadosANOVA[[coluna]] ~ grupos, data = dadosANOVA)$residuals

            resultados_shapiro[[coluna]] <- shapiro.test(resultados_residuos[[coluna]])

            if(as.numeric(resultados_shapiro[[coluna]]$p.value) > 0.05) {
                print(paste("A variavel", coluna, "passou pelos testes de shapiro e levene"))
                
                resultados_tukey[[coluna]] <- TukeyHSD(aov(dadosANOVA[[coluna]] ~ grupos, data = dadosANOVA))

            } 
            
        }
    }

}

# Visualize o gráfico QQ-Normal de todos os resíduos dos testes Homogeneos
par(mfrow=c(4, 2))  

for (coluna in names(resultados_residuos)) {
    qqnorm(resultados_residuos[[coluna]],  main = paste("Resíduos da variável", coluna))
    qqline(resultados_residuos[[coluna]],  main = paste("Resíduos da variável", coluna))
}

# VISUALIZE OS RESULTADOS DE TODAS AS VARIAVEIS NUMERICAS COM A VARIAVEL GRUPOS
resultados_homogeneidade
resultados_residuos
resultados_shapiro
resultados_tukey
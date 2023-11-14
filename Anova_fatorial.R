#####################
# ANOVA  fatorial   #
# Disciplina R.     #
# 31.10.2023        #
# Simone            #
#####################

read.table("dados_anova.txt", header=T)
dados<-read.table("dados_anova.txt", header=T)

View(dados)

attach(dados)

# Avaliação dos dados

par(mfrow = c(1,2)) # dividir a janela gráfica
boxplot(wl2 ~ group)
boxplot(wl2 ~ sex)


# Existe uma interação entre as variáveis grupo ("tratamento") e sexo?

# O teste mais recomendado nesta situação, onde nós temos duas variáveis preditoras categóricas (tratamento e sexo) e uma variável resposta contínua, é ANOVA fatorial. Porém, precisamos fazer os testes de pressupostos.

### Teste de pressupostos ###
#Teste de homogeneidade de variâncias:

library(car)

leveneTest(wl2 ~ group) #p = 0.6665
leveneTest(wl2 ~ sex) #p = 0.76

### Pressuposto da normalidade dos resíduos

anova_fatorial<-lm(wl2 ~ group*sex) 

# outra forma de fazer anova fatorial

anova_fatorial2<-aov(wl2 ~ group*sex) 

#Os resíduos:
ls(anova_fatorial)

qqnorm(anova_fatorial$residuals, pch=16)
qqline(anova_fatorial$residuals, lty=2)

shapiro.test(anova_fatorial$residuals) 

# Atendemos os pressupostos

## Olhando o resultado do teste

anova_fatorial
summary(anova_fatorial)

anova_fatorial2
summary(anova_fatorial2)

# Exploração gráfica


# Usando o pacote ggplot2:

library(Rmisc)

sum <-  summarySE(dados,
                measurevar="wl2",
                groupvars=c("group","sex"))

sum

library(ggplot2)

pd <- position_dodge(0.2)

ggplot(sum, aes(x = group, y = wl2, colour = sex))+
  geom_errorbar(aes(ymin = wl2 - se, ymax = wl2 + se), width = 0.2, size = 0.7, position = pd) +
  theme_bw() +
  geom_point(shape=15, size=4, position=pd) +
  geom_line(aes(group = sex))+
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("red", "blue"))
 

ggplot(sum, aes(x = sex, y = wl2, colour = group))+
  geom_errorbar(aes(ymin = wl2 - se, ymax = wl2 + se), width = 0.2, size = 0.7, position = pd) +
  theme_bw() +
  geom_point(shape=15, size=4, position=pd) +
  geom_line(aes(group = group))+
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("red", "blue", "green"))


# Usando o sciplot:

# install.packages("sciplot"). Caso não tenham é preciso instalar o pacote.

library(sciplot)

lineplot.CI(group, wl2, group = sex, data = dados, 
            cex = 2,
            xlab = "Tratamento", 
            ylab = "Perda de peso", 
            cex.lab = 1.5, x.leg = 1,
            col = c("blue","red"), pch = c(16,16))


lineplot.CI(sex, wl2, group = group, data = dados, 
            cex = 2,
            xlab = "Sexo", ylab = "Perda de peso", 
            cex.lab = 1.5, 
            x.leg = 1.8,
            y.leg = 12,
            col = c("blue","red", "green"), 
            pch = c(16,16,16), ylim = c(0, 12))

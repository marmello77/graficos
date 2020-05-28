#####################################################
#                                                   #
#    SCRIPT PARA DESENHAR GRAFICOS NO R             #
#                                                   #       
#####################################################

##### Sobrevivendo na Ciencia (blog)
##### Post: Qual grafico devo fazer?
##### URL: https://marcoarmello.wordpress.com
##### https://marcomellolab.wordpress.com
##### Autor: Marco Mello & Renata Muylaert
##### E-mail: marmello@gmail.com 
##### Titulo: Script para desenhar graficos no R
##### Publicado em 25 de maio de 2020 (versao em portgues).
##### Rodado no 4.0.0 (2020-04-24) -- "Arbor Day"
##### Fonte dos dados usados como exemplo: Gonçalves, Fernando, Ricardo S. Bovendorp, Gabrielle Beca, Carolina Bello, Raul Costa-Pereira, Renata L. Muylaert, Raisa R. Rodarte, et al. 2018. “ATLANTIC MAMMAL TRAITS: A Data Set of Morphological Traits of Mammals in the Atlantic Forest of South America.” Ecology 99 (2): 498–498. https://doi.org/10.1002/ecy.2106.

##### Aviso: Voce pode usar este script livremente para fins nao comerciais por seu proprio risco. Nao assumimos nenhuma responsabilidade pelo uso deste software, nao transmitimos licenca ou titulo sob nenhuma patente, direito autoral ou mascaramento de direito de trabalho sobre o produto. Reservamo-nos o direito de fazer alteracoes no software sem notificacao. Tambem nao declaramos ou garantimos que esse aplicativo seja adequado para o uso especificado sem testes ou modificacoes adicionais. Se esse script o ajudar a produzir algum trabalho academico (artigo, livro, capitulo, dissertacao etc.), por favor, reconheca os autores e cite a fonte.


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list= ls())
cat("\014")  

dados <- read.delim("dados.txt", sep = "\t", header = T, na = "NA")
head(dados)
summary(dados)
summary(dados$body_mass)
dados2 <- subset(dados, dados$body_mass < 30)


############### HISTOGRAMA ############### 


summary(dados2$body_mass)
length(dados2$body_mass)-(sum(is.na(dados2$body_mass)))

png(filename = "histograma.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
hist(dados2$body_mass,
     main = "Histograma", 
     xlab = "Massa corporal (g)", 
     ylab = "Frequência",
     col = "grey", border = "white",
     xlim=c(5,25), cex.axis = 1.5, cex.lab = 2, cex.main = 3)
par(mfrow=c(1,1))
dev.off()


############### BARRAS ############### 


sexo <- table(dados2$sex)

png(filename = "barras.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
barplot(sexo,
        main = "Diagrama de barras",
        xlab = "Sexo", names=c("Fêmeas","Machos"),
        ylab = "Frequência",
        col = "grey", border = "grey",
        cex = 1.5, cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex.sub = 1,
        ylim=c(0,1500))
par(mfrow=c(1,1))
dev.off()


############### BARRAS AGRUPADAS ############### 


idade <- table(dados2$sex, dados2$age)
idade

png(filename = "barrasagrupadas.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
barplot(idade,
        beside = T,
        main="Diagrama de barras agrupadas",
        xlab="Idade",
        ylab = "Frequência",
        col=c("black","grey"),
        names=c("Adultos","Juvenis"),
        legend = c("Fêmeas", "Machos"),
        cex = 1.5, cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex.sub = 1,
        ylim=c(0,1500))
par(mfrow=c(1,1))
dev.off()


############### BARRAS EMPILHADAS ############### 


idade2 <- table(dados2$sex, dados2$age)
idade2

idade3 <- apply(idade2, 2, function(x){x*100/sum(x,na.rm=T)})
idade3

png(filename = "barrasempilhadas.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
barplot(idade3,
        beside = F,
        main="Diagrama de barras empilhadas",
        xlab="Idade",
        ylab = "Proporção (%)",
        col=c("black","grey"),
        names=c("Adultos","Juvenis"),
        legend = c("Fêmeas", "Machos"),
        cex = 1.5, cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex.sub = 1)
par(mfrow=c(1,1))
dev.off()


############### PIZZA ############### 


png(filename = "pizza.png", res = 300, height= 3000, width= 3000)
par(mfrow=c(1,1),mar=c(1,1,5,1), bg = "white")
pie(sexo,
    main = "Diagrama de pizza",
    labels = c("fêmea","macho"),
    col = c("grey", "white"),
    cex = 2.5, cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex.sub = 1)
par(mfrow=c(1,1))
dev.off()


############### CAIXAS ############### 


png(filename = "boxplot.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
boxplot(dados2$body_mass~dados2$sex,
        main = "Diagrama de caixas", 
        xlab = "Sexo", names=c("Fêmeas","Machos"),
        ylab = "Massa corporal (g)",
        col = "grey", border = "black",
        cex.axis = 1.5, cex.lab = 2, cex.main = 3)
par(mfrow=c(1,1))
dev.off()


############### SCATTERPLOT ############### 


summary(dados2$forearm)
length(dados2$forearm)-(sum(is.na(dados2$forearm)))

png(filename = "scatterplot.png", res = 300, height= 3000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
plot(dados2$body_mass~dados2$forearm,
     main = "Diagrama de dispersão",
     xlab = "Comprimento do antebraço (mm)",
     ylab = "Massa corporal (g)",
     pch = 16, col = adjustcolor("black", alpha.f = 0.3),
     cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex = 2)
abline(lm(dados2$body_mass~dados2$forearm))
par(mfrow=c(1,1))
dev.off()


############### LINHA ############### 


anos <- tapply(dados2$body_mass, dados2$year, mean)
anos
class(anos)
plot(anos)
anos2 <- as.data.frame(anos)
anos2
anos2$year <- row.names(anos2)
anos3 <- subset(anos2, anos2$year < 2013)

anosN <- subset(dados2, dados2$year < 2013)
nrow(anosN)

png(filename = "linha.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
plot(anos3$anos ~ anos3$year, type = "l",
     main = "Diagrama de linha",
     xlab = "Anos",
     ylab = "Massa corporal (média em g)",
     cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex = 2)
par(mfrow=c(1,1))
dev.off()


############### PAREADO ############### 


library(ggplot2)

massa <- tapply(dados2$body_mass, dados2$year, mean)
massa2 <- aggregate(x=dados2$body_mass,
          by=list(dados2$year,dados2$sex),
          FUN=mean)
colnames(massa2) <- c("ano", "sexo", "massas")
head(massa2)

p1 <- ggplot(massa2, aes(x = sexo, y = massas)) +
    geom_line(aes(group = ano)) +
    geom_point() +
    ggtitle("Diagrama pareado") +
    xlab("Sexo") + ylab("Massa (g)") +
    theme(panel.background = element_rect(fill = NA),
          axis.line = element_line(size = 0.5, colour = "black"),
        plot.title = element_text(color="black", size=22, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="black", size=18, face="plain"),
        axis.title.y = element_text(color="black", size=18, face="plain"),
        axis.text = element_text(size = 16)) +
    scale_x_discrete(labels = c('Fêmeas','Machos'))
png(filename= "pareado.png", res= 300, height= 3000, width= 2500)
p1
dev.off()


############### LOGISTICO ############### 


library(lme4)
library(reshape2)

femeas<- read.delim("femeas.txt", header=T)
head(femeas)
nrow(femeas)
ncol(femeas)

summary(femeas$body_mass)
femeas2 <- subset(femeas, femeas$body_mass < 30)

femeas2$reproductive_stage
femeas2$reproductive_stage[is.na(femeas2$reproductive_stage)] <- "inactive"

femeas2$reproductive_stage2 <- ifelse(femeas2$reproductive_stage == "pregnant", 1, 0)
femeas2$reproductive_stage2

plot(femeas2$reproductive_stage2~femeas2$body_mass)


fit1 = glm(femeas2$reproductive_stage2~femeas2$body_mass, family=binomial)
summary(fit1)
res1 = anova(fit1, test="Chisq")
res1
capture.output(res1, file = "logistica.txt")

png(filename = "logistico.png", res = 300, height= 2500, width= 3000)
par(mfrow=c(1,1),mar=c(5,5,5,1), bg = "white")
plot(femeas2$reproductive_stage2~femeas2$body_mass,
     main = "Diagrama logístico",
     xlab = "Massa corporal (g)",
     ylab = "Estado reprodutivo",
     cex.axis = 1.5, cex.lab = 2, cex.main = 3, cex = 2,
     pch = 16, col = adjustcolor("black", alpha.f = 0.3),
     yaxt="n")
axis(2, at=c(0,0.5,1.0),labels=c(0, 0.5, 1.0), col.axis="black", las=2)
abline(h=0.5, col="black", lty=2)
curve (exp(fit1$coefficients[[1]]+fit1$coefficients[[2]]*x)/(1+exp(fit1$coefficients[[1]]+fit1$coefficients[[2]]*x)), add=T)
par(mfrow=c(1,1))
dev.off()


############### MAPA ############### 


library(ggplot2)
library(ggmap)
library(ggsn)
library(maps)
library(mapdata)
library(ggrepel)
library(brazilmaps)

pontos = read.delim("pontos.txt", na.strings = "NA")
head(pontos)

colnames(pontos) = c("long", "lat", "year")
head(pontos)

pontos$year = as.factor(pontos$year)
class(pontos$year)
write.csv(pontos, "pontos.csv", row.names=F)

map('world', region = "Brazil", fill = F)

area <-map_data("world", region="Brazil", zoom=1) 
head(area)

min(pontos$long)
max(pontos$long)
min(pontos$lat)
max(pontos$lat)

longs<-c(min(pontos$long)-0.01, max(pontos$long)+0.01)
lats<-c(min(pontos$lat)-0.01, max(pontos$lat)+0.01)

plot(pontos$long~pontos$lat)

g1 <- ggplot() + geom_polygon(data = area,
                              aes(x=long, y = lat, group = group),
                              fill = "lightgrey", color = "lightgrey") +
    #xlim(longs) +
    #ylim(lats) +
    coord_fixed(1.1) + 
    geom_polygon(data = area, 
                 aes(x = long, y = lat, group = group), 
                 color = "white", fill = NA, size = 0.04) +
    geom_point(data = pontos, aes(x = long, y = lat), 
               color = "red", 
               size = 2, 
               alpha = 0.6) +
    ggtitle("Mapa") + 
    labs(x="Longitude", y = "Latitude") + 
    theme(text = element_text(size=14), 
          plot.title = element_text(size=20, hjust=0.5),
          axis.text.x = element_text(size = 10, angle=0, hjust=1),
          axis.text.y = element_text(size = 10, angle=0, vjust=1),
          axis.title.x = element_text(size = 12, angle=0),
          axis.title.y = element_text(size = 12, angle=90))

png(filename= "mapa.png", res= 300,  height= 20, width=16, unit="cm")
g1 +
    ggsn::scalebar(area, dist = 500,
                   location = "bottomright", 
                   transform = TRUE,
                   dist_unit = "km", 
                   st.dist = 0.03, 
                   st.size = 2, 
                   model = 'WGS84') +
    ggsn::north(area, scale = .1)
dev.off()


############### GRAFO ############### 


library(bipartite)

grafo <- read.delim("grafo.txt", 
                    row.names=1, 
                    header=TRUE)
grafo

png(filename = "grafo1.png", res = 300, height= 2000, width= 3000)
par(mfrow=c(1,1),mar=c(1,1,5,1), bg = "white")
plotweb(grafo,method = "cca", 
        text.rot = 90, empty = TRUE, labsize = .70, ybig = 0.9, arrow ="no",
        col.interaction = adjustcolor("grey", alpha.f = 0.2), 
        bor.col.interaction = adjustcolor("grey", alpha.f = 0.2),
        col.high = "black", 
        bor.col.high="black", 
        col.low="grey50", 
        bor.col.low="grey50", 
        high.lablength = NULL, low.lablength = NULL, 
        sequence=NULL, low.abun = NULL, high.abun = NULL, 
        low.abun.col = NULL, bor.low.abun.col = NULL, 
        high.abun.col = NULL, bor.high.abun.col= NULL, 
        text.high.col = "black",text.low.col = "black", 
        adj.high=NULL, adj.low=NULL, plot.axes = FALSE, 
        low.y=0.6, high.y=1.0, add=FALSE, 
        y.lim=NULL, x.lim=NULL, low.plot=TRUE)
title("Grafo",cex.main=3,col.main="black")
par(mfrow=c(1,1))
dev.off()

#####

library(igraph)

grafo2 <- graph_from_incidence_matrix(grafo, 
                                      directed = FALSE,
                                      weighted = T, 
                                      add.names = NULL)
grafo2
E(grafo2)
V(grafo2)

vec1 <- rep(0,length=98)
vec2 <- rep(1, length=39)
types <- c(vec1,vec2)
colors <- ifelse(types == 0, "black", "grey50")
colors

E(grafo2)$width = scale(E(grafo2)$weight)

png(filename = "grafo2.png", res = 300, height= 3000, width= 3000)
par(mfrow=c(1,1),mar=c(1,1,5,1), bg = "white")
plot(grafo2,
     vertex.color = colors, 
     vertex.frame.color= colors, 
     vertex.size=6,
     vertex.label.cex=.4,
     vertex.label = V(grafo2)$names,
     vertex.label.color = "white",
     edge.color = adjustcolor("black", alpha.f = .3), 
     edge.width = E(grafo2)$width*2,
     edge.curved = 0.3,
     layout=layout_in_circle)
title("Grafo",cex.main=3,col.main="black")
par(mfrow=c(1,1))
dev.off()


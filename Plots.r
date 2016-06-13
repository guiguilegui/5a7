library(xlsx)
library(ggplot2)
library(glmnet)
library(bild)
library(lme4)
library(plotly)
require(scales)

#install.packages("bild")
#install.packages("glmnet")

Df.Resultat = read.xlsx2("C:\\Users\\Guillaume\\Documents\\FB\\5à7.xlsx", sheetName = "Sheet1",stringsAsFactors =FALSE)
Df.Resultat$Reel = as.numeric(Df.Resultat$Reel)
Df.Resultat$time = (as.numeric(Df.Resultat$Edition)-8)
#Df.Resultat$Reel = factor(Df.Resultat$Reel,levels=c(1,0))

Df.Resultat$counts = 1
Df.Resultat$oui = Df.Resultat$Reponse=="Oui"
Df.Resultat$pasrepondu = Df.Resultat$Reponse=="Pas répondu"
Df.Resultat$peutetre = Df.Resultat$Reponse=="Peut-être"
Df.Resultat$non = Df.Resultat$Reponse=="Non"

#Df.Resultat$Reponse = factor(Df.Resultat$Reponse,levels=c("Oui", "Peut-être", "Pas répondu", "Non", "Pas invité"))


#Df.Resultat2 = subset(Df.Resultat, Reponse!="Pas invité")

table(Df.Resultat2$id)
Df.Resultat2 = subset(Df.Resultat, !(Nom %in% c("Caroline","Tina","Isabelle")))

#unique(Df.Resultat$time)

Df.Resultat2$id = as.numeric(factor(Df.Resultat2$Nom))
Df.Resultat2 = Df.Resultat2[with(Df.Resultat2, order(id, time)), ]

b = bild(Reel ~ 1, data = Df.Resultat2 ,start = NULL, dependence = 'indR')
summary(b)

b = bild(Reel ~ peutetre + pasrepondu + non, data = Df.Resultat2 ,start = NULL, dependence = 'indR')

b@coefficients
b@ind.probability
b@Fitted.av
b@model.matrix
b@y.av

b = bild(Reel ~ oui+non, data = Df.Resultat2 ,start = NULL, dependence = 'indR')
summary(b)
plot(b,which=5)

glmm2 = glmer(Reel ~ oui + non + (1|Nom), data=Df.Resultat2 , family="binomial", nAGQ = 9)
summary(glmm2)
ranef(glmm2)

vecteur.noms = unique(Df.Resultat2$Nom)
df.glmm2 = data.frame(
	Nom = rep(sort(vecteur.noms), each = 3),
	intercept = rep(unlist(ranef(glmm2)),each = 3),
	oui = rep(c(TRUE,FALSE,FALSE),length(vecteur.noms)),
	non = rep(c(FALSE,FALSE,TRUE),length(vecteur.noms)),
	reponse = rep(c("Oui","Peut-Être /\nPas de réponse","Non"),length(vecteur.noms))
)

df.glmm2$fit = predict(glmm2, df.glmm2, type = "response")
df.glmm2$Nom <- factor(df.glmm2$Nom, levels = unique(df.glmm2$Nom[order(df.glmm2$intercept)]))
df.glmm2$reponse <- factor(df.glmm2$reponse, levels = c("Oui","Peut-Être /\nPas de réponse","Non"))


g3 = ggplot(data = df.glmm2)+
geom_point(aes(x = Nom, y = fit, color=reponse))+
scale_y_continuous(labels=percent)+
scale_color_manual(values = c("green","orange", "red"), name = "Réponse sur FB")+
theme_bw()+
xlab("")+
ylab("Probabilité de présence")+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

p3 = ggplotly(g3)
plotly_POST(p3, filename = "PredictionSelonReponse")

+
geom_line(aes(x = as.numeric(Edition), y = fit, color=Nom))

df.new = data.frame(Nom = unique(Df.Resultat2$Nom),oui = TRUE)
df.new = data.frame(Nom = unique(Df.Resultat2$Nom),oui = FALSE)

df.new$fit = predict(glmm,df.new,type = "response")

df.new[with(df.new, order(fit)), ]


glmm = glmer(Reel ~ 0 + (1|Nom) + poly(as.numeric(Edition),1), data = Df.Resultat2 , family="binomial", nAGQ = 1)

summary(glmm)
ranef(glmm)


mySumm2 <- function(.) {
   c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}

mySumm3 <- function(.) {
   predict(.,Df.Resultat2,type = "response")
}


vecteur.noms = unique(Df.Resultat2$Nom)
derniere.edition = max(as.numeric(Df.Resultat2$Edition))
df.new = data.frame(Edition = rep((derniere.edition+1):(derniere.edition+3), each = length(vecteur.noms)), Nom = 
rep(vecteur.noms,3))

mySumm4 <- function(.) {
   predict(.,df.new, type = "response")
}
Nsims = 1000
boo01 <- bootMer(glmm, mySumm3, nsim = Nsims, ncpus = 6, use.u = TRUE)
boo.new <- bootMer(glmm, mySumm4, nsim = Nsims, ncpus = 6, use.u = TRUE)


Df.Resultat.boot = data.frame(
	FittedBoot = as.vector(as.matrix(data.frame(boo01))),
	Nom = rep(Df.Resultat2$Nom, each = Nsims),
	Edition = rep(Df.Resultat2$Edition, each = Nsims),
	Boot = rep(1:Nsims,nrow(Df.Resultat2))
)



Df.Resultat2$fit = predict(glmm,Df.Resultat2,type = "response")
Df.Resultat2$LConf = apply(as.matrix(as.data.frame(boo01)),2,quantile,probs=0.1)
Df.Resultat2$HConf = apply(as.matrix(as.data.frame(boo01)),2,quantile,probs=0.9)

df.new$fit = predict(glmm, df.new, type = "response")
df.new$LConf = apply(as.matrix(as.data.frame(boo.new)), 2, quantile, probs=0.1)
df.new$HConf = apply(as.matrix(as.data.frame(boo.new)), 2, quantile, probs=0.9)

dfpredict = rbind(Df.Resultat2[,c("fit","LConf","HConf","Edition","Nom")],df.new[,c("fit","LConf","HConf","Edition","Nom")])

g =
ggplot(data = Df.Resultat2)+ #stat_summary(fun.y = sum)+
geom_jitter(aes(x = as.numeric(Edition), y = Reel, color=Nom),height = 0.15, width = 0.25)+
geom_smooth(aes(x = as.numeric(Edition), y = Reel, color=Nom), se=FALSE, method="gam", method.args = list(family = "binomial"))

g =
ggplot(data = Df.Resultat2)+ #stat_summary(fun.y = sum)+
geom_jitter(aes(x = as.numeric(Edition), y = Reel, color=Nom),height = 0.15, width = 0.25)+
geom_line(aes(x = as.numeric(Edition), y = fit, color=Nom))

g =
ggplot(data = Df.Resultat2)+ #stat_summary(fun.y = sum)+
geom_point(aes(x = as.numeric(Edition), y = Reel, color=Nom))+
geom_smooth(aes(x = as.numeric(Edition), y = Reel, color=Nom), se=FALSE, method="gam", method.args = list(family = "binomial"))+
scale_x_continuous(breaks = seq(9, 17, by = 1))+
scale_y_continuous(breaks = 0:1,labels = c("Non","Oui"))+
theme_bw()+
xlab("Édition")+
ylab("Présence")+
facet_wrap(~Nom)+
theme(legend.position="none")

g2 = ggplot()+ #stat_summary(fun.y = sum)+
geom_ribbon(data = dfpredict, aes(x = as.numeric(Edition), ymin = LConf, ymax = HConf),fill = "grey80")+
geom_point(data = Df.Resultat2, aes(x = as.numeric(Edition), y = Reel, color=Reponse))+
geom_line(data = Df.Resultat2, aes(x = as.numeric(Edition), y = fit))+
geom_line(data = subset(dfpredict,as.numeric(Edition)>=17), aes(x = as.numeric(Edition), y = fit),linetype = "dashed")+
scale_x_continuous(breaks = seq(9, 20, by = 1))+
scale_y_continuous(breaks = 0:1, labels = c("Non","Oui"))+
scale_color_manual(values = c("red","green", "purple","orange"), name = "Réponse sur FB")+
theme_bw()+
xlab("Édition")+
ylab("Présence")+
facet_wrap(~Nom)+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

p = plotly_build(g2)

for(i in 1:136){
if(substr(p$data[[i]]$name,1,1)=="("){
p$data[[i]]$showlegend=FALSE
}
}

#p2 = ggplotly(g2)

Sys.setenv("plotly_username"="thundergui")
Sys.setenv("plotly_api_key"="hp7t501b1z")

plotly_POST(p, filename = "Presence5a7chaque")


+
scale_alpha_discrete(range = c(1, 0.3),label = c('Présent','Absent'),name = '')+
scale_fill_brewer(type= "qual",palette = 2)+
scale_y_continuous(minor_breaks = seq(0, 25, by = 1))+
theme(
rect = element_blank(),
panel.grid.major.y = element_line(colour = "grey70", linetype = 'dotdash'),
panel.grid.minor.y = element_line(colour = "grey90", linetype = 'dotdash'),
axis.title.y = element_blank()
)+
labs(title = "Présence au 5 à 7")+
xlab("Édition")
ggplotly(g)



 opt_all_columns = TRUE
 getSummarizedWeather("yul",c("2016-01-01","2016-04-01"),opt_all_columns=TRUE)
g =
ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
geom_bar(aes(x = as.factor(Edition),alpha = Reel, fill=Reponse, y = ..count..),stat = 'count')+
scale_alpha_discrete(range = c(1, 0.3),label = c('Présent','Absent'),name = '')+
scale_fill_brewer(type= "qual",palette = 2)+
scale_y_continuous(minor_breaks = seq(0, 25, by = 1))+
theme(
rect = element_blank(),
panel.grid.major.y = element_line(colour = "grey70", linetype = 'dotdash'),
panel.grid.minor.y = element_line(colour = "grey90", linetype = 'dotdash'),
axis.title.y = element_blank()
)+
labs(title = "Présence au 5 à 7")+
xlab("Édition")
ggplotly(g)




fit = glmnet(model.matrix(~Reponse,Df.Resultat)[,-1],as.numeric(as.character(Df.Resultat$Reel)),family="binomial")
plot(fit,label=TRUE)
cv = cv.glmnet(model.matrix(~Reponse,Df.Resultat)[,-1],as.numeric(as.character(Df.Resultat$Reel)),family="binomial")
coef(cv,s = "lambda.min")
coef(fit)
data.frame(predict(cv,newx=model.matrix(~Reponse,Df.Resultat)[,-1],type = "response",s = "lambda.min"),predict(cv,newx=model.matrix(~Reponse,Df.Resultat)[,-1],type = "response",s = "lambda.1se"),Df.Resultat$Reel,Df.Resultat$Reponse)
plot(cv)

minuslogl

ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
geom_bar(aes(x = as.factor(Nom),alpha = Reel, fill=Reponse, y = ..count..),stat = 'count')+
scale_alpha_discrete(range = c(1, 0.4))+
theme(axis.text.x = element_text(angle=90))

ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
geom_area(aes(x = Edition,alpha = Reel, y = ..count..),stat = 'count')+
scale_alpha_discrete(range = c(1, 0.4))

ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
geom_area(aes(x = Edition,fill=Reponse, y = ..count..),stat = 'count')+
scale_alpha_discrete(range = c(1, 0.4))



ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
geom_area(aes(x=Edition, fill=Reponse, y = ..count.., alpha = Reel),stat = 'count')


ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
geom_ribbon(aes(x=Edition, fill=Reponse),stat = 'count')

ggplot(data = Df.Resultat) + #stat_summary(fun.y = sum)+
stat_sum(geom = "area",aes(x=Edition, y= ..n..,weight = Reel),position = 'stack',fill="black")
ggplot(data = Df.Resultat)+geom_point(aes(x=Edition, y=Reponse))

##################################
############Participation#########

Df.Lieu = read.xlsx2("C:\\Users\\Guillaume\\Documents\\FB\\5à7.xlsx", sheetName = "Lieux",stringsAsFactors =FALSE)
Df.Lieu$Date = as.Date(as.numeric(Df.Lieu$Date), origin = "1899-12-30")
Df.Lieu$Participation = as.numeric(Df.Lieu$Participation)
Df.Lieu$Édition = as.numeric(Df.Lieu$Édition)

glm = glm(Participation ~ poly(Édition,1) ,data=Df.Lieu, family = poisson)
summary(glm)
Df.Lieu$fit = predict(glm,type = "response")

Df.New = data.frame(Édition = max(Df.Lieu$Édition):(max(Df.Lieu$Édition)+3))
Df.New$fit = predict(glm,Df.New, type = "response")
Df.New$PredL = qpois(0.1, Df.New$fit)
Df.New$PredU = qpois(0.9, Df.New$fit)
Sys.setlocale('LC_TIME', "French_Canada")

g = ggplot(data = Df.Lieu)+ #stat_summary(fun.y = sum)+
geom_ribbon(data = Df.New, aes(x = Édition, ymin = PredL, ymax = PredU,fill="80% prediction\ninterval"))+
geom_point(aes(x = Édition, y = Participation , color= Lieu,text = paste("Commentaire:", Commentaire,"<br>Date:",format(Date, "%d %B %Y"))))+
geom_line(aes(x = Édition, y = fit),size=1)+
geom_line(data = Df.New , aes(x = Édition, y = fit),size=1, linetype = "dashed")+
scale_x_continuous(breaks = seq(min(Df.Lieu$Édition), max(Df.Lieu$Édition)+3, by = 1))+
scale_fill_manual(values ="grey80")+
theme_bw()
p = ggplotly(g)

Sys.setenv("plotly_username"="thundergui")
Sys.setenv("plotly_api_key"="hp7t501b1z")

plotly_POST(p, filename = "Presence5a7total")


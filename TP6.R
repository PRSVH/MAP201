#---- Q.2
ME = 23; SE = 56; MNE = 67; SNE = 136
M = ME+MNE
S = SE+SNE
E = ME+SE
NE = MNE+SNE
n = M+S

#---- Q.3
D2 = n*( -1+ ME^2/(M*E) + SE^2/(S*E) + MNE^2/(M*NE) + SNE^2/(S*NE) )
D2  # 0.396

#---- Q.4
# Le degré de liberté est égal à (2-1)*(2-1) = 1
1 - pchisq(D2, df = 1)
# p-valeur = 0.53 > 0.05. On ne rejette pas H0
# Pas de dépendance significative entre risque et maladie

#---- Q.5
# a)
ta = rbind(c(ME,SE),c(MNE,SNE))
rownames(ta) = c("expose","non expose")  # noms des lignes
colnames(ta) = c("malades","sains")  # noms des colonnes
ta  # table de contingence
# b) 
chisq.test(ta, correct = F)
# On retrouve la valeur de la statistique D2, le degré de liberté et la p-valeur
chisq.test(ta)
# p-valeur = 0.62. La valeur est assez différente du calcul sans correction de continuité, 
# mais la décision du test est la même dans les deux cas

#---- Exercice 2 ----
#---- Q.1
TI = read.table("data/titanic.csv", header = TRUE, sep=";")
P = TI$pclass
S = TI$survived
G = TI$gender
A = TI$age

#---- Q.2
table(G,S)  # table de contingence
# 388 passagers étaient des femmes. 292 femmes ont survécu. 135 hommes ont survécu

#---- Q.3
prop.table(table(G,S), 1)  # parmi les femmes, 75% ont survécu
prop.table(table(G,S), 2)  # parmi les survivants, 68% étaient des femmes

#--- Q.4
barplot(prop.table(table(G,S),1))
barplot(prop.table(table(G,S),2))
# le deuxième graphe est préférable car les deux barres valent 1 au total et donc sont plus facilement comparables
# Il semble y a voir un lien entre le genre et la survie

#---- Q.5
# H0 : genre et survie sont indépendants
# H1 : genre et survie ne sont pas indépendants
chisq.test(table(G,S))
# p-valeur < 10^-16 < 0.05. On rejette (fortement) H0.
# Les variables genre et survie ne sont pas indépendantes

#---- Q.6
A10 = A<10
table(A10,S)  # table de contingence enfant et survie
# 82 passagers avaient moins de 10 ans dont 50 ont survécu
chisq.test(table(A10,S))
# p-valeur = 0.0002 < 0.05. On rejette H0
# Les chances de survie n'étaient pas les mêmes pour les enfants et les autres.

#---- Q.7
A21 = A<21
table(A21,S)  # table de contingence enfant et survie
# 249 passagers avaient moins de 21 ans dont 114 ont survécu
chisq.test(table(A21,S))
# p-valeur = 0.08 > 0.05. On ne rejette pas H0
# La différence dans les chances de survie n'est pas significative

#---- Q.8
table(P,S)  # table de contingence entre classe et survie
# 284 passagers étaient en 1ere dont 181 ont survécu.
# 370 passagers étaient en 3e dont 131 ont survécu
chisq.test(table(P,S))
# p-valeur < 10^-16 < 0.05. On rejette H0.
# Les chances de survie dépendent de la classe du passager


#---- Exercice 3 ----
#---- Q.1
BO = read.table("data/bosson.csv",header = TRUE,sep = ";")
C = BO$country
G = BO$gender

#---- Q.2
R = BO$risk
R  # risque à 6 niveaux (0 à 5)
table(G,R)  # il y a des cases dont les effectifs sont < 5. Il faut faire un regroupement de modalités

#---- Q.3
R[R>=2] = 2
R  # risque à 3 niveaux (0 à 2)
table(G,R)  # le cadre de validité du chideux est maintenant respecté

#---- Q.4
prop.table(table(G,R),1)
# plus de risque 0 parmi les femmes (31%) que parmi les hommes (15%)
# plus de risque 2 parmi les hommes (50%) que parmi les femmes (16%)
barplot(prop.table(table(G,R),2))
# Les femmes sont de moins en moins représentées à mesure que le niveau de risque augmente
chisq.test(table(G,R))
# p-valeur = 5*10-5 < 0.05. On rejette H0
# Il y a une dépendance entre le genre et le nombre de facteurs de risque

#---- Q.5
prop.table(table(C,R),1)
# plus de risque 0 parmi les Vietnamiens (21%) que parmi les Francais (17%)
# plus de risque 2 parmi les Francais (54%) que parmi les Vietnamiens (32%)
barplot(prop.table(table(C,R),2))
# Les Francais semblent beaucoup plus représentés parmi les risque 2+.
chisq.test(table(C,R))
# p-valeur = 0.005 < 0.05. On rejette H0
# Il y a une dépendance entre le pays et le nombre de facteurs de risque

#---- Q.6
prop.table(table(C,G),1)
# plus de femmes parmi les Vietnamiens (33%) que parmi les Francais (15%)
barplot(prop.table(table(C,G),2))
# La répartition du genre semble dépendre du pays
chisq.test(table(C,G))
# p-valeur = 0.005 < 0.05. On rejette H0
# Il y a une dépendance entre le pays et le genre

#---- Q.7
# Les trois variables sont dépendantes. En aucun cas, ceux ne sont des liens de causalité.


#---- Exercice bonus ----
distrib.et.test <- function(X,Y,alpha=0.05)
  # Ici, on utilise les fonctions natives pour les distributions conditionnelles, mais on implémente le calcul de la statistique D2
{
  tab = table(X,Y)
  n = sum(tab)
  q = nrow(tab)
  r = ncol(tab)
  sum = 0
  for (i in 1:q)
  {
    for (j in 1:r)
    {
      sum = sum + tab[i,j]^2/(sum(tab[i,])*sum(tab[,j]))
    }
  }
  D2 = n*(-1+sum) # pour vérifier, on peut comparer la valeur D2 avec la valeur renvoyée par chisq.test(tab, correct=F)$stat
  z = qchisq(1-alpha, df = (q-1)*(r-1))
  decision = ifelse(D2>z, "Le test rejette l'indépendance", "Le test ne rejette pas l'indépendance")
  return(list(distrib.X.sachant.Y = prop.table(table(X,Y), 2), distrib.Y.sachant.X = prop.table(table(X,Y), 1), decision.test = decision))
}

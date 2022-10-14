#Exercice 6 FROM 2) to 6)_TP1/Ivanina Ivanova, Preshtibye Raggoo, MIN-INT, 26/03/21
TBosson = read.table(file="data/bosson.csv", header=TRUE, sep=";")
TBosson[1:6,] #Affiche les 6 premières lignes du tableau TBosson
C = TBosson[, "country"] #Vector of type string/character
C = TBosson$country #Retourne la colonne country ; Type character
C
G = TBosson[, "gender"]#Retourne la colonne Gender;Type character
G
A = TBosson[,"aneurysm"]#Retourne la colonne aneurysm;Type Int
A
R = TBosson[,"risk"]#Retourne la colonne risk; Type int
R
TBosson[120:123,] #Affiche les lignes 120 à 123
TBosson[c(67,83,101), c(1,3)]#Affiche les colonnes 1 et 3 des lignes 67, 83, 101
TBosson[C=="Vietnam", ]
TBosson[G=="M",5]
table(C)#France = 99 ; Vietnam=110
prop.table(table(C))#France = 0.4736842; Vietnam = 0.5263158
table(G)#F=51;M=158

prop.table(table(G)) #F=0.2440191, M=0.7559809
barplot(table(G))#Representation graphique du tableau G
barplot(table(C))#Representation graphique du tableau C
table(C,G) #Homme en France = 84
prop.table(table(C,G)) #Proportion d'hommes en France = 0.40191388
prop.table(table(C,G),1) # Proportion de patients vietnamienne qui sont femme = 0.3272727
prop.table(table(C,G),2) # proportion de femmes sont vietnamiennes =0.7058824 
barplot(prop.table(table(C,G)),beside =TRUE)
barplot(prop.table(table(C,G),1),beside =TRUE)
barplot(prop.table(table(C,G),2),beside =TRUE)


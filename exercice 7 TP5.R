#Preshtibye Raggoo, Ivanina Ivanova, TP5 
#01/05/2021
#MIN-INTER

#Exercice 7. On utilise le jeu de donnees cardiaque.csv.
cardiaque = read.table(file="~/MAP201/TPMAP201/data/cardiaque.csv",header=TRUE,sep=";")
SHA= cardiaque[cardiaque$activiteBinaire=="1",2]
SHA

#From code provided we can use:
data = read.table(file="~/MAP201/TPMAP201/data/cardiaque.csv", header=TRUE, sep=";")
Act = data$activiteBinaire
S = data[,"systolique"]
S
Tab = data$tabacBinaire
SHA = S[Act==1]
SHA
SA = S[Act==0]
SA
SF = S[Tab==1]
SF
SNF = S[Tab==0]
SNF
summary(S)
by(S,Act,summary)
#Act: 1
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#106.0   126.0   132.0   138.2   146.0   214.0 

#Act: 0
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#101.0   124.0   134.0   138.4   148.0   218.0 
by(S,Tab,summary)
#Tab: 0
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#108.0   122.0   130.0   133.6   139.0   206.0 
#------------------------------------------------------------------ 
#  Tab: 1
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#101.0   126.0   136.0   139.8   152.0   218.0 
boxplot(S~Act)
#On affiche le boxplots des 2 distribution pour faire une comparaison des deux distributions
#1)La distribution de la pression systolique pour les non hyperactifs a plus de valeurs aberrantes
#  que la distribution de la pression systolique pour les hyperactifs.
#2)The distribution for la pression systolique pour les non hyperactifs has a greater pool of values 
#  than the distribution for la pression systolique pour les hyperactifs.
#3) However they have approximately the same mean value of around 138.
#4) They have have approximately the same lower quartile. However The distribution for la pression 
#   systolique pour les non hyperactifs has a higher upper quartile.
#5) Both distributions are negatively skewed,i.e their respective means are found at the right of their medians.

boxplot(S~Tab)
#1)the distribution for the smokers is greater as we can see its boxplot stretches longer 
#  than that of the smokers
#2)The distribution for smokers has more outliers.
#  However the distribution for non smokers has only 6 outliers but is spread over a 
#  larger data range.
#3)The distribution for smokers has a higher median than the distribution for non-smokers
#4)Both distributions are negatively skewed,i.e their respective means are found at the right of their medians.

#H0:la pression systolique moyenne hyperactifs ??? la pression systolique moyenne non hyperactifs
#H1:la pression systolique moyenne hyperactifs > la pression systolique moyenne non hyperactifs


t.test(SHA, SA, alternative="greater")
#t = -0.071037, df = 166.37, p-value = 0.5283
#The p-value is greater than 0.05. We do not reject the null hypothesis.
#la pression systolique moyenne hyperactifs peut etre inferieure ou egale à la pression systolique moyenne non hyperactifs



#H0:la pression systolique moyenne fumeurs ??? la pression systolique moyenne non fumeurs
#H1:la pression systolique moyenne fumeurs > la pression systolique moyenne non fumeurs
t.test(SF, SNF)
t.test(SF, SNF, alternative="greater")
#t = 3.0685, df = 210.88, p-value = 0.001217
#Th p-value is less than 0.05.
#We do not reject H0.


#------------------------------------------------------------------ 
#Normal distribution curve for la pression systolique pour les hyperactifs
curve(dnorm(x,mean=meansha,sd=SDSHA), from=meansha-3*SDSHA, to=meansha+3*SDSHA)

#Normal distribution curve for la pression systolique pour les non hyperactifs
curve(dnorm(x,mean=meansa,sd=SDSA), from=meansa-3*SDSA, to=meansa+3*SDSA)

#We also made the remark that since it is a big distribution, we can also plot the 
#normal distribution graphs of the two distributions to observe the trend, shape and skewness 
# of the two distribution

   



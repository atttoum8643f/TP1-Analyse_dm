setwd("C:/Users/BossdiDibosS/Desktop/TP1-Analyse_dm/Tp 1 analyses multiples")
wine=read.csv("wine.csv")
attach(wine)
names(wine)
summary(wine)


# calcul de la variances,ecart-types,centrage et réduction des variables
#inerte et barycentre 

Mwine=wine[,4:?2]
eff_tMwine=sum(Mwine[1:21,])#effectif total du tableau(l'effectif total)

poids=matrix(rep(0,21*21),21,21)
for (i in 1:21) {
  poids[i,i]=1/21}

poids[1:5,1:5]#voici un extrait de la matrice des poids


matr_moy=t(Mwine)%*%poids%*%rep(1,21)#Matrice des ?oyennes
matr_moy[1:6,]#voici un extrait de la matrice des moyennes

a=rep(NA,21)
centr=rep(NA,21)
inert=0
M_centr=matrix(rep(NA,21*29),21,29)

for (i in 1:29) {
  
  for (j in 1:21) {
    a[j]=Mwine[j,i]}
  v=t(a)%*%poids%*%a
  e=sqrt(v)
  
  for (k in 1:2?) {
    M_centr[k,i]=(Mwine[k,i]-matr_moy[k])/e}#centrage et réduction des variables
}

M_centr[1:4,1:6]#voici un extrait de la matrice des variables quantitatives centrées et réduites

inert=0

for (i in 1:29) {
  
  for(l in 1:21){
    centr[l]=a[l]-m}
 ?v=t(centr)%*%poids%*%centr#variance de la variable i
  e=sqrt(v)#ecart-types de la variable i
  
  for(t in 1:21){
    M_centr[t,i-3]=centr[t]/e}#matrice des variables quantitatives centrées et réduites
  inert=t (centr/e)%*%poids%*%(centr/e)+inert
}
print?inert)

#calculons la matrice des appellations

Y=matrix(rep(0,21*3),21,3)
a=0
for (i in 1:21) {
  if(appela[i]=="Saumur"){
    Y[i,1]=1}
  else if(appela[i]=="Bourgueuil"){
    Y[i,2]=1}
  else{Y[i,3]=1}
}
Y[1:8,1:3]#extrais de la matrice des appelatition?





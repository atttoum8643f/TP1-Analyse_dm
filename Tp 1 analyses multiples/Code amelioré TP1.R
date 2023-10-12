# calcul de la variances,ecart-types,centrage et réduction des variables
#inerte et barycentre

Mwine=wine[,4:32]
eff_tMwine=sum(Mwine[1:21,])#effectif total du tableau(l'effectif total)

poids=matrix(rep(0,21*21),21,21)
for (i in 1:21) {
  poids[i,i]=1/21}

poids[1:5,1:5]#voici un extrait de la matrice des poids

matr_moy=t(Mwine)%*%poids%*%rep(1,21)#Matrice des moyennes
matr_moy[1:7,]#voici un extrait de la matrice des moyenne

e=rep(NA,29)
v=rep(NA,29)
for (i in 1:29) {
  v[i]=t(Mwine[,i])%*%poids%*%Mwine[,i]-(matr_moy[i])^2
  e[i]=sqrt(v[i])
 }
v[1:7]#voici un extrait de la matrice des variances
e[1:7]#voici un extrait de la matrice des ecart-types

#Le centrage des données
M_centr=matrix(rep(NA,21*29),21,29) 
for (i in 1:29) {
   for (k in 1:21) {
     M_centr[k,i]=(Mwine[k,i]-matr_moy[i])/e[i]}#un extrait de la matrice des variables centrées réduites
  }
M_centr[1:7,1:7]#voici un extrait de la matrice des variables quantitatives centrées et réduites

#Calcule de l'inertie
k=rep(NA,29)
for (i in 1:29){
   k[i]=t(M_centr[,i])%*%poids%*%M_centr[,i]}
inert=sum(k)

appela=wine[,2]
Y=matrix(rep(0,21*3),21,3)#ici Y est en majiscule
a=0
for (i in 1:21){
    if(appela[i]=="Saumur"){
       Y[i,1]=1}
    else if(appela[i]=="Bourgueuil"){
       Y[i,2]=1}
    else{Y[i,3]=1}
  }
Y[1:8,1:3]#extrais de la matrice des appelatition

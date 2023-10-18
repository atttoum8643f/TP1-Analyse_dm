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
    M_centr[k,i]=(Mwine[k,i]-matr_moy[i])/e[i]}
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

#calcule des poids
b=rep(1:3)
for (i in 1:3) {
  b[i]=sum(Y[,i])/sum(Y[,1:3])
}

poids_ap=diag(b)# la matrice des poids des appellations

M=matrix(rep(0,29*29),29,29)
for (i in 1:29){
  M[i,i]=1/29}#la matrice des poids dans R^p

# calcule des barycentre pour chaque appellation
M_saumur=matrix(rep(NA,11*29),11,29)
M_bourg=matrix(rep(NA,6*29),6,29)
M_chinon=matrix(rep(NA,4*29),4,29)
k1=0
k2=0
k3=0

for (i in 1:21) {
  
  if(wine[i,2]=="Saumur"){
    k1=1+k1
    for (j in 1:29) {
         M_saumur[k1,j]=M_centr[i,j]}}
  
  else if(wine[i,2]=="Bourgueuil"){
    k2=1+k2
    for (j in 1:29){
      M_bourg[k2,j]=M_centr[i,j]}}
  
  else {k3=1+k3
    for (j in 1:29){
       M_chinon[k3,j]=M_centr[i,j]}}
}

m_bar=matrix(rep(NA,29*3),29,3)
for (i in 1:29) {
  m_bar[i,1]=sum(M_saumur[,i])/11
  m_bar[i,2]=sum(M_bourg[,i])/6
  m_bar[i,3]=sum(M_chinon[,i])/4
}
m_bar[1:6,1:3]#matrice des barycentres des appellations

n_bar=rep(NA,3)
for (i in 1:3) {
  n_bar[i]=t(m_bar[,i])%*%M%*%m_bar[,i]#la norme des barycentre au carrée
}
n_bar#matrice des normes carrées des barycentres 
R2=sum(n_bar%*%poids_ap)

#PARTIE 2 TP1

#On note:

X=M_centr#la matrice centrée réduite des variables quantitatives
Y=Y#la matrice des indicatrices d'appllatons

sol=wine[,3]
Z=matrix(rep(0,21*4),21,4)

for (i in 1:21){
  if(sol[i]=="Env1"){
    Z[i,1]=1}
  else if(sol[i]=="Env2"){
    Z[i,2]=1}
  else if(sol[i]=="Env4"){
    Z[i,3]=1}
  else {Z[i,4]=1}
}
Z[1:8,1:4]#extrais de la matrice des aindicatrices de la variable sol
W=poids#la matrice des poids dans R^n

#1)_a.

#_b.
#calculons la projection sur Y et sur Z

Q=solve(t(Y)%*%W%*%Y)
G=solve(t(Z)%*%W%*%Z)
T=Y%*%Q%*%t(Y)%*%W
T[1:6,1:6]#extrait de la matrice de projection sur Y
H=Z%*%G%*%t(Z)%*%W
H[1:6,1:6]#extrait de la matrice de projection sur Z

for (k in 1:29) {
  Qk=solve(t(X[,k])%*%W%*%X[,k])
  Tk=X[,k]%*%Qk%*%t(X[,k])%*%W
  #print(Tk)#extrait de la matrice de projection de xk
  Tr=sum(diag(Tk%*%T))#la trace de la matrice Tk*T
  print(Tr)}

for (k in 1:4) {
  Gk=solve(t(Z[,k])%*%W%*%Z[,k])
  Hk=Z[,k]%*%Gk%*%t(Z[,k])%*%W
  T_R=sum(diag(Hk%*%H))#la trace de la matrice Hk*H
  print(T_R)
}

#_c)

R=X%*%M%*%t(X)%*%W
R[1:5,1:6]#extrait de la matrice R
sum(diag(R%*%T))#la trace de la matrice R*T
sum(diag(R%*%H))#la trace de la matrice R*H






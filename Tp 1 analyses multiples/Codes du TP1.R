setwd("C:/Users/BossdiDibosS/Desktop/TP1-Analyse_dm/Tp 1 analyses multiples")
wine=read.csv("wine.csv")
attach(wine)
names(wine)
summary(wine)

# calcul de la variances,ecart-types,centrage et réduction des variables
#inerte et barycentre 

  
n=32
ef_totw?ne=0#effectif total du tableau
inert=0
bar_centr=0
a=rep(NA,21)
centr=rep(NA,l21)
M_centr=matrix(nrow=21,ncol=29)#matrice des variables centrées

for(i in 4:n){
  
  for(j in 1:21){
    a[j]=wine[j,i]}
  eff_tot=sum(a)# l'effectif total de la variable i
  ?f_totwine=ef_totwine+eff_tot}#l'effectif totat du tableau 
print(ef_totwine)

 
poids=matrix(rep(0,21*21),21,21) 
for(i in 1:21){
      a= (1/ef_totwine)*Mwine[i,]#Fréquence de l'individus çi
      metri=sum(a)
      #print(metri)
      poids[i-3,i-3]=metr?
 } 
print(poids)

print(metri)
poids=diag(c(metri))#matrice des poids de la variable i
m=t(a)%*%poids%*%rep(1,21)

  
  for(l in 1:21){
      centr[l]=a[l]-m}
  v=t(centr)%*%poids%*%centr#variance de la variable i
  e=sqrt(v)#ecart-types de la variable i
? 
  for(t in 1:21){
      M_centr[t,i-3]=centr[t]/e}#matrice des variables quantitatives centrées et réduites
  inert=t (centr/e)%*%poids%*%(centr/e)+inert

print(inert)


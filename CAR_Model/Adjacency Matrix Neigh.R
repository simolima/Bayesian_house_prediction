library(spdep)
library(maps)
library(maptools)
library(classInt)
library(RColorBrewer)

train <- read.csv("train.csv", header=TRUE, sep=";")
n <- dim(train)[1]


#Guardiamo quanti sono i quartieri

quartieri=levels(as.factor(train$Neighborhood)) #25 quartieri servirà una matrice 25 per 25

prova=matrix(data=0,nrow=25,ncol=25)

wvuota=as.data.frame(prova)

colnames(wvuota)=levels(as.factor(train$Neighborhood))

rownames(wvuota)=levels(as.factor(train$Neighborhood))


#Riempio matrice adiacenza 


wvuota[1,9]=1 #Blmngt-Gilbert


wvuota[2,7]=1  ; wvuota[2,24]=1


wvuota[3,13]=1 ; wvuota[3,15]=1   ;   wvuota[3,17]=1;


wvuota[4,10]=1 ;  wvuota[4,13]=1  ;   wvuota[4,18]=1 ;


wvuota[5,6]=1  ;  wvuota[5,8]=1    ; wvuota[5,19]=1       ; wvuota[5,20]=1


wvuota[6,5]=1  ;  wvuota[6,8]=1    ; wvuota[6,20]=1       ;


wvuota[7,2]=1  ;  wvuota[7,23]=1 


wvuota[8,5]=1  ;  wvuota[8,6]=1    ; wvuota[8,23]=1 


wvuota[9,1]=1  ;  wvuota[9,17]=1    ;  wvuota[9,21]=1      ; wvuota[9,22]=1 


wvuota[10,4]=1  ;  wvuota[10,18]=1 


wvuota[11,12]=1


wvuota[12,11]=1


wvuota[13,3]=1  ;  wvuota[13,4]=1    ;  wvuota[13,15]=1   ;  wvuota[13,17]=1  ;  wvuota[13,18]=1


wvuota[14,16]=1   ; wvuota[14,21]=1    ;  wvuota[14,25]=1


wvuota[15,3]=1  ;  wvuota[15,13]=1


wvuota[16,14]=1 


wvuota[17,3]=1  ;  wvuota[17,9]=1   ;   wvuota[17,13]=1   ;  wvuota[17,21]=1


wvuota[18,4]=1  ;  wvuota[18,10]=1  ;   wvuota[18,13]=1  


wvuota[19,5]=1 ;  wvuota[19,20]=1   ;   


wvuota[20,5]=1 ;  wvuota[20,6]=1    ;   wvuota[20,19]=1


wvuota[21,9]=1 ;  wvuota[21,14]=1   ;   wvuota[21,17]=1


wvuota[22,9]=1 


wvuota[23,7]=1 ;  wvuota[23,8]=1     


wvuota[24,2]=1


wvuota[25,14]=1   ;  





Wfinale=wvuota

write.table(Wfinale, "Adjmatrixout.csv", 
            sep = ";",             # punto e virgola
            row.names = TRUE,     # se abbiamo la variabile ID
            dec = ",",             # separatore di decimali
            na = "",               # dati mancanti come celle vuote
            quote = TRUE
)


p=1

#Verifico simmetria (se p=1 la matrice è simmetrica)

for ( i in 1:25) {
  for (j in 1:25) {
    if(Wfinale[i,j]!=Wfinale[j,i]){
      p=0
    }
    
  }
  
}






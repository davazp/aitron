#include <stdio.h>
#include <stdlib.h>
#include <math.h>



#define MAPA_SIZE_i 100
#define MAPA_SIZE_j 100

typedef struct jugador{
  int i;
  int j;
}jugador;

typedef struct juego{
  int tablero[MAPA_SIZE_i][MAPA_SIZE_j];
  int N;
  int arg;
}juego;

int propagacion(int i, int j,int tablero[MAPA_SIZE_i][MAPA_SIZE_j]){
  int R=0;
  if(tablero[i-1][j]!=0 | i-1<0){
    R=R+1;
  }  
  if(tablero[i][j+1]!=0 | j+1>=MAPA_SIZE_j){
    R=R+1;
  }
  if(tablero[i+1][j]!=0 | i+1>=MAPA_SIZE_i){
    R=R+1;
  }
  if(tablero[i][j-1]!=0 | j-1<0){
    R=R+1;
  }
  if(R>=3){
    R=R+1;
  }
  return R;
}

int patron_tunel(int i, int j,int tablero[MAPA_SIZE_i][MAPA_SIZE_j]){
  int R=0;
  if(tablero[i-1][j-2]!=0 & tablero[i+1][j-2]!=0 & tablero[i-1][j-1]!=0 & 
     tablero[i+1][j-1]!=0){
    R=R+2;
  }
  if(tablero[i-1][j+2]!=0 & tablero[i+1][j+2]!=0 & tablero[i-1][j+1]!=0 & 
     tablero[i+1][j+1]!=0){
    R=R+2;
  }
  if(tablero[i+2][j-1]!=0 & tablero[i+2][j+1]!=0 & tablero[i+1][j-1]!=0 & 
     tablero[i+1][j+1]!=0){
    R=R+2;
  }
  if(tablero[i-2][j-1]!=0 & tablero[i-2][j+1]!=0 & tablero[i-1][j-1]!=0 & 
     tablero[i-1][j+1]!=0){
    R=R+2;
  }
  return R;
}

int Abs (int i){
  if(i<0){
    return -i;
  }
  else{
    return i;
  }
}

int Distancia_Manhatan(int i, int j, int k, int n){
  int D;
  D=Abs(i-k)+Abs(j-n);
/*Ahora como si no estuviera*/
  return 0;
}

int* Decision(int i, int j,int k,int n,
	      int tablero[MAPA_SIZE_i][MAPA_SIZE_j]){
  int UP,DOWN,RIGHT,LEFT; 
  int *Result=(int)malloc(2*sizeof(int));
  RIGHT=0;
  LEFT=0;
  UP=0;
  DOWN=0;
  /*Arriba*/
  if(tablero[i-1][j]!=0 | i-1<0){
    UP=300;
  }
  else{
    UP=2*propagacion(i-1,j,tablero)+
      patron_tunel(i-1,j,tablero)+Distancia_Manhatan(i-1,j,k,n);;
  }
  /*Derecha*/
  if(tablero[i][j+1]!=0 | j+1>=MAPA_SIZE_j){
    RIGHT=300;
  }
  else{
    RIGHT=2*propagacion(i,j+1,tablero)+
      patron_tunel(i,j+1,tablero)+Distancia_Manhatan(i,j+1,k,n);
  }
  /*Abajo*/
  if(tablero[i+1][j]!=0 | i+1>=MAPA_SIZE_i){
    DOWN=300;
  }
  else{
    DOWN=2*propagacion(i+1,j,tablero)+
      patron_tunel(i+1,j,tablero)+Distancia_Manhatan(i+1,j,k,n);
  }
  /*Izquierda*/
  if(tablero[i][j-1]!=0 | j-1<0){
    LEFT=300;
  }
  else{
    LEFT=2*propagacion(i,j-1,tablero)+
      patron_tunel(i,j-1,tablero)+Distancia_Manhatan(i,j-1,k,n);
  }
  /*En este caso gira a la derecha*/
  if(RIGHT<=LEFT & RIGHT<=UP & RIGHT<=DOWN){
    j=j+1;
  }
  /*En este caso gira hacia abajo*/
  else if(DOWN<=RIGHT & DOWN<=LEFT & DOWN<=UP){
    i=i+1;
  }
  /*En este caso gira para la izquierda*/
  else if(LEFT<=RIGHT & LEFT<=DOWN & LEFT<=UP){
    j=j-1;
  }
  /*En este caso gira hacia arriba*/
  else{
    i=i-1;
  }
  Result[0]=i;
  Result[1]=j;
  return Result;
}

int main(){
  juego tron;
  jugador local;
  jugador adversario;
  int i,j,k;
  int *Result=(int)malloc(2*sizeof(int));
  /*Posicion actual jugador local*/
  scanf("%d %d",&local.i,&local.j);
  /*Posicion actual jugador adversario*/
  scanf("%d %d",&adversario.i,&adversario.j);
  /*Recibe el numero de obstaculos*/
  scanf("%d %d",&tron.N,&tron.arg);
  /*a cero posiciones libres*/ 
  for(i=0;i<MAPA_SIZE_i;i++){
    for(j=0;j<MAPA_SIZE_j;j++){
      tron.tablero[i][j]=0;
    }
  }
  /*a 1 posiciones ocupadas por obstaculo*/
  for(k=0;k<tron.N;k++){
    scanf("%d %d",&i,&j);
    tron.tablero[i][j]=1;
  }  
  /*a 3 posiciones ocupadas por local*/
  tron.tablero[local.i][local.j]=3;
  /*Actuar*/
  while(1){
    /*a 2 posiciones ocupadas por contrincante*/
    tron.tablero[adversario.i][adversario.j]=2;
    /**/
    /*Toma de decision*/
    /*Maximizamos primero opcion del adversario*/
    Result=Decision(adversario.i,adversario.j,local.i,local.j,tron.tablero);
    i=Result[0];
    j=Result[1];
    /*Definimos un tablero ficticio donde el adversario
      a optado por su mejor opcion libre de obstaculos sin contar
      con mi futura opcion*/
    tron.tablero[i][j]=2;
    /**/
    Result=Decision(local.i,local.j,i,j,tron.tablero);
    local.i=Result[0];
    local.j=Result[1];
    /*Devolvemos el tablero a su estado original*/
    tron.tablero[i][j]=0;
    /**/ 
    tron.tablero[local.i][local.j]=3;
    printf("%d %d\n",local.i,local.j);
    fflush(stdout);
    scanf("%d %d",&adversario.i,&adversario.j);
  }
  return 0;
}

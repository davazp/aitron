/* Si usas emacs, puedes compilar con M-x compile */

#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<signal.h>
#include<SDL.h>

/* Numero del lado del tablero (es cuadrado). */
#define N 100

/* Tamaño en pixeles de cada celda. */
#define POINTSIZE 6

/* Superficie SDL donde dibujamos la simulacion. */
SDL_Surface * screen;

/* Mapa de celdas ocupadas. No necesitamos distinguir entre celdas de
   un jugador y de otro, simplemente estan ocupadas y si chocas con
   ellas mueres. */
int map[N][N];


/* Configuracion y estadisticas */
unsigned int delay = 100;
int graphicsp = 1;
int plays = 1;
int nwalls = 100;

int current_play;
int turns = 0;

/* Dibuja un cuadrado en pantalla. */
void
draw_point (int i, int j, int color)
{
  SDL_Rect rect;
  if (!graphicsp) return;
  rect.x = j * POINTSIZE;
  rect.y = i * POINTSIZE;
  rect.w = POINTSIZE;
  rect.h = POINTSIZE;
  SDL_FillRect (screen, &rect, color+1315860);
  rect.x = j * POINTSIZE + 1;
  rect.y = i * POINTSIZE + 1;
  rect.w = POINTSIZE - 2;
  rect.h = POINTSIZE - 2;
  SDL_FillRect (screen, &rect, color);
}

typedef struct
{
  char * name;
  int i, j;
  FILE* fdwrite;
  FILE* fdread;
  int pid;
  int loses;
} player_t ;


void
write_cords (player_t * player, int i, int j)
{
  fprintf (player->fdwrite, "%d %d\n", i , j);
  fflush (player->fdwrite);
}

void
read_cords (player_t * player, int *i, int *j)
{
  fscanf (player->fdread, "%d %d", i, j);
}

player_t *
create_player (const char *program)
{
  player_t * player;
  player = malloc (sizeof(player_t));
  if (player == NULL)
    {
      fprintf (stderr, "Error, falta memoria.\n");
      exit (-1);
    }
  player->name = strdup (program);
  player->loses = 0;
  return player;
}


void
prepare_player (player_t * player, int i, int j)
{
  int pstdin[2];
  int pstdout[2];
  int pid;
  pipe (pstdin);
  pipe (pstdout);
  pid = fork();
  if (!pid)
    {
      /* En el proceso hijo, redefinimos la entrada y la salida a las
         tuberias de tron y reemplazamos la imagen del proceso por el
         programa del jugador. */
      close (STDIN_FILENO);
      close (STDOUT_FILENO);
      dup2 (pstdin[0], STDIN_FILENO);
      dup2 (pstdout[1], STDOUT_FILENO);
      close (pstdin[0]);
      close (pstdin[1]);
      close (pstdout[0]);
      close (pstdout[1]);
      if (execve (player->name, NULL, NULL))
        {
          fprintf (stderr, "Error, no se pudo cargar los jugadores.\n");
          exit (-1);
        }
      /* El programa NUNCA alcanza este punto.*/
      return;
    }
  close (pstdin[0]);
  close (pstdout[1]);
  player->pid = pid;
  player->fdwrite = fdopen (pstdin[1], "w");
  player->fdread  = fdopen (pstdout[0], "r");
  player->i = i;
  player->j = j;
  write_cords (player, i, j);
}

void
finish_player (player_t * player)
{
  kill(player->pid, SIGKILL);
  fclose (player->fdread);
  fclose (player->fdwrite);
}

int
close_player (player_t * player)
{
  free (player->name);
  free (player);
  return 0;
}


static inline int
cheatp (player_t * player, int i, int j)
{
  return abs (player->i - i) + abs (player->j - j) != 1 ;
}

static inline int
sillyp (player_t * player, int i, int j)
{
  return i<0 || i>=N || j<0 || j>=N;
}


int
lossp (player_t * player, int i, int j)
{
  if(cheatp (player, i, j))
    {
      printf ("%d: Jugador `%s' pierde por tramposo.\n", current_play, player->name);
      player->loses++;
      return -1;
    }
  if(sillyp (player, i, j))
    {
      printf ("%d: Jugador `%s' pierde por inutil.\n", current_play, player->name);
      player->loses++;
      return -1;
    }
  if (map[i][j])
    {
      printf ("%d: Jugador `%s' es un perdedor.\n", current_play, player->name);
      player->loses++;
      return -1;
    }
}


int
move (player_t * player, int i, int j)
{
  player->i = i;
  player->j = j;
  map[i][j]=1;
  return 0;
}


/* Generador de posiciones aleatorias UNICAS y libres */

static int (*random_map)[2];
static int random_index;

void
initialize_random (void)
{
  int i,j;
  random_map = malloc (sizeof(int)*2*N*N);
  random_index = 0;
  for(i=0; i<N; i++)
    {
      for(j=0; j<N; j++)
        {
          random_map[i*N + j][0] = i;
          random_map[i*N + j][1] = j;
        }
    }
}

void
random_position (int *i, int*j)
{
  static int initialized=0;
  int offset;
  if (!initialized)
    {
      srand (time (NULL));
      initialized=1;
    }
  /* Un numero aleatorio desde 0 a N^2 - RANDOM_INDEX. Esto es, una
     posicion aleatoria en el mapa random_map a partir de
     RANDOM_INDEX.  */
  do
    {
      offset = (int)((float)rand() / RAND_MAX * N * N - random_index);
      /* Escribimos la posicion */
      *i = random_map[random_index + offset][0];
      *j = random_map[random_index + offset][1];
      /* Intercambiamos la celda aleatoria por la que de orden-major
         RANDOM_INDEX e incrementamos este. */
      random_map[random_index + offset][0] = random_map[random_index][0];
      random_map[random_index][0] = *i;
      random_map[random_index + offset][1] = random_map[random_index][1];
      random_map[random_index][1] = *j;
      random_index++;
    }
  while(map[*i][*j]);
}

void
finalize_random (void)
{
  free (random_map);
}


/* Generador de muros. */

/* Probabilidad con la que un muro continuara, es decir, 1-wall_p es
   la probabilidad de terminar el muro actual. Cada muro se expande en
   una dirección libre aleatoria, de no haberla, el muro se termina y
   se comienza otro. */
#define WALL_P 0.95484160391

void
walls (int n, player_t * p1, player_t * p2)
{
  Uint32 color;
  int (*v)[2];
  int i,j;
  int index;
  int new_wall_p;
  if (graphicsp)
    color = SDL_MapRGB(screen->format, 50, 50, 50);

  /* Envia la cantidad de muros que habra */
  write_cords (p1, n, 0);
  write_cords (p2, n, 0);

  new_wall_p = 1;
  while(n>0)
    {
      int i,j;
      if (!new_wall_p)
        {
          int c;
          int directions[][2] = {{-1, 0}, {0, -1}, {1, 0}, {0, 1}};
          int di, dj;
          int d;
          c=0;
          do
            {
              d = (int)((float)rand() / RAND_MAX * 4);
              di = directions[d][0];
              dj = directions[d][1];
              directions[d][0] = -1;
              directions[d][0] = -1;
              c++;
            }
          while( c<20 &&
                 (i+di<0 || i+di >= N ||
                  j+dj<0 || j+dj >= N ||
                  map[i+di][j+dj] == 1 ));
          i += di;
          j += dj;
          if (c==20)
            new_wall_p = 1;
          else
            new_wall_p = (float)rand() / RAND_MAX >= WALL_P;
        }
      if (new_wall_p)
        {
          random_position (&i, &j);
          new_wall_p = 0;
        }
      draw_point (i, j, color);
      map[i][j]=1;
      write_cords (p1, i, j);
      write_cords (p2, i, j);
      n--;
    }
}



void
usage (void)
{
  fprintf (stderr, "Uso: ./tron [-n NUMERO] [-b] [-d ms] programa1 programa2\n");
  exit (-1);
}


/* Funcion principal del programa */
int
main (int argc, char * argv[])
{
  player_t * player1;
  player_t * player2;
  int finishp;
  int p[2][2];
  int opt;
  const char * progname1;
  const char * progname2;

  while ((opt = getopt(argc, argv, "bd:n:w:")) != -1) {
    switch (opt) {
      /* Establece el retraso entre movimiento y movimiento */
    case 'd':
      delay = atoi (optarg);
      break;
      /* No usa SDL, solo recolecta estadisticsa.  */
    case 'b':
      graphicsp = 0;
      break;
      /* Numero de partidas para simular. */
    case 'n':
      plays = atoi (optarg);
      break;
      /* Numero de particulas de muro */
    case 'w':
      nwalls = atoi (optarg);
      break;
    default: /* '?' */
      usage();
    }
  }
  if (argc-optind < 2)
    usage();

  progname1 = argv[optind++];
  progname2 = argv[optind++];

  if(graphicsp)
    {
      SDL_Init (SDL_INIT_VIDEO);
      SDL_WM_SetCaption ("AI Tron", NULL);
      screen = SDL_SetVideoMode(N * POINTSIZE, N * POINTSIZE, 32, SDL_HWSURFACE);
      if (screen == NULL)
        {
          fprintf (stderr, "El sistema no soporta esta resolucion.\n");
          exit (-1);
        }
    }

  initialize_random();

  player1 = create_player (progname1);
  player2 = create_player (progname2);
  for(current_play=0; current_play<plays; current_play++)
    {
      memset (map, 0, sizeof(map));
      if (graphicsp)
        SDL_FillRect( screen, NULL, 0 );

      random_position (&p[0][0], &p[0][1]);
      random_position (&p[1][0], &p[1][1]);
      prepare_player (player1, p[0][0], p[1][1]);
      prepare_player (player2, p[1][0], p[1][1]);
      write_cords (player1, p[1][0], p[1][1]);
      write_cords (player2, p[0][0], p[0][1]);

      walls(nwalls, player1, player2);

      /* Bucle principal */
      finishp=0;
      while(!finishp)
        {
          SDL_Event event;
          draw_point (player1->i, player1->j, 255);
          draw_point (player2->i, player2->j, 65025);
          /* Lee los siguientes movimientos de cada bot */
          read_cords (player1, &p[0][0], &p[0][1]);
          read_cords (player2, &p[1][0], &p[1][1]);
          /* Comprueba si alguien ha perdido */
          if (p[0][0]==p[1][0] && p[0][1]==p[1][1])
            {
              player1->loses++;
              player2->loses++;
              finishp = 1;
            }
          else
            {
              finishp |= lossp (player1, p[0][0], p[0][1]);
              finishp |= lossp (player2, p[1][0], p[1][1]);
            }
          /* Actualiza el mapa con los movimientos */
          move (player1, p[0][0], p[0][1]);
          move (player2, p[1][0], p[1][1]);
          turns++;
          /* Informa del ultimo movimiento de cada bot al otro */
          write_cords (player1, p[1][0], p[1][1]);
          write_cords (player2, p[0][0], p[0][1]);

          if (graphicsp)
            {
              /* Elimina procesos de la cola, y termina en caso de que el
                 usuario lo haya solicitado. */
              while( SDL_PollEvent( &event ) ){
                if (event.type == SDL_QUIT)
                  {
                    plays = current_play;
                    finishp = 1;
                  }
              }
              SDL_Flip (screen);
              SDL_Delay (delay);
            }
        }
      finish_player (player1);
      finish_player (player2);
    }

  printf ("Resumen:\n");
  printf ("    %s: %d/%d\n", player1->name, plays - player1->loses, plays);
  printf ("    %s: %d/%d\n", player2->name, plays - player2->loses, plays);
  printf ("\n");
  printf ("Un total de %d turnos, %f turnos de media por partida.\n", turns, (float)turns/plays);
  close_player (player1);
  close_player (player2);

  finalize_random();

  if (graphicsp)
    SDL_Quit ();

  return 0;
}

/* tron.c termina aqui */

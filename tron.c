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

int current_play;
int turns = 0;

/* Dibuja un cuadrado en pantalla. */
void
draw_point (int i, int j, int color)
{
  SDL_Rect rect;

  if (!graphicsp)
    return;

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
  SDL_Flip (screen);
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
move (player_t * player, int i, int j)
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
  player->i = i;
  player->j = j;
  map[i][j]=1;
  return 0;
}

void
random_position (int *i, int*j)
{
  static int initialized=0;
  if (!initialized)
    {
      srand (time (NULL));
      initialized=1;
    }
  *i = (int)((float)rand() / RAND_MAX * N);
  *j = (int)((float)rand() / RAND_MAX * N);
}

void
walls (int n, player_t * p1, player_t * p2)
{
  Uint32 color;
  if (graphicsp)
    color = SDL_MapRGB(screen->format, 50, 50, 50);
  while(n>0)
    {
      int ii,jj;
      int i[5],j[5];
      random_position (&i[0], &j[0]);
      random_position (&i[1], &j[1]);
      random_position (&i[2], &j[2]);
      random_position (&i[3], &j[3]);
      random_position (&i[4], &j[4]);
      ii = (i[0] + i[1] + i[2] + i[3] + i[4]) / 5;
      jj = (j[0] + j[1] + j[2] + j[3] + j[4]) / 5;
      if (!map[ii][jj])
        {
          draw_point (ii, jj, color);
          map[ii][jj]=1;
          write_cords (p1, ii, jj);
          write_cords (p2, ii, jj);
          n--;
        }
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

  while ((opt = getopt(argc, argv, "bd:n:")) != -1) {
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

  player1 = create_player (progname1);
  player2 = create_player (progname2);
  for(current_play=0; current_play<plays; current_play++)
    {
      memset (map, 0, sizeof(map));
      if (graphicsp)
        SDL_FillRect( screen, NULL, 0 );

      do {
        random_position (&p[0][0], &p[0][1]);
      } while (map[ p[0][0] ][ p[0][1] ]);
      do {
        random_position (&p[1][0], &p[1][1]);
      } while (map[ p[1][0] ][ p[1][1] ]);

      prepare_player (player1, p[0][0], p[1][1]);
      prepare_player (player2, p[1][0], p[1][1]);
      write_cords (player1, p[1][0], p[1][1]);
      write_cords (player2, p[0][0], p[0][1]);

      walls(100, player1, player2);

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
          /* Actualiza el mapa con los movimientos */
          finishp |= move (player1, p[0][0], p[0][1]);
          finishp |= move (player2, p[1][0], p[1][1]);
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
                  plays = current_play;
                  finishp = 1;
              }
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

  if (graphicsp)
    SDL_Quit ();

  return 0;
}

/* tron.c termina aqui */

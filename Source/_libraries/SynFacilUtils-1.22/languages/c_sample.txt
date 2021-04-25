#include <stdio.h>
#include <math.h>

void main(){
  struct punto {
    int x;
    int y;
    };
  struct punto pto1, pto2;

  double dist;

  printf("\n\nESTRUCTURAS de punto(x,y)\n");

  /* asignacion inicial a pto1 */
  pto1.x = 3;
  pto1.y = 5;

  printf("Punto pt1: x=%d, y=%d\n", pto1.x, pto1.y);

  /* asignaciOn de pto2 trasladado 2,4 
  */
  pto2.x = pto1.x + 2;
  pto2.y = pto1.y + 4;

  printf("Punto pt2: x=%d, y=%d\n", pto2.x, pto2.y);

  }

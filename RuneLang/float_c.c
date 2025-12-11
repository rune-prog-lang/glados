#include <stdio.h>

void show_float(float value) {
  printf("%f", value);
  putchar('\n');
}

int main(void) {
  float a = 42.0;

  show_float(23.0);
  show_float(a);
  return 0;
}

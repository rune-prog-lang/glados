#include <stdio.h>

int main(void) {
  long result = 0;

  for (long i = 0; i < 1000000; ++i) {
    result += i;
  }
  printf("result: %ld\n", result);
}

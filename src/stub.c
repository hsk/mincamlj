#include <stdio.h>
#include <stdlib.h>

extern void min_caml_start(char *, char *);

/*
void *min_caml_create_array(int s) {
    int *ret, *p;
    __asm__("movl %%edi, %0" : "=g"(ret));
    p = ret;
    for (;s--;) {
        *p++ = 0;
    }
    __asm__("movl %0, %%edi" :: "g"(p));
    return ret;
}
*/

int main() {
  char *hp, *sp;

  sp = malloc(4000000);
  hp = malloc(4000000);
  if (hp == NULL || sp == NULL) {
    fprintf(stderr, "malloc failed\n");
    return 1;
  }
//  fprintf(stderr, "sp = %p, hp = %p\n", sp, hp);
  min_caml_start(sp, hp);

  return 0;
}

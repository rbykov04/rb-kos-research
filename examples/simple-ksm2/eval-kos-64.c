
#include "config-kos-64.h"
#include "eval.c"
//#include "MicroHs/src/runtime/eval.c"
//
void			*malloc(size_t size)
{
  return NULL; //FIXME: Not impl yet
}
void			*realloc(void *ptr, size_t size)
{
  return NULL; //FIXME: Not impl yet
}

void			free(void *ptr)
{
  return ; //FIXME: Not impl yet
}



void			*calloc(size_t nmemb, size_t size)
{
  unsigned int		s;
  char			*ptr;

  s = nmemb * size;
  if ((ptr = malloc(s)) == NULL)
    return (NULL);
  memset(ptr, 0, s);
  return (ptr);
}

typedef int cmp_t(const void*, const void*);
void qsort(void *a, size_t n, size_t es, cmp_t *cmp){

}


//#include <string.h>
char * __strcat_chk(char * dest, const char * src, size_t destlen) {
    return NULL;
}
char * __strcpy_chk(char * dest, const char * src, size_t destlen) {
    return NULL;
}
[[noreturn]] void exit(int status) {
   //FIXME
}

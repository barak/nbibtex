#line 61 "boyer-moore.nw"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include <lua.h>
#include <lauxlib.h>

#define ALPHASIZE 256  /* size of alphabet */
/* bmBc[256] has last occurrence of each character in pattern */

static void preBmBc(const unsigned char *x, int m, int bmBc[]) {
   int i;
 
   for (i = 0; i < ALPHASIZE; ++i)
      bmBc[i] = m;
   for (i = 0; i < m - 1; ++i)
      bmBc[x[i]] = m - i - 1;
}
 
 
/* suff[m] does what?? */
static void suffixes(const unsigned char *x, int m, int *suff) {
   int f, g, i;
 
   suff[m - 1] = m;
   g = m - 1;
   for (i = m - 2; i >= 0; --i) {
      if (i > g && suff[i + m - 1 - f] < i - g)
         suff[i] = suff[i + m - 1 - f];
      else {
         if (i < g)
            g = i;
         f = i;
         while (g >= 0 && x[g] == x[g + m - 1 - f])
            --g;
         suff[i] = f - g;
      }
   }
}
 
static void preBmGs(const unsigned char *x, int m, int bmGs[]) {
   int i, j, *suff;
   suff = malloc(m * sizeof(*suff));
   assert(suff);
 
   suffixes(x, m, suff);
 
   for (i = 0; i < m; ++i)
      bmGs[i] = m;
   j = 0;
   for (i = m - 1; i >= -1; --i)
      if (i == -1 || suff[i] == i + 1)
         for (; j < m - 1 - i; ++j)
            if (bmGs[j] == m)
               bmGs[j] = m - 1 - i;
   for (i = 0; i <= m - 2; ++i)
      bmGs[m - 1 - suff[i]] = m - 1 - i;
   free(suff);
}
 
#define MAX(A, B) ((A) > (B) ? (A) : (B))

#define MATMETA "boyer-moore.ncmatcher"

typedef struct matcher {
  int m;
  int bmBc[ALPHASIZE];
  int *bmGs;
  unsigned char *pat;
} *Matcher;

static int compile_nocase(lua_State *L) {
  /* take string and return compiled tolower(string) as a case-insensitive matcher */
  const unsigned char *x = (const unsigned char *) luaL_checkstring(L, 1);
  int m = lua_strlen(L, 1);
  Matcher mat = lua_newuserdata(L, sizeof *mat + m * sizeof *(mat->bmGs) + m);
  int i;

  if (m == 0)
    luaL_error(L, "Tried to compile Boyer-Moore matcher for empty string");

  assert(mat);
  luaL_getmetatable(L, MATMETA);
  lua_setmetatable(L, -2);
  
  mat->m    = m;
  mat->bmGs = (int *) (mat+1);
  mat->pat  = (unsigned char *)(mat->bmGs + m);
  for (i = 0; i < m; i++)
    mat->pat[i] = tolower(x[i]);

  /* Preprocessing */
  preBmGs(x, m, mat->bmGs);
  preBmBc(x, m, &mat->bmBc[0]);
  return 1;
}

static int search_nocase(lua_State *L) {
  /* take compiled matcher, string and return first_match, last_match or nil */
  Matcher mat = luaL_checkudata(L, 1, MATMETA);
  const unsigned char *y = (const unsigned char *) luaL_checkstring(L, 2);
  int n = lua_strlen(L, 2);
  int i, j;
  const unsigned char *x = mat->pat;
  int *bmGs = mat->bmGs;
  int *bmBc = &mat->bmBc[0];
  int m = mat->m;

  /* Searching */
  for (j = 0; j <= n - m; ) {
     for (i = m - 1; i >= 0 && (x[i] == y[i + j] || x[i] == tolower(y[i+j])); --i);
     if (i < 0) {
       lua_pushnumber(L, j+1);  /* lua positions are 1-indexed */
       lua_pushnumber(L, j+m);  /* position of final char (j+1+m-1) */
       return 2;
       j += bmGs[0];
     } else
       j += MAX(bmGs[i], bmBc[tolower(y[i + j])] - m + 1 + i);
  }
  return 0;  /* no match */
}

static const struct luaL_reg bmlib [] = {
  {"compilenc", compile_nocase },
  {"matchnc",   search_nocase },
  {NULL, NULL}
};
    
int luaopen_boyer_moore (lua_State *L) {
  luaL_newmetatable(L, MATMETA);
  luaL_openlib(L, "boyer_moore", bmlib, 0);
  return 1;
}

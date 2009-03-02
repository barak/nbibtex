#line 1179 "nbib.nw"
#include <stdlib.h>
#include <stdio.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

extern int luaopen_bibtex(lua_State *L);
extern int luaopen_boyer_moore (lua_State *L);

int main (int argc, char *argv[]) {
  int i, rc;
  lua_State *L = lua_open();
  static const char* files[] = { SHARE "/bibtex.lua",  SHARE "/natbib.nbs" };

  luaopen_base(L);
  luaopen_table(L);
  luaopen_io(L);
  luaopen_loadlib(L);
  luaopen_string(L);
  luaopen_bibtex(L);
  luaopen_boyer_moore(L);
  for (i = 0; i < sizeof(files)/sizeof(files[0]); i++) {
    if (lua_dofile(L, files[i])) {
      fprintf(stderr, "%s: error loading configuration file %s\n",
              argv[0], files[i]);
      exit(2);
    }
  }
  lua_pushstring(L, "bibtex");
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushstring(L, "main");
  lua_gettable(L, -2);
  lua_newtable(L);
  for (i = 0; i < argc; i++) {
    lua_pushnumber(L, i);
    lua_pushstring(L, argv[i]);
    lua_settable(L, -3);
  }
  rc = lua_pcall(L, 1, 0, 0);
  if (rc) {
    fprintf(stderr, "Call failed: %s\n", lua_tostring(L, -1));
    lua_pop(L, 1);
  }
  lua_close(L);
  return rc;
}

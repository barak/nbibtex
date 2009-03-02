#line 110 "nbib.nw"
#include <stdio.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include <lua.h>
#include <lauxlib.h>

#line 157 "nbib.nw"
typedef struct bibreader {
  const char *filename;             /* name of the .bib file */
  FILE *file;                       /* .bib file open for read */
  int line_num;                     /* line number of the .bib file */
  int entry_line;                   /* line number of last seen entry start */

  unsigned char *buf, *cur, *lim;   /* input buffer */
  unsigned bufsize;                 /* size of buffer */
  char entry_close;   /* character expected to close current entry */

  lua_State *L;
  int preamble;       /* reference to preamble list of strings */
  int warning;        /* reference to universal warning function */
  int macros;         /* reference to macro table */
} *Bibreader;
#line 189 "nbib.nw"
typedef int bool;
#line 194 "nbib.nw"
static bool getline(Bibreader rdr);
#line 203 "nbib.nw"
static bool upto1(Bibreader rdr, char c);
static bool upto1_getline(Bibreader rdr, char c);
static void upto_white_or_1(Bibreader rdr, char c);
static void upto_white_or_2(Bibreader rdr, char c1, char c2);
static void upto_white_or_3(Bibreader rdr, char c1, char c2, char c3);
static bool upto_nonwhite(Bibreader rdr);
static bool upto_nonwhite_getline(Bibreader rdr);
#line 218 "nbib.nw"
static bool scan_identifier (Bibreader rdr, char c1, char c2, char c3);
static bool scan_nonneg_integer (Bibreader rdr, unsigned *np);
#line 224 "nbib.nw"
static bool scan_and_buffer_a_field_token (Bibreader rdr, int key, luaL_Buffer *b);
static bool scan_balanced_braces(Bibreader rdr, char close, luaL_Buffer *b);
static bool scan_and_push_the_field_value (Bibreader rdr, int key);
#line 234 "nbib.nw"
static void lower_case(unsigned char *p, unsigned char *lim);
static void strip_leading_and_trailing_space(lua_State *L);
#line 239 "nbib.nw"
static int get_bib_command_or_entry_and_process(Bibreader rdr);
int luaopen_bibtex (lua_State *L);
#line 249 "nbib.nw"
typedef bool (*Command)(Bibreader);
static Command find_command(unsigned char *p, unsigned char *lim);
static bool do_comment (Bibreader rdr);
static bool do_preamble(Bibreader rdr);
static bool do_string  (Bibreader rdr);
#line 266 "nbib.nw"
static void warnv(Bibreader rdr, int nres, const char *fmt, ...);
#line 998 "nbib.nw"
static int openreader(lua_State *L);
static int next_entry(lua_State *L);
static int closereader(lua_State *L);
#line 176 "nbib.nw"
bool is_id_char[256];    /* needs initialization */
#define concat_char '#'  /* used to concatenate parts of a field defn */
#line 1002 "nbib.nw"
static const struct luaL_reg bibtexlib [] = {
  {"open", openreader},
  {"close", closereader},
  {"next", next_entry},
  {NULL, NULL}
};
#line 275 "nbib.nw"
#define LERRPUSH(S) do { \
  if (!lua_checkstack(rdr->L, 10)) assert(0); \
  lua_pushboolean(rdr->L, 0); \
  lua_pushfstring(rdr->L, "%s, line %d: ", rdr->filename, rdr->line_num); \
  lua_pushstring(rdr->L, S); \
  lua_concat(rdr->L, 2); \
  } while(0)
#define LERRFPUSH(S,A) do { \
  if (!lua_checkstack(rdr->L, 10)) assert(0); \
  lua_pushboolean(rdr->L, 0); \
  lua_pushfstring(rdr->L, "%s, line %d: ", rdr->filename, rdr->line_num); \
  lua_pushfstring(rdr->L, S, A); \
  lua_concat(rdr->L, 2); \
  } while(0)
#define LERR(S)   do { LERRPUSH(S);   return 2; } while(0)
#define LERRF(S,A) do { LERRFPUSH(S,A); return 2; } while(0)
  /* next: cases for Boolean functions */
#define LERRB(S)   do { LERRPUSH(S);   return 0; } while(0)
#define LERRFB(S,A) do { LERRFPUSH(S,A); return 0; } while(0)
#line 462 "nbib.nw"
#undef ready_tok
#define ready_tok(RDR) do { \
  if (!upto_nonwhite_getline(RDR)) \
    LERRB("Unexpected end of file"); \
  } while(0)
#line 474 "nbib.nw"
#define copy_char(C) luaL_putchar(b, (C))
#line 481 "nbib.nw"
static bool scan_and_push_the_field_value (Bibreader rdr, int key) {
  luaL_Buffer field;

  luaL_checkstack(rdr->L, 10, "Not enough Lua stack to parse bibtex database");
  luaL_buffinit(rdr->L, &field);
  for (;;) {
    if (!scan_and_buffer_a_field_token(rdr, key, &field)) 
      return 0;
    ready_tok(rdr); /* cur now points to [[concat_char]] or end of field */
    if (*rdr->cur != concat_char) break;
    else { rdr->cur++; ready_tok(rdr); }
  }
  luaL_pushresult(&field);
  return 1;
}
#line 513 "nbib.nw"
static bool scan_and_buffer_a_field_token (Bibreader rdr, int key, luaL_Buffer *b) {
  unsigned char *p;
  unsigned number;
  *rdr->lim = ' ';
  switch (*rdr->cur) {
    case '{': case '"':
      return scan_balanced_braces(rdr, *rdr->cur == '{' ? '}' : '"', b);
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9': 
      p = rdr->cur;
      scan_nonneg_integer(rdr, &number);
      luaL_addlstring(b, (char *)p, rdr->cur - p);
      return 1;
   default:
      /* named macro */
      p = rdr->cur;
      if (!scan_identifier(rdr, ',', rdr->entry_close, concat_char))
        LERRB("Expected a field part");
      lower_case (p, rdr->cur);   /* ignore case differences */
        /* missing warning of macro name used in its own definition */
      lua_pushlstring(rdr->L, (char *) p, rdr->cur - p); /* stack: name */
      lua_getref(rdr->L, rdr->macros);                   /* stack: name macros */
      lua_insert(rdr->L, -2);                            /* stack: name macros name */
      lua_gettable(rdr->L, -2);                          /* stack: name defn */
      lua_remove(rdr->L, -2);                            /* stack: defn */
      
#line 547 "nbib.nw"
{ int t = lua_gettop(rdr->L);
  if (lua_isnil(rdr->L, -1)) {
    lua_pop(rdr->L, 1);
    lua_pushlstring(rdr->L, (char *) p, rdr->cur - p);
    warnv(rdr, 1, "ssdss", /* tag, file, line, key, macro */
          "undefined macro", rdr->filename, rdr->line_num,
          key ? lua_tostring(rdr->L, key) : NULL, lua_tostring(rdr->L, -1));
    if (lua_isstring(rdr->L, -1))
      luaL_addvalue(b);
    else
      lua_pop(rdr->L, 1);
    lua_pop(rdr->L, 1);
  } else {
    luaL_addvalue(b);
  }
  assert(lua_gettop(rdr->L) == t-1);
}
#line 539 "nbib.nw"
      return 1;
  }
}
#line 573 "nbib.nw"
static int scan_balanced_braces(Bibreader rdr, char close, luaL_Buffer *b) {
  unsigned char *p, *cur, c;
  int braces = 0;  /* number of currently open braces *inside* string */

  rdr->cur++;   /* scan past left delimiter */
  *rdr->lim = ' ';
  if (isspace(*rdr->cur)) {
    copy_char(' ');
    ready_tok(rdr);
  }
  for (;;) {
    p = rdr->cur;
    upto_white_or_3(rdr, '}', '{', close);
    cur = rdr->cur;
    for ( ; p < cur; p++) /* copy nonwhite, nonbrace characters */
      copy_char(*p);
    *rdr->lim = ' ';
    c = *cur;  /* will be whitespace if at end of line */
    
#line 606 "nbib.nw"
if (isspace(c)) {
  copy_char(' ');
  ready_tok(rdr);
} else {
  rdr->cur++;
  if (c == close) {
    if (braces == 0) {
      luaL_pushresult(b);
      return 1;
    } else {
      copy_char(c);
      if (c == '}')
        braces--;
    }
  } else if (c == '{') {
    braces++;
    copy_char(c);
  } else {
    assert(c == '}');
    if (braces > 0) {
      braces--;
      copy_char(c);
    } else {
      luaL_pushresult(b); /* restore invariant */
      LERRB("Unexpected '}'");
    }
  }
}
#line 592 "nbib.nw"
  }
}
#line 755 "nbib.nw"
static int scan_identifier (Bibreader rdr, char c1, char c2, char c3) {
  unsigned char *p, *orig, c;

  orig = p = rdr->cur;
  if (!isdigit(*p)) {
    /* scan until end-of-line or an [[illegal_id_char]] */
    *rdr->lim = ' ';  /* an illegal id character and also white space */
    while (is_id_char[*p])
      p++;
  }
  c = *p;
  if (p > rdr->cur && (isspace(c) || c == c1 || c == c2 || c == c3)) {
    rdr->cur = p;
    return 1;
  } else {
    return 0;
  }
}
#line 780 "nbib.nw"
static bool scan_nonneg_integer (Bibreader rdr, unsigned *np) {
  unsigned char *p = rdr->cur;
  unsigned n = 0;
  *rdr->lim = ' ';   /* sentinel */
  while (isdigit(*p)) {
    n = n * 10 + (*p - '0');
    p++;
  }
  if (p == rdr->cur)
    return 0; /* no digits */
  else {
    rdr->cur = p;
    *np = n;
    return 1;
  }
}
#line 318 "nbib.nw"
#undef ready_tok
#define ready_tok(RDR) do { \
  if (!upto_nonwhite_getline(RDR)) \
    LERR("Unexpected end of file"); \
  } while(0)

static int get_bib_command_or_entry_and_process(Bibreader rdr) {
  unsigned char *id, *key;
  int keyindex;
  bool (*command)(Bibreader);
 getnext:
  
#line 358 "nbib.nw"
if (!upto1_getline(rdr, '@'))
  return 0;  /* no more entries; return nil */
assert(*rdr->cur == '@');
rdr->cur++;   /* skip the @ sign */
ready_tok(rdr);

#line 331 "nbib.nw"
  id = rdr->cur;
  if (!scan_identifier (rdr, '{', '(', '('))
    LERR("Expected an entry type");
  lower_case (id, rdr->cur);       /* ignore case differences */
  
#line 365 "nbib.nw"
command = find_command(id, rdr->cur);
if (command) {
  if (!command(rdr))
    return 2; /* command put (false, message) on Lua stack; we're done */
  goto getnext;
}

#line 337 "nbib.nw"
  lua_pushlstring(rdr->L, (char *) id, rdr->cur - id);    /* push entry type */
  rdr->entry_line = rdr->line_num;
  ready_tok(rdr);
  
#line 376 "nbib.nw"
if (*rdr->cur == '{') 
    rdr->entry_close = '}';
else if (*rdr->cur == '(') 
    rdr->entry_close = ')';
else
    LERR("Expected entry to open with { or (");
rdr->cur++;
#line 341 "nbib.nw"
  ready_tok(rdr);
  key = rdr->cur;
  
#line 387 "nbib.nw"
if (rdr->entry_close == '}') {
  upto_white_or_1(rdr, ',');
} else {
  upto_white_or_2(rdr, ',', '}');
}
#line 344 "nbib.nw"
  lua_pushlstring(rdr->L, (char *) key, rdr->cur - key);  /* push database key */
  keyindex = lua_gettop(rdr->L);
  lua_newtable(rdr->L);                                   /* push table of fields */
  ready_tok(rdr);
  for (; *rdr->cur != rdr->entry_close; ) {
    
#line 399 "nbib.nw"
if (*rdr->cur == ',') {
  rdr->cur++;
  ready_tok(rdr);
  if (*rdr->cur == rdr->entry_close) {
    break;
  }
} else {
  LERR("Expected comma or end of entry");
}
#line 350 "nbib.nw"
    
#line 412 "nbib.nw"
if (id = rdr->cur, !scan_identifier (rdr, '=', '=', '='))
  LERR("Expected a field name");
lower_case(id, rdr->cur);
lua_pushlstring(rdr->L, (char *) id, rdr->cur - id);  /* push field name */
ready_tok(rdr);
if (*rdr->cur != '=')
  LERR("Expected '=' to follow field name");
rdr->cur++;                    /* skip over the [['=']] */
ready_tok(rdr);
if (!scan_and_push_the_field_value(rdr, keyindex))
  return 2;
strip_leading_and_trailing_space(rdr->L);
#line 440 "nbib.nw"
lua_pushvalue(rdr->L, -2);  /* push key */
lua_gettable(rdr->L, -4);
if (lua_isnil(rdr->L, -1)) {
  lua_pop(rdr->L, 1);
  lua_settable(rdr->L, -3);
} else {
  lua_pop(rdr->L, 1);  /* off comes old value  */
  warnv(rdr, 0, "ssdsss", /* tag, file, line, cite-key, field, newvalue */
        "extra field", rdr->filename, rdr->line_num,
        lua_tostring(rdr->L, keyindex),
        lua_tostring(rdr->L, -2), lua_tostring(rdr->L, -1));
  lua_pop(rdr->L, 2);  /* off come key and new value */
}
#line 351 "nbib.nw"
    ready_tok(rdr);
  }
  rdr->cur++;  /* skip past close of entry */
  return 3; /* entry type, key, table of fields */
}
#line 639 "nbib.nw"
static bool upto1(Bibreader rdr, char c) {
  unsigned char *p = rdr->cur;
  unsigned char *lim = rdr->lim;
  *lim = c;
  while (*p != c)
    p++;
  rdr->cur = p;
  return p < lim;
}
#line 652 "nbib.nw"
static int upto1_getline(Bibreader rdr, char c) {
  while (!upto1(rdr, c))
    if (!getline(rdr))
      return 0;
  return 1;
}
#line 662 "nbib.nw"
static void upto_white_or_1(Bibreader rdr, char c) {
  unsigned char *p = rdr->cur;
  unsigned char *lim = rdr->lim;
  *lim = c;
  while (*p != c && !isspace(*p))
    p++;
  rdr->cur = p;
}
#line 673 "nbib.nw"
static void upto_white_or_2(Bibreader rdr, char c1, char c2) {
  unsigned char *p = rdr->cur;
  unsigned char *lim = rdr->lim;
  *lim = c1;
  while (*p != c1 && *p != c2 && !isspace(*p))
    p++;
  rdr->cur = p;
}
#line 684 "nbib.nw"
static void upto_white_or_3(Bibreader rdr, char c1, char c2, char c3) {
  unsigned char *p = rdr->cur;
  unsigned char *lim = rdr->lim;
  *lim = c1;
  while (!isspace(*p) && *p != c1 && *p != c2 && *p != c3)
    p++;
  rdr->cur = p;
}
#line 697 "nbib.nw"
static bool upto_nonwhite(Bibreader rdr) {
  unsigned char *p = rdr->cur;
  unsigned char *lim = rdr->lim;
  *lim = 'x';
  while (isspace(*p))
    p++;
  rdr->cur = p;
  return p < lim;
}
#line 710 "nbib.nw"
static int upto_nonwhite_getline(Bibreader rdr) {
  while (!upto_nonwhite(rdr))
    if (!getline(rdr))
      return 0;
  return 1;
}
#line 719 "nbib.nw"
static bool getline(Bibreader rdr) {
  char *result;
  unsigned char *buf = rdr->buf;
  int n;
  result = fgets((char *)buf, rdr->bufsize, rdr->file);
  if (result == NULL)
    return 0;
  rdr->line_num++;
  for (n = strlen((char *)buf); buf[n-1] != '\n'; n = strlen((char *)buf)) {
    /* failed to get whole line */
    rdr->bufsize *= 2;
    buf = rdr->buf = realloc(rdr->buf, rdr->bufsize);
    assert(buf);
    if (fgets((char *)buf+n,rdr->bufsize-n,rdr->file)==NULL) {
      n = strlen((char *)buf) + 1;  /* -1 below is incorrect without newline */
      break; /* file ended without a newline */
    }
  }
  rdr->cur = buf;
  rdr->lim = buf+n-1;  /* trailing newline not in string */
  return 1; 
}
#line 826 "nbib.nw"
static void lower_case(unsigned char *p, unsigned char *lim) {
  for (; p < lim; p++)
    *p = tolower(*p);
}
#line 832 "nbib.nw"
static void strip_leading_and_trailing_space(lua_State *L) {
  const char *p;
  int n;
  assert(lua_isstring(L, -1));
  p = lua_tostring(L, -1);
  n = lua_strlen(L, -1);
  if (n > 0 && (isspace(*p) || isspace(p[n-1]))) {
    while(n > 0 && isspace(*p))
      p++, n--;
    while(n > 0 && isspace(p[n-1]))
      n--;
    lua_pushlstring(L, p, n);
    lua_remove(L, -2);
  }
}
#line 853 "nbib.nw"
static Command find_command(unsigned char *p, unsigned char *lim) {
  int n = lim - p;
  assert(lim > p);
#define match(S) (!strncmp(S, (char *)p, n) && (S)[n] == '\0')
  switch(*p) {
    case 'c' : if (match("comment"))  return do_comment;  else break;
    case 'p' : if (match("preamble")) return do_preamble; else break;
    case 's' : if (match("string"))   return do_string;   else break;
  }
  return (Command)0;
}
#line 870 "nbib.nw"
static bool do_comment(Bibreader rdr) {
  return 1;
}
#line 892 "nbib.nw"
static bool do_preamble(Bibreader rdr) {
  ready_tok(rdr);
  
#line 376 "nbib.nw"
if (*rdr->cur == '{') 
    rdr->entry_close = '}';
else if (*rdr->cur == '(') 
    rdr->entry_close = ')';
else
    LERR("Expected entry to open with { or (");
rdr->cur++;
#line 895 "nbib.nw"
  ready_tok(rdr);
  lua_rawgeti(rdr->L, LUA_REGISTRYINDEX, rdr->preamble);
  lua_pushnumber(rdr->L, luaL_getn(rdr->L, -1) + 1);
  if (!scan_and_push_the_field_value(rdr, 0))
    return 0;
  ready_tok(rdr);
  if (*rdr->cur != rdr->entry_close)
    LERRFB("Missing '%c' in preamble command", rdr->entry_close);
  rdr->cur++;
  lua_settable(rdr->L, -3);
  lua_pop(rdr->L, 1); /* remove preamble */
  return 1;
}
#line 931 "nbib.nw"
static bool do_string(Bibreader rdr) {
  unsigned char *id;
  int keyindex;
  ready_tok(rdr);
  
#line 376 "nbib.nw"
if (*rdr->cur == '{') 
    rdr->entry_close = '}';
else if (*rdr->cur == '(') 
    rdr->entry_close = ')';
else
    LERR("Expected entry to open with { or (");
rdr->cur++;
#line 936 "nbib.nw"
  ready_tok(rdr);
  id = rdr->cur;
  if (!scan_identifier(rdr, '=', '=', '='))
    LERRB("Expected a string name followed by '='");
  lower_case(id, rdr->cur);
  lua_pushlstring(rdr->L, (char *)id, rdr->cur - id);
  keyindex = lua_gettop(rdr->L);
  ready_tok(rdr);
  if (*rdr->cur != '=')
    LERRB("Expected a string name followed by '='");
  rdr->cur++;
  ready_tok(rdr);
  if (!scan_and_push_the_field_value(rdr, keyindex))
    return 0;
  ready_tok(rdr);
  if (*rdr->cur != rdr->entry_close)
    LERRFB("Missing '%c' in macro definition", rdr->entry_close);
  rdr->cur++;
  lua_getref(rdr->L, rdr->macros);
  lua_insert(rdr->L, -3);
  lua_settable(rdr->L, -3);
  lua_pop(rdr->L, 1);
  return 1;
}
#line 965 "nbib.nw"
static Bibreader checkreader(lua_State *L, int index) {
  Bibreader rdr = luaL_checkudata(L, index, "bibtex.reader");
  luaL_argcheck(L, rdr != NULL, index, "bibtex reader expected");
  return rdr;
}
#line 977 "nbib.nw"
static int reader_meta_index(lua_State *L) {
  Bibreader rdr = checkreader(L, 1);
  const char *key;
  if (!lua_isstring(L, 2))
    return 0;
  key = lua_tostring(L, 2);
  if (!strcmp(key, "next"))
    lua_pushcfunction(L, next_entry);
  else if (!strcmp(key, "entry_line"))
    lua_pushnumber(L, rdr->entry_line);
  else if (!strcmp(key, "preamble"))
    lua_rawgeti(L, LUA_REGISTRYINDEX, rdr->preamble);
  else if (!strcmp(key, "close"))
    lua_pushcfunction(L, closereader);
  else
    lua_pushnil(L);
  return 1;
}
#line 1032 "nbib.nw"
#define INBUF 128  /* initial size of input buffer */
/* filename * macro table * warning function -> reader */
static int openreader(lua_State *L) {
  const char *filename = luaL_checkstring(L, 1);
  FILE *f = fopen(filename, "r");
  Bibreader rdr;
  if (!f) {
    lua_pushnil(L);
    lua_pushfstring(L, "Could not open file '%s'", filename);
    return 2;
  }

  
#line 1068 "nbib.nw"
if (lua_type(L, 2) == LUA_TNONE)
  lua_newtable(L);

if (lua_type(L, 3) == LUA_TNONE)
  lua_pushnil(L);
else if (!lua_isfunction(L, 3))
  luaL_error(L, "Warning value to bibtex.open is not a function");

#line 1046 "nbib.nw"
  rdr = lua_newuserdata(L, sizeof(*rdr));
  luaL_getmetatable(L, "bibtex.reader");
  lua_setmetatable(L, -2);

  rdr->line_num = 0;
  rdr->buf = rdr->cur = rdr->lim = malloc(INBUF);
  rdr->bufsize = INBUF;
  rdr->file = f;
  rdr->filename = malloc(lua_strlen(L, 1)+1);
  assert(rdr->filename);
  strncpy((char *)rdr->filename, filename, lua_strlen(L, 1)+1);
  rdr->L = L;
  lua_newtable(L);
  rdr->preamble = luaL_ref(L, LUA_REGISTRYINDEX);
  lua_pushvalue(L, 2);
  rdr->macros = luaL_ref(L, LUA_REGISTRYINDEX);
  lua_pushvalue(L, 3);
  rdr->warning = luaL_ref(L, LUA_REGISTRYINDEX);
  return 1;
}
#line 1082 "nbib.nw"
static int next_entry(lua_State *L) {
  Bibreader rdr = checkreader(L, 1);
  if (!rdr->file)
    luaL_error(L, "Tried to read from closed bibtex.reader");
  return get_bib_command_or_entry_and_process(rdr);
}  
#line 1092 "nbib.nw"
static int closereader(lua_State *L) {
  Bibreader rdr = checkreader(L, 1);
  if (!rdr->file)
    luaL_error(L, "Tried to close closed bibtex.reader");
  fclose(rdr->file);
  rdr->file = NULL;
  free(rdr->buf);
  rdr->buf = rdr->cur = rdr->lim = NULL;
  rdr->bufsize = 0;
  free((void*)rdr->filename);
  rdr->filename = NULL;
  rdr->L = NULL;
  luaL_unref(L, LUA_REGISTRYINDEX, rdr->preamble);
  rdr->preamble = 0;
  luaL_unref(L, LUA_REGISTRYINDEX, rdr->warning);
  rdr->warning = 0;
  luaL_unref(L, LUA_REGISTRYINDEX, rdr->macros);
  rdr->macros = 0;
  return 0;
}  
#line 1116 "nbib.nw"
static void warnv(Bibreader rdr, int nres, const char *fmt, ...) {
  const char *p;
  va_list vl;
    
  lua_rawgeti(rdr->L, LUA_REGISTRYINDEX, rdr->warning);
  if (lua_isnil(rdr->L, -1)) {
    lua_pop(rdr->L, 1);
    while (nres-- > 0)
      lua_pushnil(rdr->L);
  } else {
    va_start(vl, fmt);
    for (p = fmt; *p; p++)
      switch (*p) {
        case 'f': lua_pushnumber(rdr->L, va_arg(vl, double)); break;
        case 'd': lua_pushnumber(rdr->L, va_arg(vl, int)); break;
        case 's': {
          const char *s = va_arg(vl, char *);
          if (s == NULL) lua_pushnil(rdr->L);
          else lua_pushstring(rdr->L, s);
          break;
        }
        default: luaL_error(rdr->L, "invalid parameter type %c", *p);
      }
    lua_call(rdr->L, p - fmt, nres);
    va_end(vl);
  }
}
#line 1147 "nbib.nw"
int luaopen_bibtex (lua_State *L) {
  luaL_newmetatable(L, "bibtex.reader");
  lua_pushstring(L, "__index");
  lua_pushcfunction(L, reader_meta_index);  /* pushes the index method */
  lua_settable(L, -3);  /* metatable.__index = metatable */

  luaL_openlib(L, "bibtex", bibtexlib, 0);
  
#line 1161 "nbib.nw"
{
  unsigned c;
  static unsigned char *nonids = (unsigned char *)"\"#%'(),={} \t\n\f";
  unsigned char *p;

  for (c = 0; c <= 0377; c++)
    is_id_char[c] = 1;
  for (c = 0; c <= 037; c++)
    is_id_char[c] = 0;
  for (p = nonids; *p; p++)
    is_id_char[*p] = 0;
}
#line 1155 "nbib.nw"
  return 1;
}

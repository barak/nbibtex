local config = { nbs = "" }
-- From here out, everything is written in Lua
-- (http://www.lua.org). The main module is [[bibtex]],
-- and style-file support is in the submodule
-- [[bibtex.bst]]. Each has a [[doc]] submodule, which is
-- intended as machine-readable documentation.
-- <bibtex.lua>=
-- The Lua code relies on the C code. How we get the
-- C code depends on how bibtex.lua is used; there are
-- two alternatives:
--   * In the distribution, bibtex.lua is loaded by the
--     C code in chunk [->], which defines the [[bibtex]]
--     module.
--   * For standalone testing purposes, bibtex.lua can be
--     loaded directly into an interactive Lua
--     interpreter, in which case it loads the [[bibtex]]
--     module as a shared library.
-- <if not already present, load the C code for the [[bibtex]] module>=
if not bibtex then
  require 'nrlib'
  nrlib.load 'bibtex'
end

local config = config or { } --- may be defined by config process

local workaround = {
  badbibs = true,  --- don't look at bad .bib files that come with teTeX
}
local bst = { } 
bibtex.bst = bst

bibtex.doc = { }
bibtex.bst.doc = { }

bibtex.doc.bst = '# table of functions used to write style files'
-- Not much code is executed during startup, so the main
-- issue is to manage declaration before use. I have a
-- few forward declarations in [[<<declarations of
-- internal functions>>]]; otherwise, count only on
-- ``utility'' functions being declared before
-- ``exported'' ones.
-- <bibtex.lua>=
local find = string.find
-- <declarations of internal functions>=
local matchq
bibtex.doc.matchq = 'matchq: string -> predicate --- compile query string'
--   Error handling, warning messages, and logging
-- 
-- <Lua utility functions>=


------------------------------------------------------------------------
---------- Utility functions for "Implementation of nbibtex" -----------
------------------------------------------------------------------------

local function printf (...) return io.stdout:write(string.format(unpack(arg))) end
local function eprintf(...) return io.stderr:write(string.format(unpack(arg))) end
-- I have to figure out what to do about errors --- the
-- current code is bogus. Among other things, I should be
-- setting error levels.
-- <Lua utility functions>=
local function bibwarnf (...) eprintf(unpack(arg)); eprintf('\n') end
local function biberrorf(...) eprintf(unpack(arg)); eprintf('\n') end
local function bibfatalf(...) eprintf(unpack(arg)); eprintf('\n'); os.exit(2) end
-- Logging? What logging?
-- 
-- <Lua utility functions>=
local function logf() end
--  Support for delayed warnings
-- 
-- Like classic BibTeX, NbibTeX typically warns only
-- about entries that are actually used. This
-- functionality is implemented by function
-- [[hold_warning]], which keeps warnings on ice until
-- they are either returned by [[held_warnings]] or
-- thrown away by [[drop_warning]]. The function
-- [[emit_warning]] emits a warning message eagerly when
-- called; it is used to issue warnings about entries we
-- actually use, or if the [[-strict]] option is given,
-- to issue every warning.
-- <Lua utility functions>=
local hold_warning  -- function suitable to pass to bibtex.open; holds
local emit_warning  -- function suitable to pass to bibtex.open; prints
local held_warnings -- returns nil or list of warnings since last call
local drop_warnings -- drops warnings

local extra_ok = { reffrom = true }
   -- set of fields about which we should not warn of duplicates

do
  local warnfuns = { }
  warnfuns["extra field"] =
    function(file, line, cite, field, newvalue)
      if not extra_ok[field] then 
        bibwarnf("Warning--I'm ignoring %s's extra \"%s\" field\n--line %d of file %s\n",
                 cite, field, line, file)
      end
    end

  warnfuns["undefined macro"] =
    function(file, line, cite, macro)
      bibwarnf("Warning--string name \"%s\" is undefined\n--line %d of file %s\n",
               macro, line, file)
    end

  function emit_warning(tag, ...)
    return assert(warnfuns[tag])(unpack(arg))
  end

  local held
  function hold_warning(...)
    held = held or { }
    table.insert(held, arg)
  end
  function held_warnings()
    local h = held
    held = nil
    return h
  end
  function drop_warnings()
    held = nil
  end
end
--   Miscellany
-- 
-- All this stuff is dubious.
-- <Lua utility functions>=
function table.copy(t)
  local u = { }
  for k, v in pairs(t) do u[k] = v end
  return u
end
-- <Lua utility functions>=
local function open(f, m, what)
  local f, msg = io.open(f, m)
  if f then
    return f
  else
    (what or bibfatalf)('Could not open file %s: %s', f, msg)
  end
end
-- <Lua utility functions>=
local function help(code)
  printf([[
Usage: nbibtex [OPTION]... AUXFILE[.aux] [BIBFILE...]
  Write bibliography for entries in AUXFILE to AUXFILE.bbl.

Options:
  -bib                   write output as BibTeX source
  -help                  display this help and exit
  -o FILE                write output to FILE (- for stdout)
  -min-crossrefs=NUMBER  include item after NUMBER cross-refs; default 2
  -permissive            allow missing bibfiles and (some) duplicate entries
  -strict                complain about any ill-formed entry we see
  -version               output version information and exit

Home page at http://www.eecs.harvard.edu/~nr/nbibtex.
Email bug reports to nr@eecs.harvard.edu.
]])
  os.exit(code or 0)
end
-- <Lua utility functions>=
-- return 'key' or 'type' or 'field <name>' at which entries differ,
-- or nil if entries are the same
local function entries_differ(e1, e2, notkey)
  if e1.key  ~= e2.key  and not notkey then return 'key'  end
  if e1.type ~= e2.type                then return 'type' end
  for k, v in pairs(e1.fields) do
    if e2.fields[k] ~= v then return 'field ' .. k end
  end
  for k, v in pairs(e2.fields) do
    if e1.fields[k] ~= v then return 'field ' .. k end
  end
end
-- I've seen at least one bibliography with identical
-- entries listed under multiple keys. (Thanks, Andrew.)
-- <Lua utility functions>=
-- every entry is identical to every other
local function all_entries_identical(es, notkey)
  if table.getn(es) == 0 then return true end
  for i = 2, table.getn(es) do
    if entries_differ(es[1], es[i], notkey) then
      return false
    end
  end
  return true
end
--   Path search and other system-dependent stuff
-- 
-- To find a bib file, I rely on the kpsewhich program,
-- which is typically found on Unix TeX installations,
-- and which should guarantee to find the same bib files
-- as normal bibtex.
-- <Lua utility functions>=
assert(io.popen)
local function capture(cmd, raw)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  assert(f:close())  --- can't get an exit code
  if raw then return s end
  s = string.gsub(s, '^%s+', '')
  s = string.gsub(s, '%s+$', '')
  s = string.gsub(s, '[\n\r]+', ' ')
  return s
end
--   Output formats for BibTeX entries
-- 
-- We can emit a BibTeX entry in any of three formats:
-- [[bib]], [[terse]], and [[full]]. An emitter takes as
-- arguments the type, key, and fields of the entry, and
-- optionally the name of the file the entry came from.
-- <Lua utility functions>=


------------------------------------------------------------------------
---------- Utility functions for "Implementation of nbibfind" ----------
------------------------------------------------------------------------

local emit_tkf = { }
-- Function [[truncate]] returns enough of a string to
-- fit in [[n]] columns, with ellipses as needed.
-- <Lua utility functions>=
local function truncate(s, n)
  local l = string.len(s)
  if l <= n then
    return s
  else
    return string.sub(s, 1, n-3) .. '...'
  end
end
-- To search all bib files, we lean heavily on kpsewhich,
-- which is distributed with the Web2C version of TeX,
-- and which knows exactly which directories to search.
-- <Lua utility functions>=
local readable -- is a file readable?
local function all_bibs()
  local pre_path = assert(capture('kpsewhich -show-path bib'))
  local path = assert(capture('kpsewhich -expand-path ' .. pre_path))
  local bibs = { } -- list of results
  local inserted = { } -- set of inserted bibs, to avoid duplicates
  for _, dir in ipairs(split(path, ':')) do
    local files = assert(capture('echo ' .. dir .. '/*.bib'))
    for _, file in ipairs(split(files, '%s')) do
      if readable(file) then
        if not (workaround.badbibs and (find(file, 'amsxport%-options') or
                                        find(file, '/plbib%.bib$')))
        then
          if not inserted[file] then
            table.insert(bibs, file)
            inserted[file] = true
          end
        end
      end
    end
  end
  return bibs
end
bibtex.all_bibs = all_bibs
-- [[workaround.badbibs]], which prevents us
-- from searching some bogus bibfiles that come with
-- Thomas Esser's teTeX.

-- It's a pity there's no more efficient way to see if a
-- file is readable than to try to read it, but that's
-- portability for you.
-- <Lua utility functions>=
function readable(file)
  local f, msg = io.open(file, 'r')
  if f then
    f:close()
    return true
  else
    return false, msg
  end
end
--   Special string-processing support
-- 
-- A great deal of BibTeX's processing depends on giving
-- a special status to substrings inside braces; indeed,
-- when such a substring begins with a backslash, it is
-- called a ``special character.'' Accordingly, we
-- provide a function to search for a pattern outside
-- balanced braces.
-- <Lua utility functions>=


------------------------------------------------------------------------
----------- Utility functions for "Support for style files" ------------
------------------------------------------------------------------------

local function find_outside_braces(s, pat, i)
  local len = string.len(s)
  local j, k = string.find(s, pat, i)
  if not j then return j, k end
  local jb, kb = string.find(s, '%b{}', i)
  while jb and jb < j do --- scan past braces
    --- braces come first, so we search again after close brace
    local i2 = kb + 1
    j, k = string.find(s, pat, i2)
    if not j then return j, k end
    jb, kb = string.find(s, '%b{}', i2)
  end
  -- either pat precedes braces or there are no braces
  return string.find(s, pat, j) --- 2nd call needed to get captures
end
--  String splitting
-- 
-- Another common theme in BibTeX is the list represented
-- as string. A list of names is represented as a string
-- with individual names separated by ``and.'' A name
-- itself is a list of parts separated by whitespace. So
-- here are some functions to do general splitting. When
-- we don't care about the separators, we use [[split]];
-- when we care only about the separators, we use
-- [[splitters]]; and when we care about both, we use
-- [[odd_even_split]].
-- <Lua utility functions>=
local function split(s, pat, find) --- return list of substrings separated by pat
  find = find or string.find -- could be find_outside_braces
  local len = string.len(s)
  local t = { }
  local insert = table.insert
  local i, j, k = 1, true
  while j and i <= len + 1 do
    j, k = find(s, pat, i)
    if j then
      insert(t, string.sub(s, i, j-1))
      i = k + 1
    else
      insert(t, string.sub(s, i))
    end
  end
  return t
end
-- Function [[splitters]] returns a table that, when
-- interleaved with the result of [[split]], reconstructs
-- the original string.
-- <Lua utility functions>=
local function splitters(s, pat, find) --- return list of separators
  find = find or string.find -- could be find_outside_braces
  local t = { }
  local insert = table.insert
  local j, k = find(s, pat, 1)
  while j do
    insert(t, string.sub(s, j, k))
    j, k = find(s, pat, k+1)
  end
  return t
end
-- Function [[odd_even_split]] makes odd entries strings
-- between the sought-for pattern and even entries the
-- strings that match the pattern.
-- <Lua utility functions>=
local function odd_even_split(s, pat)
  local len = string.len(s)
  local t = { }
  local insert = table.insert
  local i, j, k = 1, true
  while j and i <= len + 1 do
    j, k = find(s, pat, i)
    if j then
      insert(t, string.sub(s, i, j-1))
      insert(t, string.sub(s, j, k))
      i = k + 1
    else
      insert(t, string.sub(s, i))
    end
  end
  return t
end
-- As a special case, we may want to pull out
-- brace-delimited substrings:
-- <Lua utility functions>=
local function brace_split(s) return odd_even_split(s, '%b{}') end
--  String lengths and widths
-- 
-- Function [[text_char_count]] counts characters, but a
-- special counts as one character. It is based on
-- BibTeX's [[text.length]] function. 
-- <Lua utility functions>=
local function text_char_count(s)
  local n = 0
  local i, last = 1, string.len(s)
  while i <= last do
    local special, splast, sp = find(s, '(%b{})', i)
    if not special then
      return n + (last - i + 1)
    elseif find(sp, '^{\\') then
      n = n + (special - i + 1) -- by statute, it's a single character
      i = splast + 1
    else
      n = n + (splast - i + 1) - 2  -- don't count braces
      i = splast + 1
    end
  end
  return n
end
bst.text_length = text_char_count
bst.doc.text_length = "string -> int # length (with 'special' char == 1)"
-- The von name starts with the first token satisfying
-- [[isVon]], unless that is the last token. A ``von
-- token'' is simply one that begins with a lower-case
-- letter---but those damn specials complicate
-- everything.
-- <Lua utility functions>=
local upper_specials = { OE = true, AE = true, AA = true, O = true, L = true }
local lower_specials = { i = true, j = true, oe = true, ae = true, aa = true,
                         o = true, l = true, ss = true }
-- <exported Lua functions>=


------------------------------------------------------------------------
---------------------- IMPLEMENTATION OF NBIBTEX -----------------------
------------------------------------------------------------------------

local function entries(rdr, empty)
  assert(not empty)
  return function() return rdr:next() end
end

bibtex.entries = entries
bibtex.doc.entries = 'reader -> iterator   # generate entries'
--   Internal documentation
-- 
-- We attempt to document everything!
-- <exported Lua functions>=
function bibtex:show_doc(title)
  local out = bst.writer(io.stdout, 5)
  local function outf(...) return out:write(string.format(unpack(arg))) end
  local allkeys, dkeys = { }, { }
  for k, _ in pairs(self)     do table.insert(allkeys, k) end
  for k, _ in pairs(self.doc) do table.insert(dkeys,   k) end
  table.sort(allkeys)
  table.sort(dkeys)
  for i = 1, table.getn(dkeys) do
    outf("%s.%-12s : %s\n", title, dkeys[i], self.doc[dkeys[i]])
  end
  local header
  for i = 1, table.getn(allkeys) do
    local k = allkeys[i]
    if k ~= "doc" and k ~= "show_doc" and not self.doc[k] then
      if not header then
        outf('Undocumented keys in table %s:', title)
        header = true
      end
      outf(' %s', k)
    end
  end
  if header then outf('\n') end
end
bibtex.bst.show_doc = bibtex.show_doc
-- Here is the documentation for what's defined in
-- C code:
-- <exported Lua functions>=
bibtex.doc.open  = 'filename -> reader # open a reader for a .bib file'
bibtex.doc.close = 'reader -> unit     # close open reader'
bibtex.doc.next  = 'reader -> type * key * field table # read an entry'
-- <exported Lua functions>=
bibtex.doc.main = 'string list -> unit # main program that dispatches on argv[0]'
function bibtex.main(argv)
  if argv[1] == '-doc' then -- undocumented internal doco
    bibtex:show_doc('bibtex')
    bibtex.bst:show_doc('bst')
  elseif find(argv[0], 'bibfind$') then
    return bibtex.run_find(argv)
  elseif find(argv[0], 'bibtex$') then
    return bibtex.bibtex(argv)
  else
    error("Call me something ending in 'bibtex' or 'bibfind'; when called\n  "..
          argv[0]..", I don't know what to do")
  end
end
-- <exported Lua functions>=
local permissive    = false -- nbibtex extension (ignore missing .bib files, etc.)
local strict        = false -- complain eagerly about errors in .bib files
local min_crossrefs = 2     -- how many crossref's required to add an entry?
local output_name   = nil   -- output file if not default
local bib_out       = false -- output .bib format

bibtex.doc.bibtex = 'string list -> unit # main program for nbibtex'
function bibtex.bibtex(argv)
  -- Options are straightforward.
  -- 
  -- <set bibtex options from [[argv]]>=
  while table.getn(argv) > 0 and find(argv[1], '^%-') do
    if argv[1] == '-terse' then
      -- do nothing
    elseif argv[1] == '-permissive' then
      permissive = true
    elseif argv[1] == '-strict' then
      strict = true
    elseif argv[1] == '-min-crossrefs' and find(argv[2], '^%d+$') then
      min_crossrefs = assert(tonumber(argv[2]))
      table.remove(argv, 1)
    elseif string.find(argv[1], '^%-min%-crossrefs=(%d+)$') then
      local _, _, n = string.find(argv[1], '^%-min%-crossrefs=(%d+)$')
      min_crossrefs = assert(tonumber(n))
    elseif string.find(argv[1], '^%-min%-crossrefs') then
      biberrorf("Ill-formed option %s", argv[1])
    elseif argv[1] == '-o' then
      output_name = assert(argv[2])
      table.remove(argv, 1)
    elseif argv[1] == '-bib' then
      bib_out = true
    elseif argv[1] == '-help' then
      help()
    elseif argv[1] == '-version' then
      printf("nbibtex version <VERSION>\n")
      os.exit(0)
    else
      biberrorf('Unknown option %s', argv[1])
      help(2)
    end
    table.remove(argv, 1)
  end
  if table.getn(argv) < 1 then
    bibfatalf('Usage: %s [-permissive|-strict|...] filename[.aux] [bibfile...]',
              argv[0])
  end
  local auxname = table.remove(argv, 1)
  local basename = string.gsub(string.gsub(auxname, '%.aux$', ''), '%.$', '')
  auxname = basename .. '.aux'
  local bblname = output_name or (basename .. '.bbl')
  local blgname = basename .. (output_name and '.nlg' or '.blg')
  local blg = open(blgname, 'w')

  -- Here's what we accumulate by reading .aux files: 
  local bibstyle           -- the bibliography style
  local bibfiles = { }     -- list of files named in order of file
  local citekeys = { }     -- list of citation keys from .aux (in order, lower, no dups)
  local cited_star = false -- .tex contains \cite{*} or \nocite{*}

  --   Reading all the aux files and validating the inputs
  -- 
  -- We pay attention to four commands: [[input]],
  -- [[\bibdata]], [[\bibstyle]], and [[\citation]].
  -- <using file [[auxname]], set [[bibstyle]], [[citekeys]], and [[bibfiles]]>=
  do
    local commands = { } -- table of commands we recognize in .aux files
    local function do_nothing() end -- default for unrecognized commands
    setmetatable(commands, { __index = function() return do_nothing end })
    -- <functions for commands found in .aux files>=
    do
      local auxopened = { }  --- map filename to true/false

      commands['@input'] = function (auxname)
        if not find(auxname, '%.aux$') then
          bibwarnf('Name of auxfile "%s" does not end in .aux\n', auxname)
        end
        -- <mark [[auxname]] as opened (but fail if opened already)>=
        if auxopened[auxname] then
          error("File " .. auxname .. " cyclically \\@input's itself")
        else
          auxopened[auxname] = true
        end
        local aux = open(auxname, 'r')
        logf('Top-level aux file: %s\n', auxname)
        for line in aux:lines() do
          local _, _, cmd, arg = find(line, '^\\([%a%@]+)%s*{([^%}]+)}%s*$')
          if cmd then commands[cmd](arg) end
        end
        aux:close()
      end
    end
    -- BibTeX expects .bib files to be separated by commas.
    -- They are forced to lower case, should have no spaces
    -- in them, and the [[\bibdata]] command should appear
    -- exactly once.
    -- <functions for commands found in .aux files>=
    do
      local bibdata_seen = false

      function commands.bibdata(arg)
        assert(not bibdata_seen, [[LaTeX provides multiple \bibdata commands]])
        bibdata_seen = true
        for bib in string.gfind(arg, '[^,]+') do
          assert(not find(bib, '%s'), 'bibname from LaTeX contains whitespace')
          table.insert(bibfiles, string.lower(bib))
        end
      end
    end
    -- The style should be unique, and it should be known to
    -- us.
    -- <functions for commands found in .aux files>=
    function commands.bibstyle(stylename)
      if bibstyle then
        biberrorf('Illegal, another \\bibstyle command')
      else
        bibstyle = bibtex.style(string.lower(stylename))
        if not bibstyle then
          bibfatalf('There is no nbibtex style called "%s"')
        end
      end
    end
    -- We accumulated cited keys in [[citekeys]]. Keys may be
    -- duplicated, but the input should not contain two keys
    -- that differ only in case.
    -- <functions for commands found in .aux files>=
    do
      local keys_seen, lower_seen = { }, { } -- which keys have been seen already

      function commands.citation(arg)
        for key in string.gfind(arg, '[^,]+') do
          assert(not find(key, '%s'), 'Citation key from LaTeX contains whitespace')
          if key == '*' then
            cited_star = true
          elseif not keys_seen[key] then --- duplicates are OK
            keys_seen[key] = true
            local low = string.lower(key)
            -- <if another key with same lowercase, complain bitterly>=
            if lower_seen[low] then
              biberrorf("Citation key '%s' inconsistent with earlier key '%s'",
                        key, lower_seen[low])
            else
              lower_seen[low] = key
            end
            if not cited_star then -- no more insertions after the star
              table.insert(citekeys, low) --- maybe should be key instead?
            end
          end
        end
      end
    end
    commands['@input'](auxname)  -- reads all the variables
  end

  if table.getn(argv) > 0 then -- override the bibfiles listed in the .aux file
    bibfiles = argv
  end
  -- After reading the variables, we do a little
  -- validation. I can't seem to make up my mind what
  -- should be done incrementally while things are being
  -- read.
  -- <validate contents of [[bibstyle]], [[citekeys]], and [[bibfiles]]>=
  if not bibstyle then
    bibfatalf('No \\bibliographystyle in original LaTeX')
  end

  if table.getn(bibfiles) == 0 then
    bibfatalf('No .bib files specified --- no \\bibliography in original LaTeX?')
  end

  if table.getn(citekeys) == 0 and not cited_star then
    biberrorf('No citations in document --- empty bibliography')
  end

  do --- check for duplicate bib entries
    local i = 1
    local seen = { }
    while i <= table.getn(bibfiles) do
      local bib = bibfiles[i]
      if seen[bib] then
        bibwarnf('Multiple references to bibfile "%s"', bib)
        table.remove(bibfiles, i)
      else
        i = i + 1
      end
    end
  end    
  --   Reading the entries from all the BibTeX files
  -- 
  -- These are diagnostics that might be written to a log.
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  logf("bibstyle == %q\n", bibstyle.name)
  logf("consult these bibfiles:")
  for _, bib in ipairs(bibfiles) do logf(" %s", bib) end
  logf("\ncite these papers:\n")
  for _, key in ipairs(citekeys) do logf("  %s\n", key) end
  if cited_star then logf("  and everything else in the database\n") end
  -- Each bibliography file is opened with [[openbib]].
  -- Unlike classic BibTeX, we can't simply select the
  -- first entry matching a citation key. Instead, we read
  -- all entries into [[bibentries]] and do searches later.
  -- 
  -- The easy case is when we're not permissive: we put all
  -- the entries into one list, just as if they had come
  -- from a single .bib file. But if we're permissive,
  -- duplicates in different bibfiles are OK: we will
  -- search one bibfile after another and stop after the
  -- first successful search---thus instead of a single
  -- list, we have a list of lists.
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  local bibentries = { } -- if permissive, list of lists, else list of entries
  local dupcheck = { } -- maps lower key to entry
  local preamble = { } -- accumulates preambles from all .bib files
  local got_one_bib = false -- did we open even one .bib file?
  -- Here we open files. If we're not being permissive, we
  -- must open each file successfully. If we're permissive,
  -- it's enough to get at least one.
  -- 
  -- To find the pathname for a bib file, we use
  -- [[bibtex.bibpath]].
  -- <definition of function [[openbib]], which sets [[get_one_bib]] if successful>=
  local function openbib(bib)
    local macros = bibstyle.macros()
    local filename, msg = bibtex.bibpath(bib)
    if not filename then
      if not permissive then biberrorf("Cannot find file %s.bib", bib) end
      return
    end
    local rdr = bibtex.open(filename, macros, strict and emit_warning or hold_warning)
    if not rdr and not permissive then
      biberrorf("Cannot open file %s.bib", bib)
      return
    end
    got_one_bib = true
    return filename, rdr
  end

  local warnings = { }  -- table of held warnings for each entry
  for _, bib in ipairs(bibfiles) do
    local bibfilename, rdr = openbib(bib)
    if rdr then
      local t -- list that will receive entries from this reader
      if permissive then
        t = { }
        table.insert(bibentries, t)
      else
        t = bibentries
      end
      local localdupcheck = { } -- lower key to entry; finds duplicates within this file
      for type, key, fields, file, line in entries(rdr) do
        if type == nil then
          break
        elseif type then -- got something without error
          local e = { type = type, key = key, fields = fields,
                      file = bibfilename, line = rdr.entry_line }
          warnings[e] = held_warnings()
          --  Duplication checks
          -- 
          -- There's a great deal of nuisance to checking the
          -- integrity of a .bib file.
          -- <definition of local function [[not_dup]]>=
          -- Calling [[savecomplaint(e1, e2, complain, ...)]] takes
          -- the complaint [[complain(...)]] and associates it with
          -- entries [[e1]] and [[e2]]. If we are operating in
          -- ``strict'' mode, the complaint is issued right away;
          -- otherwise calling [[issuecomplaints(e)]] issues the
          -- complaint lazily. In non-strict, lazy mode, the
          -- outside world arranges to issue only complaints with
          -- entries that are actually used.
          -- <abstraction exporting [[savecomplaint]] and [[issuecomplaints]]>=
          local savecomplained, issuecomplaints
          if strict then
            function savecomplaint(e1, e2, complain, ...)
              return complain(unpack(arg))
            end
            function issuecomplaints(e) end
          else
            local complaints = { }
            local function save(e, t)
              complaints[e] = complaints[e] or { }
              table.insert(complaints[e], t)
            end
            function savecomplaint(e1, e2, ...)
              save(e1, arg)
              save(e2, arg)
            end
            local function call(c, ...)
              return c(unpack(arg))
            end
            function issuecomplaints(e)
              for _, c in ipairs(complaints[e] or { }) do
                call(unpack(c))
              end
            end
          end

          local k = string.lower(key)
          local function not_dup(dup)
            local e1, e2 = dup[k], e
            if e1 then
              -- do return false end --- avoid extra msgs for now
              local diff = entries_differ(e1, e2)
              if diff then
                local verybad = not permissive or e1.file == e2.file
                local complain = verybad and biberrorf or bibwarnf
                if e1.key == e2.key then
                  if verybad then
                    savecomplaint(e1, e2, complain,
                                  "Ignoring second entry with key '%s' on file %s, line %d\n" ..
                                  "  (first entry occurred on file %s, line %d;\n"..
                                  "   entries differ in %s)\n",
                                  e2.key, e2.file, e2.line, e1.file, e1.line, diff)
                  end
                else
                  savecomplaint(e1, e2, complain,
                     "Entries '%s' on file %s, line %d and\n  '%s' on file %s, line %d" ..
                     " have keys that differ only in case\n",
                     e1.key, e1.file, e1.line, e2.key, e2.file, e2.line)
                end
              elseif e1.file == e2.file then
                savecomplaint(e1, e2, bibwarnf,
                  "Entry '%s' is duplicated in file '%s' at both line %d and line %d\n",
                  e1.key, e1.file, e1.line, e2.line)
              elseif not permissive then
                savecomplaint(e1, e2, bibwarnf,
                  "Entry '%s' appears both on file '%s', line %d and file '%s', line %d"..
                  "\n  (entries are exact duplicates)\n",
                  e1.key, e1.file, e1.line, e2.file, e2.line)
              end
              return false
            else
              dup[k] = e
              return true
            end
          end
          local ok1, ok2 = not_dup(localdupcheck), not_dup(dupcheck) -- evaluate both
          if ok1 and ok2 then 
            table.insert(t, e)
          end
        end
      end
      for _, l in ipairs(rdr.preamble) do table.insert(preamble, l) end
      rdr:close()
    end
  end

  if not got_one_bib then
    bibfatalf("Could not open any of the following .bib files: %s",
              table.concat(bibfiles, ' '))
  end
  --   Computing and emitting the list of citations
  -- 
  -- A significant complexity added in NbibTeX is that a
  -- single entry may be cited using more than one citation
  -- key. For example, [[[cite milner:type-polymorphism]]]
  -- and [[[cite milner:theory-polymorphism]]] may well
  -- specify the same paper. Thus, in addition to a list of
  -- citations, I also keep track of the set of keys with
  -- which each entry is cited, as well as the first such
  -- key. The function [[cite]] manages all these data
  -- structures.
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  local citations = { } -- list of citations
  local cited = { } -- (entry -> key set) table
  local first_cited = { } -- (entry -> key) table
  local function cite(c, e) -- cite entry e with key c
    local seen = cited[e]
    cited[e] = seen or { }
    cited[e][c] = true
    if not seen then
      first_cited[e] = c
      table.insert(citations, e)
    end
  end
  -- For each actual [[[cite ]]] command in the original
  -- LaTeX file, we call [[find_entry]] to find an
  -- appropriate BibTeX entry. Because a [[[cite ]]]
  -- command might match more than one paper, the results
  -- may be ambiguous. We therefore produce a list of all
  -- candidates matching the [[[cite ]]] command. If we're
  -- permissive, we search one list of entries after
  -- another, stopping as soon as we get some candidates.
  -- If we're not permissive, we have just one list of
  -- entries overall, so we search it and we're done. If
  -- permissive, we search entry lists in turn until we
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  local find_entry -- function from key to citation
  do
    -- The [[query]] function uses the engine described in
    -- Section [->].
    -- <definition of internal function [[query]], used to search a list of entries>=
    local function query(c, entries)
      local p = matchq(c)
      local t = { }
      for _, e in ipairs(entries) do
        if p(e.type, e.fields) then
          table.insert(t, e)
        end
      end
      return t
    end
    local cache = { } -- (citation-key -> entry) table

    function find_entry(c)
      local function remember(e) cache[c] = e; return e end -- cache e and return it

      if cache[c] or dupcheck[c] then
        return cache[c] or dupcheck[c]
      else
        local candidates
        if permissive then
          for _, entries in ipairs(bibentries) do
            candidates = query(c, entries)
            if table.getn(candidates) > 0 then break end
          end
        else
          candidates = query(c, bibentries)
        end
        assert(candidates)    
        -- If we have no candidates, we're hosed. Otherwise, if
        -- all the candidates are identical (most likely when
        -- there is a unique candidate, but still possible
        -- otherwise), [Andrew Appel has a bibliography in which
        -- the \emph{Definition of Standard~ML} appears as two
        -- different entries that are identical except for keys.]
        -- we take the first. Finally, if there are multiple,
        -- distinct candidates to choose from, we take the first
        -- and issue a warning message. To avoid surprising the
        -- unwary coauthor, we put a warning message into the
        -- entry as well, from which it will go into the printed
        -- bibliography.
        -- <from the available [[candidates]], choose one and [[remember]] it>=
        if table.getn(candidates) == 0 then
          biberrorf('No .bib entry matches \\cite{%s}', c)
        elseif all_entries_identical(candidates, 'notkey') then
          logf("Query '%s' produced unique candidate %s from %s\n",
                  c, candidates[1].key, candidates[1].file)
          return remember(candidates[1])
        else
          local e = table.copy(candidates[1])
          -- I can do better later...
          -- 
          -- <warn of multiple candidates for query [[c]]>=
          bibwarnf("Query '%s' produced %d candidates\n    (using %s from %s)\n",
                   c, table.getn(candidates), e.key, e.file)
          bibwarnf("First two differ in %s\n", entries_differ(candidates[1], candidates[2], true))
          e.warningmsg = string.format('[This entry is the first match for query ' ..
                                       '\\texttt{%s}, which produced %d matches.]',
                                       c, table.getn(candidates))
          return remember(e)
        end
      end
    end
  end
  -- Finally we can compute the list of entries: search on
  -- each citation key, and if we had [[[cite *]]] or [[]],
  -- add all the other entries as well. The [[cite]]
  -- command takes care of avoiding duplicates.
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  for _, c in ipairs(citekeys) do
    local e = find_entry(c)
    if e then cite(c, e) end
  end
  if cited_star then
    for _, es in ipairs(permissive and bibentries or {bibentries}) do
      logf('Adding all entries in list of %d\n', table.getn(es))
      for _, e in ipairs(es) do
        cite(e.key, e)
      end
    end
  end
  -- When the dust settles, we adjust members of each
  -- citation record: the first key actually used becomes
  -- [[key]], the original key becomes [[orig_key]], and
  -- other keys go into [[also_cited_as]].
  -- <using [[cited]] and [[first_cited]], adjust fields [[key]] and [[also_cited_as]]>=
  for i = 1, table.getn(citations) do
    local c = citations[i]
    local key = assert(first_cited[c], "citation is not cited?!")
    c.orig_key, c.key = c.key, key
    local also = { }
    for k in pairs(cited[c]) do
      if k ~= key then table.insert(also, k) end
    end
    c.also_cited_as = also
  end
  -- I've always hated BibTeX's cross-reference feature,
  -- but I believe I've implemented it faithfully.
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  bibtex.do_crossrefs(citations, find_entry)
  -- With the entries computed, there are two ways to emit:
  -- as another BibTeX file or as required by the style
  -- file. So that we can read from [[bblname]] before
  -- writing to it, the opening of [[bbl]] is carefully
  -- delayed to this point.
  -- <from [[bibstyle]], [[citekeys]], and [[bibfiles]], compute and emit the list of entries>=
  -- <emit warnings for entries in [[citations]]>=
  for _, e in ipairs(citations) do
    if warnings[e] then
      for _, w in ipairs(warnings[e]) do emit_warning(unpack(w)) end
    end
  end
  local bbl = bblname == '-' and io.stdout or open(bblname, 'w')
  if bib_out then
    bibtex.emit(bbl, preamble, citations)
  else
    bibstyle.emit(bbl, preamble, citations)
  end
  if bblname ~= '-' then bbl:close() end
  blg:close()
end
-- Here's a function to emit a list of citations as
-- BibTeX source.
-- <exported Lua functions>=
bibtex.doc.emit = 
  'outfile * string list * entry list -> unit -- write citations in .bib format'
function bibtex.emit(bbl, preamble, citations)
  local warned = false
  if preamble[1] then
    bbl:write('@preamble{\n')
    for i = 1, table.getn(preamble) do
      bbl:write(string.format('  %s "%s"\n', i > 1 and '#' or ' ', preamble[i]))
    end
    bbl:write('}\n\n')
  end
  for _, e in ipairs(citations) do
    local also = e.also_cited_as
    if also and table.getn(also) > 0 then
      for _, k in ipairs(e.also_cited_as or { }) do
        bbl:write(string.format('@%s{%s, crossref={%s}}\n', e.type, k, e.key))
      end
      if not warned then
        warned = true
        bibwarnf("Warning: some entries (such as %s) are cited with multiple keys;\n"..
          "  in the emitted .bib file, these entries are duplicated (using crossref)\n",
                 e.key)
      end
    end
    emit_tkf.bib(bbl, e.type, e.key, e.fields)
  end
end
--   Cross-reference
-- 
-- If an entry contains a [[crossref]] field, that field
-- is used as a key to find the parent, and the entry
-- inherits missing fields from the parent.
-- 
-- If the parent is cross-referenced sufficiently often
-- (i.e., more than [[min_crossref]] times), it may be
-- added to the citation list, in which case the style
-- file knows what to do with the [[crossref]] field. But
-- if the parent is not cited sufficiently often, it
-- disappears, and do does the [[crossref]] field.
-- <exported Lua functions>=
bibtex.doc.do_crossrefs = "citation list -> unit # add crossref'ed fields in place"
function bibtex.do_crossrefs(citations, find_entry)
  local map  = { } --- key to entry (on citation list)
  local xmap = { } --- key to entry (xref'd only)
  local xref_count = { } -- entry -> number of times xref'd
  -- <make [[map]] map lower-case keys in [[citations]] to their entries>=
  for i = 1, table.getn(citations) do
    local c = citations[i]
    local key = string.lower(c.key)
    map[key] = map[key] or c
  end
  for i = 1, table.getn(citations) do
    local c = citations[i]
    if c.fields.crossref then
      local lowref = string.lower(c.fields.crossref)
      local parent = map[lowref] or xmap[lowref]
      if not parent and find_entry then
        parent = find_entry(lowref)
        xmap[lowref] = parent
      end
      if not parent then
        biberrorf("Entry %s cross-references to %s, but I can't find %s",
                  c.key, c.fields.crossref, c.fields.crossref)
        c.fields.crossref = nil
      else
        xref_count[parent] = (xref_count[parent] or 0) + 1 
        local fields = c.fields
        fields.crossref = parent.key -- force a case match!
        for k, v in pairs(parent.fields) do -- inherit field if missing
          fields[k] = fields[k] or v
        end
      end
    end
  end
  -- <add oft-crossref'd entries from [[xmap]] to the list in [[citations]]>=
  for _, e in pairs(xmap) do -- includes only missing entries
    if xref_count[e] >= min_crossrefs then
      table.insert(citations, e)
    end
  end
  -- <remove [[crossref]] fields for entries with seldom-crossref'd parents>=
  for i = 1, table.getn(citations) do
    local c = citations[i]
    if c.fields.crossref then
      local parent = xmap[string.lower(c.fields.crossref)]
      if parent and xref_count[parent] < min_crossrefs then
        c.fields.crossref = nil
      end
    end
  end
end
--   The query engine (i.e., the point of it all)
-- 
-- [*] The query language is described in the man page
-- for [[nbibtex]]. Its implementation is divided into
-- two parts: the internal predicates which are composed
-- to form a query predicate, and the parser that takes a
-- string and produces a query predicate. Function
-- [[matchq]] is declared [[local]] above and is the only
-- function visible outside this block.
-- <exported Lua functions>=
do
  if not boyer_moore then
    require 'boyer-moore'
  end
  local bm = boyer_moore
  local compile = bm.compilenc
  local search  = bm.matchnc

  -- type predicate = type * field table -> bool
  -- val match   : field * string -> predicate
  -- val author  : string -> predicate
  -- val matchty : string -> predicate
  -- val andp    : predicate option * predicate option -> predicate option
  -- val orp     : predicate option * predicate option -> predicate option
  -- val matchq  : string -> predicate --- compile query string

  --  Query predicates
  -- 
  -- The common case is a predicate for a named field. We
  -- also have some special syntax for ``all fields'' and
  -- the BibTeX ``type,'' which is not a field.
  -- <definitions of query-predicate functions>=
  local matchty
  local function match(field, string)
    if string == '' then return nil end
    local pat = compile(string)
    if field == '*' then
      return function (t, fields)
               for _, v in pairs(fields) do if search(pat, v) then return true end end
             end
    elseif field == '[type]' then
      return matchty(string)
    else
      return function (t, fields) return search(pat, fields[field] or '') end
    end
  end
  -- Here's a type matcher.
  -- 
  -- <definitions of query-predicate functions>=
  function matchty(string)
    if string == '' then return nil end
    local pat = compile(string)
    return function (t, fields) return search(pat, t) end
  end
  -- We make a special case of [[author]] because it really
  -- means ``author or editor.''
  -- <definitions of query-predicate functions>=
  local function author(string)
    if string == '' then return nil end
    local pat = compile(string)
    return function (t, fields)
             return search(pat, fields.author or fields.editor or '')
           end
  end
  -- We conjoin and disjoin predicates, being careful to
  -- use tail calls (not [[and]] and [[or]]) in order to
  -- save stack space.
  -- <definitions of query-predicate functions>=
  local function andp(p, q)
    -- associate to right for constant stack space
    if not p then
      return q
    elseif not q then 
      return p
    else 
      return function (t,f) if p(t,f) then return q(t,f) end end
    end
  end
  -- <definitions of query-predicate functions>=
  local function orp(p, q)
    -- associate to right for constant stack space
    if not p then
      return q
    elseif not q then 
      return p
    else 
      return function (t,f) if p(t,f) then return true else return q(t,f) end end
    end
  end

  --  The query compiler
  -- 
  -- The function [[matchq]] takes the syntax explained in
  -- the man page and produces a predicate.
  -- <definition of [[matchq]], the query compiler>=
  function matchq(query)
    local find = string.find
    local parts = split(query, '%:')
    local p = nil
    if parts[1] and not find(parts[1], '=') then
      -- Here's where an unnamed key defaults to author or
      -- editor.
      -- <add to [[p]] a match for [[parts[1]]] as author>=
      for _, word in ipairs(split(parts[1], '-')) do
        p = andp(author(word), p)
      end
      table.remove(parts, 1)
      if parts[1] and not find(parts[1], '=') then
        -- <add to [[p]] a match for [[parts[1]]] as title or year>=
        local field, words = find(parts[1], '%D') and 'title' or 'year', parts[1]
        -- There could be lots of matches on a year, so we check
        -- years last.
        -- <add to [[p]] a match for [[words]] as [[field]]>=
        for _, word in ipairs(split(words, '-')) do
          if field == 'year' then
            p = andp(p, match(field, word))
          else
            p = andp(match(field, word), p)
          end
        end
        table.remove(parts, 1)
        if parts[1] and not find(parts[1], '=') then
          -- <add to [[p]] a match for [[parts[1]]] as type or year>=
          if find(parts[1], '%D') then
            local ty = nil
            for _, word in ipairs(split(parts[1], '-')) do
              ty = orp(matchty(word), ty)
            end
            p = andp(p, ty) --- check type last for efficiency
          else
            for _, word in ipairs(split(parts[1], '-')) do
              p = andp(p, match('year', word)) -- check year last for efficiency
            end
          end
          table.remove(parts, 1)
        end
      end
    end
    for _, part in ipairs(parts) do
      if not find(part, '=') then
        biberrorf('bad query %q --- late specs need = sign', query)
      else
        local _, _, field, words = find(part, '^(.*)=(.*)$')
        assert(field and words, 'bug in query parsing')
        -- There could be lots of matches on a year, so we check
        -- years last.
        -- <add to [[p]] a match for [[words]] as [[field]]>=
        for _, word in ipairs(split(words, '-')) do
          if field == 'year' then
            p = andp(p, match(field, word))
          else
            p = andp(match(field, word), p)
          end
        end
      end
    end
    if not p then
      bibwarnf('empty query---matches everything\n')
      return function() return true end
    else
      return p
    end
  end
end
-- Function [[bibpath]] is normally called on a bibname
-- in a LaTeX file, but because a bibname may also be
-- given on the command line, we add .bib only if not
-- already present. Also, because we can
-- <exported Lua functions>=
bibtex.doc.bibpath = 'string -> string # from \\bibliography name, find pathname of file'
function bibtex.bibpath(bib)
  assert(not(find(bib, '/')), 'Function bibpath should not be given a pathname')
  if not find(bib, '%.bib$') then
    bib = bib .. '.bib'
  end
  local pathname = capture('kpsewhich ' .. bib)
  if string.len(pathname) > 1 then
    return pathname
  else
    return nil, 'kpsewhich cannot find ' .. bib
  end
end
-- The simplest entry is legitimate BibTeX source:
-- 
-- <exported Lua functions>=


------------------------------------------------------------------------
---------------------- IMPLEMENTATION OF NBIBFIND ----------------------
------------------------------------------------------------------------

function emit_tkf.bib(outfile, type, key, fields)
  outfile:write('@', type, '{', key, ',\n')
  for k, v in pairs(fields) do
    outfile:write('  ', k, ' = {', v, '},\n')
  end
  outfile:write('}\n\n')
end
-- For the other two entries, we devise a string format.
-- In principle, we could go with an ASCII form of a
-- full-blown style, but since the purpose is to identify
-- the entry in relatively few characters, it seems
-- sufficient to spit out the author, year, title, and
-- possibly the source. ``Full'' output shows the whole
-- string; ``terse'' is just the first line.
-- <exported Lua functions>=
do
  local function bibstring(type, key, fields, bib)
    -- <define local [[format_lab_names]] as for a bibliography label>=
    local format_lab_names
    do 
      local fmt = '{vv }{ll}'
      local function format_names(s)
        local s = bst.commafy(bst.format_names(fmt, bst.namesplit(s)))
        return (string.gsub(s, ' and others$', ' et al.'))
      end
      function format_lab_names(s)
        if not s then return s end
        local t = bst.namesplit(s)
        if table.getn(t) > 3 then
          return bst.format_name(fmt, t[1]) .. ' et al.'
        else
          return format_names(s)
        end
      end
    end
    local names = format_lab_names(fields.author) or
                  format_lab_names(fields.editor) or
                  fields.key or fields.organization or '????'
    local year = fields.year
    local lbl = names .. (year and ' ' .. year or '')
    local title = fields.title or '????'
    if bib then
      key = string.gsub(bib, '.*/', '') .. ': ' .. key
    end
    local answer =
      bib and
      string.format('%-25s = %s: %s', key, lbl, title) or
      string.format('%-21s = %s: %s', key, lbl, title)
    local where = fields.booktitle or fields.journal
    if where then answer = answer .. ', in ' .. where end
    answer = string.gsub(answer, '%~', ' ')
    for _, cs in ipairs { 'texttt', 'emph', 'textrm', 'textup' } do
      answer = string.gsub(answer, '\\' .. cs .. '%A', '')
    end
    answer = string.gsub(answer, '[%{%}]', '')
    return answer
  end

  function emit_tkf.terse(outfile, type, key, fields, bib)
    outfile:write(truncate(bibstring(type, key, fields, bib), 80), '\n')
  end

  function emit_tkf.full(outfile, type, key, fields, bib)
    local w = bst.writer(outfile)
    w:write(bibstring(type, key, fields, bib), '\n')
  end
end
-- <exported Lua functions>=
bibtex.doc.run_find = 'string list -> unit # main program for nbibfind'
bibtex.doc.find = 'string * string list -> entry list'

function bibtex.find(pattern, bibs)
  local es = { }
  local p = matchq(pattern)
  for _, bib in ipairs(bibs) do
    local rdr = bibtex.open(bib, bst.months(), hold_warning)
    for type, key, fields in entries(rdr) do
      if type == nil then
        break
      elseif not type then
        io.stderr:write('Something disastrous happened with entry ', key, '\n')
      elseif key == pattern or p(type, fields) then
        -- <emit held warnings, if any>=
        local ws = held_warnings()
        if ws then
          for _, w in ipairs(ws) do
            emit_warning(unpack(w))
          end
        end
        table.insert(es, { type = type, key = key, fields = fields,
                           bib = table.getn(bibs) > 1 and bib })
      else
        drop_warnings()
      end
    end
    rdr:close()
  end
  return es
end

function bibtex.run_find(argv)
  local emit = emit_tkf.terse
  while argv[1] and find(argv[1], '^-') do
    if emit_tkf[string.sub(argv[1], 2)] then
      emit = emit_tkf[string.sub(argv[1], 2)]
    else
      biberrorf('Unrecognized option %s', argv[1])
    end
    table.remove(argv, 1)
  end
  if table.getn(argv) == 0 then
    io.stderr:write(string.format('Usage: %s [-bib|-terse|-full] pattern [bibs]\n',
                                  string.gsub(argv[0], '.*/', '')))
    os.exit(1)
  end
  local pattern = table.remove(argv, 1)
  local bibs = { }
  -- If we have no arguments, search all available
  -- bibfiles. Otherwise, an argument with a [[/]] is a
  -- pathname, and an argument without [[/]] is a name as
  -- it would appear in [[[BibTeX bibliography]
  -- <make [[bibs]] the list of pathnames implied by [[argv]]>=
  if table.getn(argv) == 0 then
    bibs = all_bibs()
  else
    for _, a in ipairs(argv) do
      if find(a, '/') then
        table.insert(bibs, a)
      else
        table.insert(bibs, assert(bibtex.bibpath(a)))
      end
    end
  end

  local entries = bibtex.find(pattern, bibs)
  for _, e in ipairs(entries) do
    emit(io.stdout, e.type, e.key, e.fields, e.bib)
  end
end
-- In classic BibTeX, each style is its own separate
-- file. Here, we share code by allowing a single file to
-- register multiple styles.
-- <exported Lua functions>=


------------------------------------------------------------------------
----------------------- SUPPORT FOR STYLE FILES ------------------------
------------------------------------------------------------------------

bibtex.doc.register_style = 
  [[string * style -> unit # remember style with given name
type style = { emit   : outfile * string list * citation list -> unit
             , style  : table of formatting functions # defined document types
             , macros : unit -> macro table 
             }]]
bibtex.doc.style = 'name -> style # return style with given name, loading on demand'

do
  local styles = { }

  function bibtex.register_style(name, s)
    assert(not styles[name], "Duplicate registration of style " .. name)
    styles[name] = s
    s.name = s.name or name
  end

  function bibtex.style(name)
    if not styles[name] then
      local loaded 
      if config.nbs then
        local loaded = loadfile(config.nbs .. '/' .. name .. '.nbs')
        if loaded then loaded() end
      end
      if not loaded then
        require ('nbib-' .. name)
      end
      if not styles[name] then
        bibfatalf('Tried to load a file, but it did not register style %s\n', name)
      end
    end
    return styles[name] 
  end
end
-- Sometimes we want to know not how many characters are
-- in a string, but how much space we expect it to take
-- when typeset. (Or rather, we want to compare such
-- widths to find the widest.) This is original BibTeX's
-- [[width]] function. 
-- 
-- The code should use the [[char_width]] array, for
-- which [[space]] is the only whitespace character given
-- a nonzero printing width. The widths here are taken
-- from Stanford's June '87 cmr10 font and represent
-- hundredths of a point (rounded), but since they're
-- used only for relative comparisons, the units have no
-- meaning.
-- <exported Lua functions>=
do
  local char_width = { }
  local special_widths = { ss = 500, ae = 722, oe = 778, AE = 903, oe = 1014 }
  for i = 0, 255 do char_width[i] = 0 end
  local char_width_from_32 = {
     278, 278, 500, 833, 500, 833, 778, 278, 389, 389, 500, 778, 278, 333,
     278, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 278, 278,
     278, 778, 472, 472, 778, 750, 708, 722, 764, 681, 653, 785, 750, 361,
     514, 778, 625, 917, 750, 778, 681, 778, 736, 556, 722, 750, 750,
     1028, 750, 750, 611, 278, 500, 278, 500, 278, 278, 500, 556, 444,
     556, 444, 306, 500, 556, 278, 306, 528, 278, 833, 556, 500, 556, 528,
     392, 394, 389, 556, 528, 722, 528, 528, 444, 500, 1000, 500, 500,
  }
  for i = 1, table.getn(char_width_from_32) do
    char_width[32+i-1] = char_width_from_32[i]
  end

  bst.doc.width = "string -> faux_points # width of string in 1987 cmr10"
  function bst.width(s)
    assert(false, 'have not implemented width yet')
  end
end
--   Parsing names and lists of names
-- 
-- Names in a string are separated by and surrounded by
-- nonnull whitespace. Case is not significant.
-- <exported Lua functions>=
local function namesplit(s)
  local t = split(s, '%s+[aA][nN][dD]%s+', find_outside_braces)
  local i = 2
  while i <= table.getn(t) do
    while find(t[i], '^[aA][nN][dD]%s+') do
      t[i] = string.gsub(t[i], '^[aA][nN][dD]%s+', '')
      table.insert(t, i, '')
      i = i + 1
    end
    i = i + 1
  end
  return t
end
bst.namesplit = namesplit
bst.doc.namesplit = 'string -> list of names # split names on "and"'

-- <exported Lua functions>=
local sep_and_not_tie = '%-'
local sep_chars = sep_and_not_tie .. '%~'
-- To parse an individual name, we want to count commas.
-- We first remove leading white space (and
-- [[sep_char]]s), and trailing white space (and
-- [[sep_char]]s) and commas, complaining for each
-- trailing comma.
-- 
-- We then represent the name as two sequences:
-- [[tokens]] and [[trailers]]. The [[tokens]] are the
-- names themselves, and the [[trailers]] are the
-- separator characters between tokens. (A separator is
-- white space, a dash, or a tie, and multiple separators
-- in sequence are frowned upon.) The [[commas]] table
-- becomes an array mapping the comma number to the index
-- of the token that follows it.
-- <exported Lua functions>=
local parse_name
do
  local white_sep         = '[' .. sep_chars .. '%s]+'
  local white_comma_sep   = '[' .. sep_chars .. '%s%,]+'
  local trailing_commas   = '(,[' .. sep_chars .. '%s%,]*)$'
  local sep_char          = '[' .. sep_chars .. ']'
  local leading_white_sep = '^' .. white_sep

  -- <name-parsing utilities>=
  function isVon(s)
    local lower  = find_outside_braces(s, '%l') -- first nonbrace lowercase
    local letter = find_outside_braces(s, '%a') -- first nonbrace letter
    local bs, ebs, command = find_outside_braces(s, '%{%\\(%a+)') -- \xxx
    if lower and lower <= letter and lower <= (bs or lower) then
      return true
    elseif letter and letter <= (bs or letter) then
      return false
    elseif bs then
      if upper_specials[command] then
        return false
      elseif lower_specials[command] then
        return true
      else
        local close_brace = find_outside_braces(s, '%}', ebs+1)
        lower  = find(s, '%l') -- first nonbrace lowercase
        letter = find(s, '%a') -- first nonbrace letter
        return lower and lower <= letter
      end
    else
      return false
    end
  end

  function parse_name(s, inter_token)
    if string.find(s, trailing_commas) then
      biberrorf("Name '%s' has one or more commas at the end", s)
    end
    s = string.gsub(s, trailing_commas, '')
    s = string.gsub(s, leading_white_sep, '')
    local tokens = split(s, white_comma_sep, find_outside_braces)
    local trailers = splitters(s, white_comma_sep, find_outside_braces)
    -- The string separating tokens is reduced to a single
    -- ``separator character.'' A comma always trumps other
    -- separator characters. Otherwise, if there's no comma,
    -- we take the first character, be it a separator or a
    -- space. (Patashnik considers that multiple such
    -- characters constitute ``silliness'' on the user's
    -- part.)
    -- <rewrite [[trailers]] to hold a single separator character each>=
    for i = 1, table.getn(trailers) do
      local s = trailers[i]
      assert(string.len(s) > 0)
      if find(s, ',') then
        trailers[i] = ','
      else
        trailers[i] = string.sub(s, 1, 1)
      end
    end
    local commas = { } --- maps each comma to index of token the follows it
    for i, t in ipairs(trailers) do
      string.gsub(t, ',', function() table.insert(commas, i+1) end)
    end
    local name = { }
    -- A name has up to four parts: the most general form is
    -- either ``First von Last, Junior'' or ``von Last,
    -- First, Junior'', but various vons and Juniors can be
    -- omitted. The name-parsing algorithm is baroque and is
    -- transliterated from the original BibTeX source, but
    -- the principle is clear: assign the full version of
    -- each part to the four fields [[ff]], [[vv]], [[ll]],
    -- and [[jj]]; and assign an abbreviated version of each
    -- part to the fields [[f]], [[v]], [[l]], and [[j]].
    -- <parse the name tokens and set fields of [[name]]>=
    local first_start, first_lim, last_lim, von_start, von_lim, jr_lim
      -- variables mark subsequences; if start == lim, sequence is empty
    local n = table.getn(tokens)
    -- The von name, if any, goes from the first von token to
    -- the last von token, except the last name is entitled
    -- to at least one token. So to find the limit of the von
    -- name, we start just before the last token and wind
    -- down until we find a von token or we hit the von start
    -- (in which latter case there is no von name).
    -- <local parsing functions>=
    function divide_von_from_last()
      von_lim = last_lim - 1;
      while von_lim > von_start and not isVon(tokens[von_lim-1]) do
        von_lim = von_lim - 1
      end
    end

    local commacount = table.getn(commas)
    if commacount == 0 then -- first von last jr
      von_start, first_start, last_lim, jr_lim = 1, 1, n+1, n+1
      -- OK, here's one form.
      -- 
      -- <parse first von last jr>=
      local got_von = false
      while von_start < last_lim-1 do
        if isVon(tokens[von_start]) then
          divide_von_from_last()
          got_von = true
          break
        else
          von_start = von_start + 1
        end
      end
      if not got_von then -- there is no von name
        while von_start > 1 and find(trailers[von_start - 1], sep_and_not_tie) do
          von_start = von_start - 1
        end
        von_lim = von_start
      end
      first_lim = von_start
    elseif commacount == 1 then -- von last jr, first
      von_start, last_lim, jr_lim, first_start, first_lim =
        1, commas[1], commas[1], commas[1], n+1
      divide_von_from_last()
    elseif commacount == 2 then -- von last, jr, first
      von_start, last_lim, jr_lim, first_start, first_lim =
        1, commas[1], commas[2], commas[2], n+1
      divide_von_from_last()
    else
      biberrorf("Too many commas in name '%s'")
    end
    -- <set fields of name based on [[first_start]] and friends>=
    -- We set long and short forms together; [[ss]] is the
    -- long form and [[s]] is the short form.
    -- <definition of function [[set_name]]>=
    local function set_name(start, lim, long, short)
      if start < lim then
        -- string concatenation is quadratic, but names are short
        -- An abbreviated token is the first letter of a token,
        -- except again we have to deal with the damned specials.
        -- <definition of [[abbrev]], for shortening a token>=
        local function abbrev(token)
          local first_alpha, _, alpha = find(token, '(%a)')
          local first_brace           = find(token, '%{%\\')
          if first_alpha and first_alpha <= (first_brace or first_alpha) then
            return alpha
          elseif first_brace then
            local i, j, special = find(token, '(%b{})', first_brace)
            if i then
              return special
            else -- unbalanced braces
              return string.sub(token, first_brace)
            end
          else
            return ''
          end
        end
        local ss = tokens[start]
        local s  = abbrev(tokens[start])
        for i = start + 1, lim - 1 do
          if inter_token then
            ss = ss .. inter_token .. tokens[i]
            s  = s  .. inter_token .. abbrev(tokens[i])
          else
            local ssep, nnext = trailers[i-1], tokens[i]
            local sep,  next  = ssep,          abbrev(nnext)
            -- Here is the default for a character between tokens:
            -- a tie is the default space character between the last
            -- two tokens of the name part, and between the first two
            -- tokens if the first token is short enough; otherwise,
            -- a space is the default.
            -- <possibly adjust [[sep]] and [[ssep]] according to token position and size>=
            if find(sep, sep_char) then
              -- do nothing; sep is OK
            elseif i == lim-1 then
              sep, ssep = '~', '~'
            elseif i == start + 1 then
              sep  = text_char_count(s)  < 3 and '~' or ' '
              ssep = text_char_count(ss) < 3 and '~' or ' '
            else
              sep, ssep = ' ', ' '
            end
            ss = ss ..        ssep .. nnext
            s  = s  .. '.' .. sep  .. next
          end
        end
        name[long] = ss
        name[short] = s
      end
    end
    set_name(first_start, first_lim, 'ff', 'f')
    set_name(von_start,   von_lim,   'vv', 'v')
    set_name(von_lim,     last_lim,  'll', 'l')
    set_name(last_lim,    jr_lim,    'jj', 'j')
    return name
  end
end
bst.parse_name = parse_name
bst.doc.parse_name = 'string * string option -> name table'
--   Formatting names
-- 
-- Lacking Lua's string-processing utilities, classic
-- BibTeX defines a way of converting a ``format string''
-- and a name into a formatted name. I find this
-- formatting technique painful, but I also wanted to
-- preserve compatibility with existing bibliography
-- styles, so I've implemented it as accurately as I can.
-- 
-- The interface is not quite identical to classic
-- BibTeX; a style can use [[namesplit]] to split names
-- and then [[format_name]] to format a single one, or it
-- can throw caution to the winds and call
-- [[format_names]] to format a whole list of names.
-- <exported Lua functions>=
bst.doc.format_names = "format * name list -> string list # format each name in list"
function bst.format_names(fmt, t)
  local u = { }
  for i = 1, table.getn(t) do
    u[i] = bst.format_name(fmt, t[i])
  end
  return u
end
-- A BibTeX format string contains its variable elements
-- inside braces. Thus, we format a name by replacing
-- each braced substring of the format string.
-- <exported Lua functions>=
do
  local good_keys = { ff = true, vv = true, ll = true, jj = true,
                      f  = true, v  = true, l  = true, j  = true, }

  bst.doc.format_name = "format * name -> string # format 1 name as in bibtex"
  function bst.format_name(fmt, name)
    local t = type(name) == 'table' and name or parse_name(name)
    -- at most one of the important letters, perhaps doubled, may appear
    local function replace_braced(s)
      local i, j, alpha = find_outside_braces(s, '(%a+)', 2)
      if not i then
        return '' --- can never be printed, but who are we to complain?
      elseif not good_keys[alpha] then
        biberrorf ('The format string %q has an illegal brace-level-1 letter', s)
      elseif find_outside_braces(s, '%a+', j+1) then
        biberrorf ('The format string %q has two sets of brace-level-1 letters', s)
      elseif t[alpha] then
        local k = j + 1
        local t = t
        -- <make [[k]] follow inter-token string, if any, rebuilding [[t]] as needed>=
        local kk, jj = find(s, '%b{}', k)
        if kk and kk == k then
          k = jj + 1
          if type(name) == 'string' then
            t = parse_name(name, string.sub(s, kk+1, jj-1))
          else
            error('Style error -- used a pre-parsed name with non-standard inter-token format string')
          end
        end
        local head, tail = string.sub(s, 2, i-1) .. t[alpha], string.sub(s, k, -2)
        -- <adjust [[tail]] to account for discretionality of ties, if any>=
        if find(tail, '%~%~$') then
          tail = string.sub(tail, 1, -2) -- denotes hard tie
        elseif find(tail, '%~$') then
          if text_char_count(head) + text_char_count(tail) - 1 >= 3 then
            tail = string.gsub(tail, '%~$', ' ')
          end
        end
        return head .. tail
      else
        return ''
      end
    end
    return (string.gsub(fmt, '%b{}', replace_braced))
  end
end   
--   Line-wrapping output
-- 
-- EXPLAIN THIS INTERFACE!!!
-- 
-- My [[max_print_line]] appears to be off by one from
-- Oren Patashnik's.
-- <exported Lua functions>=
local min_print_line, max_print_line = 3, 79
bibtex.hard_max = max_print_line
bibtex.doc.hard_max = 'int # largest line that avoids a forced line break (for wizards)'
bst.doc.writer = "io-handle * int option -> object # result:write(s) buffers and breaks lines"
function bst.writer(out, indent)
  indent = indent or 2
  assert(indent + 10 < max_print_line)
  indent = string.rep(' ', indent)
  local gsub = string.gsub
  local buf = ''
  local function write(self, ...)
    local s = table.concat(arg)
    local lines = split(s, '\n')
    lines[1] = buf .. lines[1]
    buf = table.remove(lines)
    for i = 1, table.getn(lines) do
      local line = lines[i]
      if not find(line, '^%s+$') then -- no line of just whitespace
        line = gsub(line, '%s+$', '')
        while string.len(line) > max_print_line do
          -- <emit initial part of line and reassign>=
          local last_pre_white, post_white
          local i, j, n = 1, 1, string.len(line)
          while i and i <= n and i <= max_print_line do
            i, j = find(line, '%s+', i)
            if i and i <= max_print_line + 1 then
              if i > min_print_line then last_pre_white, post_white = i - 1, j + 1 end
              i = j + 1
            end
          end
          if last_pre_white then
            out:write(string.sub(line, 1, last_pre_white), '\n')
            if post_white > max_print_line + 2 then
              post_white = max_print_line + 2 -- bug-for-bug compatibility with bibtex
            end
            line = indent .. string.sub(line, post_white)
          elseif n < bibtex.hard_max then
            out:write(line, '\n')
            line = ''
          else -- ``unbreakable''
            out:write(string.sub(line, 1, bibtex.hard_max-1), '%\n')
            line = string.sub(line, bibtex.hard_max)
          end
        end
        out:write(line, '\n')
      end
    end
  end
  assert(out.write, "object passed to bst.writer does not have a write method")
  return { write = write }
end
--   Functions copied from classic BibTeX
-- 
--    Adding a period
-- 
-- Find the last non-[[]] character, and if it is not a
-- sentence terminator, add a period.
-- <exported Lua functions>=
do
  local terminates_sentence = { ["."] = true, ["?"] = true, ["!"] = true }

  bst.doc.add_period = "string -> string # add period unless already .?!"
  function bst.add_period(s)
    local _, _, last = find(s, '([^%}])%}*$')
    if last and not terminates_sentence[last] then
      return s .. '.'
    else
      return s
    end
  end
end
--    Case-changing
-- 
-- Classic BibTeX has a [[change.case]] function, which
-- takes an argument telling whether to change to lower
-- case, upper case, or ``title'' case (which has initial
-- letters capitalized). Because Lua supports first-class
-- functions, it makes more sense just to export three
-- functions: [[lower]], [[title]], and [[upper]]. 
-- <exported Lua functions>=
do
  bst.doc.lower = "string -> string  # lower case according to bibtex rules"
  bst.doc.upper = "string -> string  # upper case according to bibtex rules"
  bst.doc.title = "string -> string  # title case according to bibtex rules"

  -- Case conversion is complicated by the presence of
  -- brace-delimited sequences, especially since there is
  -- one set of conventions for a ``special character''
  -- (brace-delimited sequence beginning with TeX control
  -- sequence) and another set of conventions for other
  -- brace-delimited sequences. To deal with them, we
  -- typically do an ``odd-even split'' on balanced braces,
  -- then apply a ``normal'' conversion function to the odd
  -- elements and a ``special'' conversion function to the
  -- even elements. The application is done by [[oeapp]].
  -- <utilities for case conversion>=
  local function oeapp(f, g, t)
    for i = 1, table.getn(t), 2 do
      t[i] = f(t[i])
    end
    for i = 2, table.getn(t), 2 do
      t[i] = g(t[i])
    end
    return t
  end
  -- Here is [[convert_special]]. If a special begins with
  -- an alphabetic control sequence, we convert only
  -- elements between control sequences. If a special
  -- begins with a nonalphabetic control sequence, we
  -- convert the whole special as usual. Finally, if a
  -- special does not begin with a control sequence, we
  -- leave it the hell alone. (This is the convention that
  -- allows us to put [[FORTRAN]] in a BibTeX entry and be
  -- assured that capitalization is not lost.)
  -- <utilities for case conversion>=
  function convert_special(cvt)
    return function(s)
             if find(s, '^{\\(%a+)') then
               local t = odd_even_split(s, '\\%a+')
               for i = 1, table.getn(t), 2 do
                 t[i] = cvt(t[i])
               end
               return table.concat(t)
             elseif find(s, '^{\\') then
               return cvt(s)
             else
               return s
             end
           end
  end
  -- Title conversion doesn't fit so nicely into the
  -- framework.
  -- 
  -- Function [[lower_later]] lowers all but the first
  -- letter of a string.
  -- <utilities for case conversion>=
  local function lower_later(s)
    return string.sub(s, 1, 1) .. string.lower(string.sub(s, 2))
  end

  -- Upper- and lower-case conversion are easiest.
  -- Non-specials are hit directly with [[string.lower]] or
  -- [[string.upper]]; for special characters, we use
  -- utility called [[convert_special]].
  -- <definitions of case-conversion functions>=
  local lower_special = convert_special(string.lower)
  local upper_special = convert_special(string.upper)

  function bst.lower(s)
    return table.concat(oeapp(string.lower, lower_special, brace_split(s)))
  end

  function bst.upper(s)
    return table.concat(oeapp(string.upper, upper_special, brace_split(s)))
  end
  -- For title conversion, we don't mess with a token that
  -- follows a colon. Hence, we must maintain [[prev]] and
  -- can't use [[convert_special]].
  -- <definitions of case-conversion functions>=
  local function title_special(s, prev)
    if find(prev, ':%s+$') then
      return s
    else
      if find(s, '^{\\(%a+)') then
        local t = odd_even_split(s, '\\%a+')
        for i = 1, table.getn(t), 2 do
          local prev = t[i-1] or prev
          if find(prev, ':%s+$') then
            assert(false, 'bugrit')
          else
            t[i] = string.lower(t[i])
          end
        end
        return table.concat(t)
      elseif find(s, '^{\\') then
        return string.lower(s)
      else
        return s
      end
    end
  end
  -- Internal function [[recap]] deals with the damn
  -- colons.
  -- <definitions of case-conversion functions>=
  function bst.title(s)
    local function recap(s, first)
      local parts = odd_even_split(s, '%:%s+')
      parts[1] = first and lower_later(parts[1]) or string.lower(parts[1])
      for i = (first and 3 or 1), table.getn(parts), 2 do
        parts[i] = lower_later(parts[i])
      end
      return table.concat(parts)
    end
    local t = brace_split(s)
    for i = 1, table.getn(t), 2 do -- elements outside specials get recapped
      t[i] = recap(t[i], i == 1)
    end
    for i = 2, table.getn(t), 2 do -- specials are, well, special
      local prev = t[i-1]
      if i == 2 and not find(prev, '%S') then prev = ': ' end 
      t[i] = title_special(t[i], prev)
    end
    return table.concat(t)
  end
end
--    Purification
-- 
-- Purification (classic [[purify]]) involves removing
-- non-alphanumeric characters. Each sequence of
-- ``separator'' characters becomes a single space. 
-- <exported Lua functions>=
do
  bst.doc.purify = "string -> string # remove nonalphanumeric, non-sep chars"
  local high_alpha = string.char(128) .. '-' .. string.char(255)
  local sep_white_char = '[' .. sep_chars .. '%s]'
  local disappears     = '[^' .. sep_chars .. high_alpha .. '%s%w]'
  local gsub = string.gsub
  local function purify(s)
    return gsub(gsub(s, sep_white_char, ' '), disappears, '')
  end
  -- special characters are purified by removing all non-alphanumerics,
  -- including white space and sep-chars
  local function spurify(s)
    return gsub(s, '[^%w' .. high_alpha .. ']+', '')
  end
  local purify_all_chars = { oe = true, OE = true, ae = true, AE = true, ss = true }
  
  function bst.purify(s)
    local t = brace_split(s)
    for i = 1, table.getn(t) do
      local _, k, cmd = find(t[i], '^{\\(%a+)%s*')
      if k then
        if lower_specials[cmd] or upper_specials[cmd] then
          if not purify_all_chars[cmd] then
            cmd = string.sub(cmd, 1, 1)
          end
          t[i] = cmd .. spurify(string.sub(t[i], k+1))
        else
          t[i] = spurify(string.sub(t[i], k+1))
        end
      elseif find(t[i], '^{\\') then
        t[i] = spurify(t[i])
      else
        t[i] = purify(t[i])
      end
    end
    return table.concat(t)
  end
end  
--    Text prefix
-- 
-- Function [[text_prefix]] (classic [[text.prefix]])
-- takes an initial substring of a string, with the
-- proviso that a BibTeX ``special character'' sequence
-- counts as a single character. 
-- <exported Lua functions>=
bst.doc.text_prefix = "string * int -> string  # take first n chars with special == 1"
function bst.text_prefix(s, n)
  local t = brace_split(s)
  local answer, rem = '', n
  for i = 1, table.getn(t), 2 do
    answer = answer .. string.sub(t[i], 1, rem)
    rem = rem - string.len(t[i])
    if rem <= 0 then return answer end
    if find(t[i+1], '^{\\') then
      answer = answer .. t[i+1]
      rem = rem - 1
    else
      -- <take up to [[rem]] characters from [[t[i+1]]], not counting braces>=
      local s = t[i+1]
      local braces = 0
      local sub = string.sub
      for i = 1, string.len(s) do
        local c = sub(s, i, i)
        if c == '{' then
          braces = braces + 1
        elseif c == '}' then
          braces = braces + 1
        else
          rem = rem - 1
          if rem == 0 then
            return answer .. string.sub(s, 1, i) .. string.rep('}', braces)
          end
        end
      end
      answer = answer .. s
    end
  end
  return answer
end
--    Emptiness test
-- 
-- Function [[empty]] (classic [[empty]]) tells if a
-- value is empty; i.e., it is missing (nil) or it is
-- only white space. 
-- <exported Lua functions>=
bst.doc.empty = "string option -> bool # is string there and holding nonspace?"
function bst.empty(s)
  return s == nil or not find(s, '%S')
end
--   Other utilities
-- 
--    A stable sort
-- 
-- Function [[bst.sort]] is like [[table.sort]] only
-- stable. It is needed because classic BibTeX uses a
-- stable sort. Its interface is the same as
-- [[table.sort]].
-- <exported Lua functions>=
bst.doc.sort = 'value list * compare option # like table.sort, but stable'
function bst.sort(t, lt)
  lt = lt or function(x, y) return x < y end
  local pos = { } --- position of each element in original table
  for i = 1, table.getn(t) do pos[t[i]] = i end
  local function nlt(x, y)
    if lt(x, y) then
      return true
    elseif lt(y, x) then
      return false
    else -- elements look equal
      return pos[x] < pos[y]
    end
  end
  return table.sort(t, nlt)
end
bst.doc.sort = 'value list * compare option -> unit  # stable sort'
--    The standard months
-- 
-- Every style is required to recognize the months, so we
-- make it easy to create a fresh table with either full
-- or abbreviated months.
-- <exported Lua functions>=
bst.doc.months = "string option -> table # macros table containing months"
function bst.months(what)
  local m = {
    jan = "January", feb = "February", mar = "March", apr = "April",
    may = "May", jun = "June", jul = "July", aug = "August",
    sep = "September", oct = "October", nov = "November", dec = "December" }
  if what == 'short' or what == 3 then
    for k, v in pairs(m) do
      m[k] = string.sub(v, 1, 3)
    end
  end
  return m
end
--    Comma-separated lists
-- 
-- The function [[commafy]] takes a list and inserts
-- commas and [[and]] (or [[or]]) using American
-- conventions. For example,
-- 
--   [[commafy 'Graham', 'Knuth', 'Patashnik' ]]
-- 
-- returns [['Graham, Knuth, and Patashnik']], but
-- 
--   [[commafy 'Knuth', 'Plass' ]]
-- 
-- returns [['Knuth and Plass']].
-- <exported Lua functions>=
bst.doc.commafy = "string list -> string # concat separated by commas, and"
function bst.commafy(t, andword)
  andword = andword or 'and'
  local n = table.getn(t)
  if n == 1 then
    return t[1]
  elseif n == 2 then
    return t[1] .. ' ' .. andword .. ' ' .. t[2]
  else
    local last = t[n]
    t[n] = andword .. ' ' .. t[n]
    local answer = table.concat(t, ', ')
    t[n] = last
    return answer
  end
end
-- Here are a couple of test functions I used during
-- development that I thought might be worth keeping
-- around.
-- <exported Lua functions>=


------------------------------------------------------------------------
-------------------------- TESTING AND SO ON ---------------------------
------------------------------------------------------------------------

bibtex.doc.cat = 'string -> unit # emit the named bib file in bib format'
function bibtex.cat(bib)
  local rdr = bibtex.open(bib, bst.months())
  if not rdr then
    rdr = assert(bibtex.open(assert(bibtex.bibpath(bib)), bst.months()))
  end
  for type, key, fields in entries(rdr) do
    if type == nil then
      break
    elseif not type then
      io.stderr:write('Error on key ', key, '\n')
    else
      emit_tkf.bib(io.stdout, type, key, fields)
    end
  end
  bibtex.close(rdr)
end
-- <exported Lua functions>=
bibtex.doc.count = 'string list -> unit # take list of bibs and print number of entries'
function bibtex.count(argv)
  local bibs = { }
  local macros = { }
  local n = 0
  -- If we have no arguments, search all available
  -- bibfiles. Otherwise, an argument with a [[/]] is a
  -- pathname, and an argument without [[/]] is a name as
  -- it would appear in [[[BibTeX bibliography]
  -- <make [[bibs]] the list of pathnames implied by [[argv]]>=
  if table.getn(argv) == 0 then
    bibs = all_bibs()
  else
    for _, a in ipairs(argv) do
      if find(a, '/') then
        table.insert(bibs, a)
      else
        table.insert(bibs, assert(bibtex.bibpath(a)))
      end
    end
  end
  local function warn() end
  for _, bib in ipairs(bibs) do
    local rdr = bibtex.open(bib, macros)
    for type, key, fields in entries(rdr) do
      if type == nil then
        break
      elseif type then
        n = n + 1
      end
    end
    rdr:close()
  end
  printf("%d\n", n)
end
-- <exported Lua functions>=
bibtex.doc.all_entries = "bibname * macro-table -> preamble * citation list"
function bibtex.all_entries(bib, macros)
  macros = macros or bst.months()
  warn = warn or emit_warning
  local rdr = bibtex.open(bib, macros, warn)
  if not rdr then
    rdr = assert(bibtex.open(assert(bibtex.bibpath(bib)), macros, warn),
                 "could not open bib file " .. bib)
  end
  local cs = { }
  local seen = { }
  for type, key, fields in entries(rdr) do
    if type == nil then
      break
    elseif not type then
      io.stderr:write(key, '\n')
    elseif not seen[key] then
      seen[key] = true
      table.insert(cs, { type = type, key = key, fields = fields, file = bib,
                         line = rdr.entry_line })
    end
  end
  local p = assert(rdr.preamble)
  rdr:close()
  return p, cs
end
-- <check constant values for consistency>=
assert(min_print_line >= 3)
assert(max_print_line > min_print_line)

return bibtex

@echo off

set CLEAN_QUIET=1

set LIBS=user32.lib gdi32.lib shell32.lib clock.lib
set LIBPATH=lib

set INCLUDE_DIRS_RAW=.\include
for %%i in (!INCLUDE_DIRS_RAW!) do (
  set INCLUDE_DIRS=!INCLUDE_DIRS! /I%%i
)

set IGNORED_WARNINGS_RAW= 4201 4127 4005

for %%i in (!IGNORED_WARNINGS_RAW!) do (
  set IGNORED_WARNINGS=!IGNORED_WARNINGS! /wd%%i
)

set config=%2
if not defined config (
  set config=debug
)

if "!config!"=="debug" (
  set COMMON_CFLAGS=/Zi /W4 /nologo /MTd !IGNORED_WARNINGS! /MP /DTESTING=0 /DDEBUG
) else if "!config!"=="release" (
  set COMMON_CFLAGS=/W4 /nologo /MT !IGNORED_WARNINGS! /MP /O2
) else (
  echo ERROR: Unknown configuration '!config!'...
  exit /b 1
)

echo Building for !config! config...
echo.

set arg=%1

if not defined arg (
  echo ERROR: No subcommand provided...
  exit /b 1
)

if "!arg!"=="chess" (
  call shell cl src\chess.c !COMMON_CFLAGS! !INCLUDE_DIRS! /link /LIBPATH:!LIBPATH! !LIBS!
) else if "!arg!"=="clean" (
  call shell clean exe
  call shell clean ilk
  call shell clean pdb
  call shell clean obj
) else (
  echo ERROR: Unknown subcommand !arg!...
  exit /b 1
)

call shell clean .obj

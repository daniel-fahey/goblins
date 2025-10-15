
dnl -*- Autoconf -*-

AC_INIT(guile-goblins, 0.17.0)
AC_SUBST(AUTHOR, "\"The Spritely Institute\"")
AC_SUBST(COPYRIGHT, "'(2025)")
AC_SUBST(LICENSE, asl2.0)
AC_CONFIG_SRCDIR(goblins.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 gnu silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile goblins/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

dnl Search for 'guile' and 'guild'.  This macro defines
dnl 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
fi

dnl Get directory for Guile's .scm files for use with Hoot.
AC_MSG_CHECKING(for Guile module source directory)
GUILE_SCM_DIR="`$PKG_CONFIG --print-errors --variable=pkgdatadir guile-$GUILE_EFFECTIVE_VERSION`/$GUILE_EFFECTIVE_VERSION"
AC_MSG_RESULT($GUILE_SCM_DIR)
if test "$GUILE_SCM_DIR" = ""; then
AC_MSG_FAILURE(source directory not found)
fi
AC_SUBST(GUILE_SCM_DIR)

GUILE_MODULE_REQUIRED([fibers])
GUILE_MODULE_REQUIRED([gnutls])
GUILE_MODULE_REQUIRED([web socket server])

dnl Guile-bstructs is needed for Unix domain socket netlayer support.
GUILE_MODULE_AVAILABLE([have_guile_bstructs], [(bstructs)])
AM_CONDITIONAL([HAVE_GUILE_BSTRUCTS], [test "x$have_guile_bstructs" = "xyes"])

dnl Find Hoot's reflect.js/reflect.wasm directories.
AC_MSG_CHECKING(for Hoot reflect.js directory)
HOOT_REFLECT_JS_DIR=`$GUILE -c '(display (or (false-if-exception (@ (hoot config) %reflect-js-dir)) ""))'`
AC_MSG_RESULT($HOOT_REFLECT_JS_DIR)
if test "$HOOT_REFLECT_JS_DIR" = ""; then
AC_MSG_WARN(Hoot reflect.js directory not found)
fi
AC_SUBST(HOOT_REFLECT_JS_DIR)

AC_MSG_CHECKING(for Hoot reflect.wasm directory)
HOOT_REFLECT_WASM_DIR=`$GUILE -c '(display (or (false-if-exception (@ (hoot config) %reflect-wasm-dir)) ""))'`
AC_MSG_RESULT($HOOT_REFLECT_WASM_DIR)
if test "$HOOT_REFLECT_WASM_DIR" = ""; then
AC_MSG_WARN(Hoot reflect.wasm directory not found)
fi
AC_SUBST(HOOT_REFLECT_WASM_DIR)

dnl Installation directories for .scm and .go files.
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_ARG_WITH([debug],
  [AS_HELP_STRING([--with-debug],
  [Turn off Guile compiler optimizations])],
  [],
  [with_debug=no])

AS_IF([test "x$with_debug" != xno],
  [guile_debug_flags=-O0],
  [guile_debug_flags=])
AC_SUBST([guile_debug_flags])

dnl Look up absolute file name of 'dot' executable, falling back to
dnl just 'dot' to look it up on $PATH at runtime.
AC_PATH_PROG([DOT], [dot], [dot])

AC_OUTPUT

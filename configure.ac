
dnl -*- Autoconf -*-

AC_INIT(guile-goblins, 0.11.0)
AC_SUBST(AUTHOR, "\"The Spritely Institute\"")
AC_SUBST(COPYRIGHT, "'(2022)")
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

dnl Hall auto-generated guile-module dependencies
GUILE_MODULE_REQUIRED([fibers])
GUILE_MODULE_REQUIRED([gcrypt hash])

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

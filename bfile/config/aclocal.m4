dnl ----------------------------------------------------------------------
dnl
dnl BT_MSG_CONTROL checks for msg_control member in msghdr and that
dnl the cmsg fields aren't broken...
dnl

AC_DEFUN(BT_MSG_CONTROL,
[
AC_CACHE_CHECK([for msg_control member in msghdr],
  bt_cv_have_msghdr_msg_control,
[AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/socket.h>], 
  [struct msghdr msg;
   msg.msg_control;],
  bt_cv_have_msghdr_msg_control=yes, bt_cv_have_msghdr_msg_control=no)])
if test $bt_cv_have_msghdr_msg_control = yes; then
  AC_DEFINE(HAVE_MSGHDR_MSG_CONTROL)
fi

if test $bt_cv_have_msghdr_msg_control = yes; then
  AC_MSG_CHECKING(for broken CMSG_FIELDS)
  case "$target_os" in
        linux*)
           AC_DEFINE(BROKEN_CMSG_FIELDS)
           AC_MSG_RESULT(yes)
           ;;
        *)
           AC_MSG_RESULT(no)
           ;;
  esac
fi
])

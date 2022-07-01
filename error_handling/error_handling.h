#  ifdef _VERIFY
#    undef _VERIFY
#  endif
#  ifdef _RC
#    undef _RC
#  endif
#  ifdef _FILE_
#    undef _FILE_
#  endif

#  define _FILE_ __FILE__
#  ifdef I_AM_MAIN
#     define _VERIFY(A) call verify_abort(A,_FILE_,__LINE__)
#  else
#     define _VERIFY(A) if (verify_return(A,_FILE_,__LINE__)) return
#  endif
#  define _RC rc=rc);_VERIFY(rc


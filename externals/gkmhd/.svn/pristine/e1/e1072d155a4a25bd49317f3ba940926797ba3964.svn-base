#!/bin/tcsh -ef

setenv ARCHIVE 'ar -r -u'
setenv SYS gateway
setenv DODEBUG no
unsetenv DOOPT 
setenv DOMPI no

#setenv MAKEFLAGS '-I ${MAKEFILEDIR}'
setenv MAKEFLAGS '-I ..'

make clean ; make

foreach file ( equpdate eqpostdate )
( cd $file ; ./build.sh )
end


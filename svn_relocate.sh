#!/bin/sh

# File to change the SVN repository address for the external codes 

NEWURLBASE='https://gforge-next.eufus.eu/svn'

USER=g2yyudin

CODENAMES=('ets' 'imp4dv' 'bohmgb' 'chease' 'gem0')

PATHLINES=('/' '/modtransp/trunk/' '/modtransp/' '/chease/' '/modtransp/trunk/')

BRANCHNAMES=('/tags/4.10b.10_8/' '/' '/tags/4.10b/' '/tags/4.10b.10_CHEASEv12_9/' '/')

for i in ${!CODENAMES[@]}; do

    NEWURL=${NEWURLBASE}${PATHLINES[$i]}${CODENAMES[$i]}${BRANCHNAMES[$i]}
    echo $NEWURL

    cd externals/${CODENAMES[$i]}
    svn relocate --username ${USER} ${NEWURL}
    cd ../../

done



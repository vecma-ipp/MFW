#!/bin/sh

# File to change the SVN repository address for the external codes 

NEWURLBASE=https://gforge-next.eufus.eu/svn/

USER=g2yyudin

CODENAME=imp4dv

NEWURL=${NEWURLBASE}${CODENAME}/tags/4.10b.10_8

cd externals/$CODENAME
svn relocate --username $USER $NEWURL


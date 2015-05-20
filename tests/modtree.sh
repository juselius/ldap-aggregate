#!/bin/bash

SCRIPT=`readlink -f $0`
SCRIPTDIR=`dirname $SCRIPT`

cd $SCRIPTDIR

case $1 in
    *)   f="ldif/source.update.ldif" ;;
esac

ldapmodify -x -H ldap://localhost -D cn=admin,dc=source -w secret -f $f

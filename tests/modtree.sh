#!/bin/bash

SCRIPT=`readlink -f $0`
SCRIPTDIR=`dirname $SCRIPT`

cd $SCRIPTDIR

case $1 in
    1)   f="ldif/source.update.ldif" ;;
    2)   f="ldif/source.update2.ldif" ;;
esac

ldapmodify -x -H ldap://localhost -D cn=admin,dc=source -w secret -f $f

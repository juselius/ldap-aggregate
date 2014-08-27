#!/bin/bash

echo -n "bindpw: "
read pw

ldapadd -H ldap://localhost -D cn=admin,dc=source -w $pw\
    -f ./ldif/source.bootstrap.ldif
ldapadd -H ldap://localhost -D cn=admin,dc=target -w $pw\
    -f ./ldif/target.bootstrap.ldif

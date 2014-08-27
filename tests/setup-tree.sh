#!/bin/bash

echo -n "bindpw: "
read pw

ldapadd -H ldap://localhost -D cn=admin,dc=source -w $pw\
    -f ./ldif/source.create.ldif
ldapadd -H ldap://localhost -D cn=admin,dc=target -w $pw\
    -f ./ldif/target.create.ldif

#!/bin/bash

ldapadd -H ldap://localhost -D cn=admin,dc=source -w secret\
    -f ./ldif/source.dit.ldif
ldapadd -H ldap://localhost -D cn=admin,dc=target -w secret\
    -f ./ldif/target.dit.ldif

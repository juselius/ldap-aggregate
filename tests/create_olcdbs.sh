#!/bin/bash

tmp=`sudo ls -1 /etc/ldap/slapd.d/cn=config/`
idx=`echo $tmp |sed -n 's/.*{\(-\?[0-9]\+\)}.*/\1/p' |sort -g |tail -1`

mkolcdb "source" "$((idx+1))"
mkolcdb "target" "$((idx+2))"

mkolcdb () {
    db=$1
    ix=$2
    echo "$db => hdb{$ix}"

    if [ ! -e /var/lib/ldap/$db ]; then
        sudo mkdir /var/lib/ldap/$db
        sudo cp /var/lib/ldap/DB_CONFIG /var/lib/ldap/$db
        sudo chown -R openldap:openldap /var/lib/ldap/$db
    fi

    sed "s/@ix@/$ix/g; s/@db@/$db/g" ldif/olcDatabase.ldif > /tmp/olcDb.$$
    sudo ldapadd -Y EXTERNAL -H ldapi:/// -f /tmp/olcDb.$$
    rm /tmp/olcDb.$$
}

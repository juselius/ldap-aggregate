#!/bin/bash


case $1 in
    "source" | "s") b="dc=source" ;;
    "target" | "t") b="dc=target" ;;
    *)              b="dc=target" ;;
esac

ldapsearch -x -H ldap://localhost -b "$b"

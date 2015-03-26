# LDAP Aggregate

ldap-aggregate is a tool to transfer select updates in one LDAP tree to new
locations in another tree, including rewriting of attributes.

## Install

ldap-aggregate is written in Haskell, and requires the Haskell Platform
(http://haskell.org/platform) to be installed on the system. ldap-aggregate is
configured and built using cabal:

    $ git clone git@source.uit.no/hpc/ldap-aggregate
    $ cd ldap-aggregate
    ($ cabal update)
    $ cabal confiure --prefix=/opt
    $ cabal build
    $ cabal test
    $ cabal install

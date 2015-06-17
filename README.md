# ldap-aggregate

ldap-aggregate gathers records from multiple LDAP sources into a target
tree. The source records are filtered and rewritten using a regular expression
based configuration format. At startup ldap-aggreagte does a full initial
sweep of the source and target trees, and then enters a polling loop
requesting only changed records at regular intervals.

ldap-aggreagte is configured using a YML based configuration file. There
are example configurations in the ``examples/`` directory.

## Install

ldap-aggregate is written in Haskell, and requires a recent version of the
Glasgow Haskell Compiler (> ghc-7.10.0) and Cabal (> cabal-1.22.0) to
be installed on the system. To install ``ghc`` and ``cabal``, follow the
instructions at [Stackage](http://www.stackage.org/install). The repository
has been configured to automatically use Stackage package repository in order
to avoid dependency problems.

To build and install ldap-aggreagte:

    $ git clone git@github.com/juselius/ldap-aggregate
    $ cd ldap-aggregate
    $ cabal update
    $ cabal confiure --prefix=/opt
    $ cabal install --only-dependencies
    $ cabal build
    $ cabal test
    $ cabal install

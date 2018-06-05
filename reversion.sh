#!/bin/bash

## + requires perl-reversion from Perl::Version (debian package libperl-version-perl)
## + example call:
##    ./reversion.sh -bump -dryrun

pmfiles=(./lib/word2vec/Model.pm ./lib/word2vec/Kcluster.pm)

exec perl-reversion "$@" "${pmfiles[@]}"

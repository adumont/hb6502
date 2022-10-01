#!/bin/sh

REPO=$( git config --get remote.origin.url | cut -d: -f2 | sed -e 's/\.git//' )
BRANCH=$( git symbolic-ref --short HEAD )
SHA=$( git rev-parse --short HEAD )
DATE=$( git log -1 --format=%cd --date=format:"%y%m" )
YEAR=$( git log -1 --format=%cd --date=format:"%Y" )
#DATE=$( date +%y%m )
NUMB=$( git log --oneline | wc -l )

#echo "$REPO($BRANCH):$SHA-$DATE-$NUMB" > .version

#VERSION="$DATE-$NUMB"
VERSION="$DATE-$SHA"

if [ "$BRANCH" != "main" ]
then
    VERSION="$VERSION($BRANCH)"
fi

if [ ! -z "$( git status --porcelain -uno )" ]
then
    VERSION="$VERSION*"
fi

echo "VERS_STR: CString {\"ALEXFORTH $VERSION\", \$0A, \$0D, \"(c) 2021-$YEAR Alex Dumont\", \$0A, \$0D}"

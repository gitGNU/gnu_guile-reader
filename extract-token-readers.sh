#!/bin/sh
#
# Write a list of the built-in token reader names, looking at the given source
# files, in a way that is suitable as a `gperf' input.

if test "$1" != "--no-gperf-header"
then
  echo '%{'
  echo '#include "reader.h"'
  echo '#include "token-readers.h"'
  echo '%}'
  echo
  echo 'struct scm_token_reader_entry { const char *name; const void *reader; };'
  echo '%%'
else
  shift
fi

cat $@ | \
grep '^ *SCM_DEFTOKEN' | \
sed -es'/^ *SCM_DEFTOKEN_\([A-Z_]\+\) *(.\+, *"\([^"]\+\)", *\([a-zA-Z0-9_]\+\).*$/\2, \3/g'

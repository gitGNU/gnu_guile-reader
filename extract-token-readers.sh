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
  echo 'struct scm_token_reader_entry { const char *name; scm_token_reader_spec_t reader; };'
  echo '%%'
else
  shift
fi

grep '^#define SCM_TR_' | \
sed -es'/^#define SCM_TR_\([A-Z0-9_]\+\) \/\* \([^ ]\+\) \*\/.*$/\2, SCM_TR_\1/g'

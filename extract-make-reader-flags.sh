#!/bin/sh
#
# Write a list of the built-in token reader names, looking at the given source
# files, in a way that is suitable as a `gperf' input.

if test "$1" != "--no-gperf-header"
then
  echo '%{'
  echo '#include "reader.h"'
  echo '%}'
  echo
  echo 'struct scm_reader_flag_entry { const char *name; unsigned flag; };'
  echo '%%'
else
  shift
fi

cat "reader.h" | \
grep '^#define SCM_READER_FLAG_' | \
sed -es'/^#define SCM_READER_FLAG_\([A-Z0-9_]\+\).*\/\* \([^ ]\+\) \*\/.*$/\2, SCM_READER_FLAG_\1/g'

# arch-tag: 14e80f57-8f11-4adb-8276-df7da2254daf

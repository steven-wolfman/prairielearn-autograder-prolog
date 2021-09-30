#!/usr/bin/env bash

# TODO: documentation!
# Bash script template based on https://betterdev.blog/minimal-safe-bash-script-template/

set -Eeuo pipefail
shopt -s globstar extglob nullglob

LIBRARY_LOCATION="serverFilesCourse/prolog-grader"
FILE_LOCATION="test/code.pl"

USAGE=$(cat <<EOF
Usage: $(basename "${BASH_SOURCE[0]}") [-h] [<filename>]

Runs Prolog testing on <filename>, including attempting to load and run its associated
tests. (Specifically, if <filename> is <x>.pl, the associated tests are in <x>.plt.)
Also attempts to set up a Prolog path alias named grader to the autograder's folder
if it can find one named "${LIBRARY_LOCATION}" in any ancestor of the current folder.

<filename> defaults to "${FILE_LOCATION}"

Available options:

-h, --help      Print this help and exit
EOF
)

usage() {
  echo "${USAGE}"
  exit
}

msg() {
  echo >&2 -e "${1-}"
}

die() {
  local msg=$1
  local code=${2-1} # default exit status 1
  msg "$msg"
  exit "$code"
}

dieUI() {
  local msg=$1
  local code=${2-1}
  msg "$msg"
  msg "\n${USAGE}"
  exit "$code"
}

parse_params() {
  while :; do
    case "${1-}" in
    -h | --help) usage ;;
    -?*) dieUI "Unknown option: $1" ;;
    *) break ;;
    esac
    shift
  done

  # Optional argument
  if [[ $# -gt 0 ]]
  then
    FILE_LOCATION="${1-}"
    msg "Using specified code file: ${FILE_LOCATION}"
    shift
  else
    msg "Using default code file: ${FILE_LOCATION}"
  fi

  # Check for disallowed arguments:
  [[ $# -gt 0 ]] && dieUI "Too many arguments provided"

  return 0
}

parse_params "$@"

# script logic here

# Adapted from https://unix.stackexchange.com/a/293477:
findconfig() {
  # Adapted from: https://www.npmjs.com/package/find-config#algorithm
  # 1. If X/$1 exists and is a folder, return it. STOP
  # 2. If X has a parent directory, change X to parent. GO TO 1
  # 3. Return NULL.

  if [ -d "$1" ]; then
    printf '%s' "${PWD%/}/$1"
  elif [ "$PWD" = / ]; then
    false
  else
    # a subshell so that we don't affect the caller's $PWD
    (cd .. && findconfig "$1")
  fi
}

if [[ ! -f "${FILE_LOCATION}" ]]
then
    die "No code file found at: ${FILE_LOCATION}"
fi

LIBRARY_PATH="$(findconfig "${LIBRARY_LOCATION}" || die "Cannot find \"${LIBRARY_LOCATION}\" in any ancestor.")"

msg "Found library at \"${LIBRARY_PATH}\""


# Now call swipl:
#swipl -p grader="${LIBRARY_PATH}" -g 'load_test_files([]), run_tests' -t halt "${FILE_LOCATION}"
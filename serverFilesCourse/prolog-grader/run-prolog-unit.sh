#! /bin/bash

# TODO: deal with the .hs processing below, also marked with TODO

##### SETUP #####
echo "Setting up..."

GRADER_NAME='grader_prolog_unit.py'
LIBRARY_NAME='plplunit.pl'
JOB_DIR='/grade/'
STUDENT_DIR=$JOB_DIR'student/'
SHARED_DIR=$JOB_DIR'shared/'
TEST_DIR=$JOB_DIR'tests/'
PROLOG_GRADER_DIR=$SHARED_DIR'prolog-grader/'
MUSTACHE_PROCESSOR=$PROLOG_GRADER_DIR'mustache-process.sh'

# Student files, test files, and shared files will be merged into this directory
AG_DIR=$JOB_DIR'run/'
# Results should end up in this directory
RESULTS_DIR=$JOB_DIR'results/'

mkdir $AG_DIR
mkdir $RESULTS_DIR

# Copy tests and student code into the run directory and perform mustache processing on them
cp -av $TEST_DIR. $AG_DIR

echo "Performing mustache processing."
if [[ -f $MUSTACHE_PROCESSOR ]]
then
    chmod u+x $MUSTACHE_PROCESSOR
    (cd $AG_DIR && $MUSTACHE_PROCESSOR)
fi
echo "Mustache processing complete."

# Copy student code into the run directory
cp -rv $STUDENT_DIR. $AG_DIR'src/'

# Copy the grader script and testing library into the run directory
cp -v $PROLOG_GRADER_DIR$GRADER_NAME $AG_DIR
cp -v $PROLOG_GRADER_DIR$LIBRARY_NAME $AG_DIR

# give the ag user ownership of the run folder
/usr/bin/sudo chown -R ag $AG_DIR
/usr/bin/sudo chmod -R +rw $AG_DIR


##### EXECUTION #####
echo "Starting grading..."

cd $AG_DIR

# Construct the code.pl file to perform testing on.
cat src/code-header.pl src/code.pl > foo.pl
mv foo.pl src/code.pl

# Move the testing file into place as well.
mv test/code.plt src/code.plt


# run the autograder as non-root
# THIS IS IMPORTANT
# we do the capturing ourselves, so that only the stdout of the autograder is used and that we aren't relying on any files that the student code could easily create
# we are also running the autograder as a limited user called ag
/usr/bin/sudo -H -u ag bash -c "python3 $GRADER_NAME" > results.json

# get the results from the file
cp $AG_DIR'results.json' $RESULTS_DIR'results.json'

echo "Grading complete!"

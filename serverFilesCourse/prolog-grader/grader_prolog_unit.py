import os
import re
import json
import subprocess
import tempfile

TEST_START_PREFIX = "Begin PrairieLearn Prolog Test: "
GROUP_START_PREFIX = "Begin PrairieLearn Prolog Test Group: "
GROUP_END_PREFIX = "End PrairieLearn Prolog Test Group: "

SWIPL_RUN_COMMAND = [
    '/usr/bin/swipl', 
    '-p', 'grader=/grade/run',
    '-g', 'load_test_files([]), run_tests',
    '-t', 'halt',
    'src/code.pl'
]

def process(lines):
    grading_result = {}

    # States
    # prelude = Before the first group
    # group = Inside a group, collecting its contents (ended by closing group tag)
    # test = Inside a test with no error/warning, collecting its contents (ended by next tag, should be closing group or next test start)
    # error = Inside a test with at least one error, collecting its contents (ended by next tag, as above)
    # warning = Inside a test with no error and at least one warning, collecting its contents (ended by next tag, as above)
    # nogroup = Outside a group but after the first one appeared

    state = "prelude"

    tests_results_dict = {'Pass':{}, 'Fail':{}}

    groupName = ""
    testName = ""
    testRequired = False
    message = ""
    points = 0
    allFail = False
    debug = True

    for line in lines:
        if debug:
           print(state, line)

        groupStart = re.match(" *{}=G= (?P<name>.*)".format(GROUP_START_PREFIX), line)
        groupEnd = re.match(" *{}=G= (?P<name>.*)".format(GROUP_END_PREFIX), line)
        testStart = re.match(" *{}=(?P<type>[RP])= (?P<name>.*) \((?P<points>[0-9]+) points?\).*".format(TEST_START_PREFIX), line)
        errorIndicator = re.match(" *ERROR: .*", line)
        warningIndicator = re.match(" *Warning: .*", line)

        if groupStart:
            # We want to start the group, but to do so, we should be in one of:
            #   prelude
            #   test/error/warning, nogroup
            #   and we should set the current group name
            #   and transition to group
            if debug:
               print("groupStart")

            # Errors:
            if state == "group":
                raise RuntimeError("Error: group \"{}\"contained within group \"{}\"".format(groupStart.group('name'),groupName))

            # Standard behaviour:
            groupName = groupStart.group('name')
            state = "group"
        elif groupEnd:
            # We want to end the group, but to do so, we should be in one of:
            #   test/error/warning, group
            #   and the group name in groupEnd should match the current group
            #   and we should wrap up the previous test/error/warning (if any)
            #   and we should clear the group name
            #   and we should transition to nogroup
            if debug:
               print("groupEnd")

            # Errors:
            if state == "nogroup" or state == "prelude":
                raise RuntimeError("Error: encountered group end for group \"{}\" while not in a group context".format(groupEnd.group('name')))
            if groupName != groupEnd.group('name'):
                raise RuntimeError("Error: encountered group end for group \"{}\" while in group \"{}\"".format(groupEnd.group('name'), groupName))
            
            # Test wrapup:
            if state == "warning" or state == "error":
                tests_results_dict['Fail'][testName] = (message,points)
                message = ""
                if testRequired:
                    allFail = True
            elif state == "test":
                tests_results_dict['Pass'][testName] = points
            elif debug:
                print("no test to wrap up")
            testName = ""
            testRequired = False
            points = 0
            message = ""

            # Standard behaviour:
            groupName = ""
            state = "nogroup"
        elif testStart:
            # We want to start a new test, but to do so, we should be in one of:
            #   test/error/warning, group
            #   and we should wrap up the previous test/error/warning (if any)
            #   and we should start a new test, storing its name, points, and type
            #   and we should transition to test
            if debug:
               print("testStart")

            # Errors:
            if state == "nogroup" or state == "prelude":
                raise RuntimeError("Error: encountered test start for test \"{}\" while not in a group context".format(testStart.group('name')))
            
            # Test wrapup:
            if state == "warning" or state == "error":
                tests_results_dict['Fail'][testName] = (message,points)
                message = ""
                if testRequired:
                    allFail = True
            elif state == "test":
                tests_results_dict['Pass'][testName] = points
            elif debug:
                print("no test to wrap up")
            testName = ""
            testRequired = False
            points = 0
            message = ""

            # Standard behaviour:
            testName = groupName + " / " + testStart.group('name')
            testRequired = testStart.group('type') == "R"
            points = int(testStart.group('points'))
            state = "test"
        elif errorIndicator or warningIndicator:
            # If we're in nogroup or prelude, we ignore.
            # Otherwise, an error/warning occurred. We need to record it, but to do so, we should be in one of:
            #   test/error/warning
            #   we should collect this line
            #   and we should transition to: error if already in error, otherwise the indicated state (error or warning)
            if debug:
               print("errorIndicator")

            # Errors:
            if state == "group":
                raise RuntimeError("Error: encountered test error in group \"{}\" but outside any test".format(groupName))
            
            # Standard behaviour:
            if state == "test" or state == "error" or state == "warning":
                message = message + "\n" + line
                if errorIndicator:
                    state = "error"
                elif warningIndicator and state != "error":
                    state = "warning"
        else:
            # Some other type of line, which is fine in any case at all.
            # But, if we're in the midst of a test, we should record this in the message.
            if state == "test" or state == "warning" or state == "error":
                message = message + "\n" + line

    if state == "prelude":
        raise RuntimeError("Error: encountered end of file before any group")
    elif state == "group":
        raise RuntimeError("Error: encountered end of file during group \"{groupName}\"")
    elif state == "test" or state == "warning" or state == "error":
        raise RuntimeError("Error: encountered end of file during test \"{testName}\"")
    
    #The final structure of tests_results_dict:
    #      {'Pass': {'Name1':Points1,'Name2':Points2},
    #       'Fail': {'Name3': ('Message3',Points3),
    #                'Name4': ('Message4',Points4)}
    #Final score calculation
    test_results = []
    total_points = 0
    earned_points = 0

    #Count the passed tests
    for (key,points) in tests_results_dict['Pass'].items():
        results = {}
        results['name'] = key
        results['max_points'] = points
        results['points'] = points

        total_points += points
        earned_points += points

        test_results.append(results)

    #Count the failed tests, and output the error message
    for (key,(message,points)) in tests_results_dict['Fail'].items():
        results = {}
        results['name'] = key
        results['max_points'] = points
        results['points'] = 0
        total_points += points

        results['message'] = message

        test_results.append(results)


    grading_result['tests'] = test_results

    grading_result['succeeded'] = True
    if total_points > 0:
        tscore = float(earned_points) / float(total_points)
    else:
        tscore = 0
    grading_result['score'] = tscore

    if allFail:
        grading_result['score'] = 0

    return grading_result

def main():

    grading_result = {}

    # Attempt to compile student code
    try:
        #'stack test' would compile and run tests cases at the same time
        subprocess.check_output(SWIPL_RUN_COMMAND, stderr=subprocess.STDOUT, timeout=60)

    except subprocess.CalledProcessError as e:
        # Run failed :(
        if re.search("{}=G=".format(GROUP_START_PREFIX),e.output.decode('utf-8')):
            grading_result['succeeded'] = True
            # we're okay, the exit code is from a test failure
        else:
            grading_result['succeeded'] = False
            grading_result['score'] = 0.0
            grading_result['message'] = 'Test run failed with no test output, with error code {}. Please check your code for infinite recursion or syntax errors.'.format(e.returncode)
            grading_result['output'] = e.output.decode('utf-8')

            print(json.dumps(grading_result))
            return
    except subprocess.TimeoutExpired:
        grading_result['succeeded'] = False
        grading_result['score'] = 0.0
        grading_result['message'] = 'Your code timed out. Check for infinite recursion!'

        print(json.dumps(grading_result))
        return
    except Exception as e:
        grading_result['succeeded'] = False
        grading_result['score'] = 0.0
        grading_result['message'] = 'Unspecified error; contact course staff: ' + str(e)

        print(json.dumps(grading_result))
        return
    # Student code ran successfully!

    #read the output of terminal
    #Citation: fix the bug of subprocess.Popen for large output https://stackoverflow.com/questions/4408377/how-can-i-get-terminal-output-in-python
    with tempfile.TemporaryFile() as tempf:
        proc = subprocess.Popen(SWIPL_RUN_COMMAND, stdout=tempf)
        proc.wait(timeout=60)
        tempf.seek(0)
        tests_results_raw = tempf.read().decode().split('\n')

    #reorganized the raw test results: clean the empty strings and generate a well-organized dictionary
    tests_results_itmd = list(filter(lambda s : s != '', tests_results_raw))

    grading_result = process(tests_results_itmd)

    # Write the grading results to stdout
    print(json.dumps(grading_result))

if __name__ == '__main__':
    main()

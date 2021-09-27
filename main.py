#!/usr/bin/env python3.5

import os
import sys
import stat
import json
import requests

from datetime import datetime
from subprocess import call
from string import Template

# specifies environment variables that should be forwarded to the child process
# that executes the grading script
ENV_WHITELIST = ['PATH', 'HOME']

# Set the PATH for sharing to the child process.
# Among other things, stack will use /home/ag/.local/bin for files.
# This previously included the direct path to ghc's binaries. That
# should not be necessary with stack. Just in case, it was:
# :/home/ag/.stack/programs/x86_64-linux/ghc-8.2.2/bin/
# That would need updating whenever the LTS was changed, however!
os.environ['PATH'] = '/usr/local/bin:/home/ag/.local/bin:' + os.environ['PATH']

def error(message):
    log(Template('ERR: $message').substitute(message=message))
    sys.stdout.flush()

def warn(message):
    log(Template('WARN: $message').substitute(message=message))
    sys.stdout.flush()

def log(message):
    print(Template('[main] $message').substitute(message=message))
    sys.stdout.flush()


def finish(succeeded, info):
    """
    This function will attempt to read any results file, add our metadata to it,
    write it to disk, send everything to S3, and exit the program.

    If succeeded is false, we'll assume things failed badly enough that we shouldn't
    even try to read /grade/results/results.json.
    """

    if info['job_id'] is None:
        # Someone screwed up bad; without a job ID, we can't upload archives to
        # S3 or notify the webhook that we've failed
        error('job_id was not specified, so it\'s impossible to upload an archive')
        sys.exit(1)

    data = None
    if succeeded:
        try:
            with open('/grade/results/results.json') as json_data:
                data = json.load(json_data)
        except Exception:
            succeeded = False
            pass

    results = {}
    results['results'] = data
    results['job_id'] = info['job_id']
    results['start_time'] = info['start_time']
    results['end_time'] = datetime.utcnow().isoformat()
    results['succeeded'] = succeeded

    # Note the distinction between grade/results/results.json and grade/results.json
    # The former is generated by the code that we're calling and will store
    # test results; the latter will hold the results file that we'll generate
    # that contains both test results and some addition metadata
    with open('grade/results.json', 'w+') as results_file:
        json.dump(results, results_file)

    if not info['dev_mode']:
        # Now we can upload this results file to AWS
        if info['results_bucket']:
            s3_results_file = Template('s3://$bucket/job_$job.json').substitute(bucket=info['results_bucket'], job=info['job_id'])
            s3_results_push_ret = call(['aws', 's3', 'cp', '/grade/results.json', s3_results_file])
            if s3_results_push_ret != 0:
                error('could not push results to S3')
        else:
            error('the results bucket was not specified')

        # Now let's make an archive of the /grade directory and send it back to S3
        # for storage
        if info['archives_bucket']:
            zip_ret = call(['tar', '-zcf', '/archive.tar.gz', '/grade/'])
            if zip_ret != 0:
                error('error zipping up archive')
            else:
                s3_archive_file = Template('s3://$bucket/job_$job.tar.gz').substitute(bucket=info['archives_bucket'], job=info['job_id'])
                s3_archive_push_ret = call(['aws', 's3', 'cp', '/archive.tar.gz', s3_archive_file])
                if s3_archive_push_ret != 0:
                    error('could not push archive to S3')
        else:
            error('the archives bucket was not specified')

        # Finally, notify the webhook (if specified) that this grading job is done
        if info['webhook_url']:
            headers = {'Content-Type': 'application/json'}
            if info['csrf_token']:
                log('CSRF Token: %s' % info['csrf_token'])
                headers['x-csrf-token'] = info['csrf_token']

            # Wrap the results inside one more level of json
            # This makes the data field optional; job_id on the outer object is still mandatory
            final_data = {}
            final_data['data'] = results
            final_data['event'] = 'grading_result'
            final_data['job_id'] = info['job_id']

            r = requests.post(info['webhook_url'], data=json.dumps(final_data), headers=headers)

    # We're all done now.
    sys.exit(0 if succeeded else 1)

def main():
    log('started')

    info = {}

    # We use the ISO-formatted string because python doesn't know how to
    # serialize datetimes to json values by default
    info['start_time'] = datetime.utcnow().isoformat()

    environ_error = False
    dev_mode = False

    if 'DEV_MODE' in os.environ and os.environ['DEV_MODE']:
        info['dev_mode'] = True
        dev_mode = True
    else:
        info['dev_mode'] = False
        dev_mode = False

    if 'JOB_ID' not in os.environ:
        error('job id was not specified in the JOB_ID environment variable')
        info['job_id'] = None
        environ_error = True
    else:
        try:
            info['job_id'] = int(os.environ['JOB_ID'])
        except:
            error('could not parse JOB_ID environment variable to an int')
            environ_error = True

    if 'ENTRYPOINT' not in os.environ:
        error('entrypoint was not specified in the ENTRYPOINT environment variable')
        info['entrypoint'] = None
        environ_error = True
    else:
        info['entrypoint'] = os.environ['ENTRYPOINT']

    # These will only be present and useful when running on AWS infrastructure
    if not dev_mode:
        if 'S3_JOBS_BUCKET' not in os.environ:
            error('the S3 jobs bucket was not specified in the S3_JOBS_BUCKET environment variable')
            info['jobs_bucket'] = None
            environ_error = True
        else:
            info['jobs_bucket'] = os.environ['S3_JOBS_BUCKET']

        if 'S3_RESULTS_BUCKET' not in os.environ:
            error('the S3 results bucket was not specified in the S3_RESULTS_BUCKET environment variable')
            info['results_bucket'] = None
            environ_error = True
        else:
            info['results_bucket'] = os.environ['S3_RESULTS_BUCKET']

        if 'S3_ARCHIVES_BUCKET' not in os.environ:
            error('the S3 archives bucket was not specified in the S3_ARCHIVES_BUCKET environment variable')
            info['archives_bucket'] = None
            environ_error = True
        else:
            info['archives_bucket'] = os.environ['S3_ARCHIVES_BUCKET']

        if 'WEBHOOK_URL' not in os.environ:
            warn('the webhook callback url was not specified in the WEBHOOK_URL environment variable')
            info['webhook_url'] = None
        else:
            info['webhook_url'] = os.environ['WEBHOOK_URL']

        if 'CSRF_TOKEN' not in os.environ:
            warn('a csrf token was not specified in the CSRF_TOKEN environment variable')
            info['csrf_token'] = None
        else:
            info['csrf_token'] = os.environ['CSRF_TOKEN']

    if environ_error:
        finish(False, info)

    job_id = info['job_id']

    log(Template('running job $job').substitute(job=job_id))

    if not dev_mode:
        # Notify PL that grading has been started
        # force
        if info['webhook_url']:
            headers = {'Content-Type': 'application/json'}
            if info['csrf_token']:
                log('CSRF Token: %s' % info['csrf_token'])
                headers['x-csrf-token'] = info['csrf_token']

            data = {}
            data['start_time'] = info['start_time']

            final_data = {}
            final_data['data'] = data
            final_data['event'] = 'grading_start'
            final_data['job_id'] = info['job_id']

            r = requests.post(info['webhook_url'], data=json.dumps(final_data), headers=headers)

        # Load the job archive from S3
        jobs_bucket = info['jobs_bucket']
        s3_job_file = Template('s3://$bucket/job_$job.tar.gz').substitute(bucket=jobs_bucket, job=job_id)
        s3_fetch_ret = call(['aws', 's3', 'cp', s3_job_file, '/job.tar.gz'])
        if s3_fetch_ret != 0:
            error('failed to load the job files from S3')
            finish(False, info)

        # Unzip the downloaded archive
        unzip_ret = call(['tar', '-xf', '/job.tar.gz', '-C', 'grade'])
        if unzip_ret != 0:
            error('failed to unzip the job archive')
            finish(False, info)

    entrypoint = info['entrypoint']

    # We need to construct a new set of environment variables from the whitelist
    env = {}
    for key in ENV_WHITELIST:
        env[key] = os.environ[key]

    if os.path.isfile(entrypoint):
        chmod_ret = call(['chmod', '+x', entrypoint])
        if chmod_ret != 0:
            error(Template('Could not make $file executable').substitute(file=entrypoint))
        else:
            run_ret = call([entrypoint], env=env, shell=True)
            if run_ret != 0:
                error(Template('error executing $file').substitute(file=entrypoint))
                finish(False, info)
    else:
        error(Template('$file not found').substitute(file=entrypoint))
        finish(False, info)

    # If we got this far, we (probably) succeeded!
    log('finishing')
    finish(True, info)

# if __name__ == '__main__':
#     main()

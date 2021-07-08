#!/usr/bin/env python

import sys
import boto3
import botocore
import time
import concurrent.futures
import io
import os
import queue
from fabric import Connection
import patchwork.transfers
import invoke.exceptions
import tempfile
# import paramiko.rsakey
import json
from invoke import run as local
from functools import reduce

# # DEBUGGING:
# import logging
# logging.basicConfig(level=logging.DEBUG)


#### Environment/arguments: ####

# We'll upload reports to: 
#   f"{RESULTS_S3_BUCKET}/mono-pr-{PR_NUMBER}/{benchmark_set}.json"
# Result sets can be retreived efficiently with 
#   s3.list_objects(Bucket=RESULTS_S3_BUCKET, Prefix=f"{THIS_S3_BUCKET_PREFIX}/")['Contents'] 
PR_NUMBER = os.environ['PR_NUMBER']
THIS_S3_BUCKET_PREFIX = f"mono-pr-{PR_NUMBER}"

PR_TARGET_BRANCH = os.environ['PR_TARGET_BRANCH']

# The image file we'll ship to the benchmark runner to test:
# You can pull get a local image file from docker with e.g.:
#    docker image save -o /tmp/hasura-blah.tar hasura/graphql-engine:latest
HASURA_DOCKER_IMAGE = os.environ['HASURA_DOCKER_IMAGE']
if not os.path.isfile(HASURA_DOCKER_IMAGE):
    sys.exit(f"Could not find a docker image file at {HASURA_DOCKER_IMAGE}")

# For boto3:
AWS_ACCESS_KEY_ID = os.environ['BENCHMARKS_AWS_ACCESS_KEY_ID']
AWS_SECRET_ACCESS_KEY = os.environ['BENCHMARKS_AWS_SECRET_ACCESS_KEY']

#### Globals: ####

# This key just has privileges to SSH into the benchmarks runner instance.
# This variable is the entire PEM file (hasura-benchmarks-runner.pem) as a
# string:
BENCHMARKS_RUNNER_PRIVATE_KEY = os.environ['BENCHMARKS_RUNNER_PRIVATE_KEY']

# Keep track of EC2 instances launched in 'run_benchmark_set' so we can
# clean them up on exit/error (NOTE: 'atexit' is useless for this):
LAUNCHED_INSTANCES = queue.Queue()

# Global mutable to allow us to abort restarts when the user initiates a shutdown (e.g. CTRL-C)
SHUTTING_DOWN = False

RESULTS_S3_BUCKET = 'hasura-benchmark-results'
# NOTE: Reports uploaded will be PUBLICLY ACCESSIBLE at:
def s3_url(filename, bucket_prefix=THIS_S3_BUCKET_PREFIX):
    return f"https://{RESULTS_S3_BUCKET}.s3.us-east-2.amazonaws.com/{bucket_prefix}/{filename}"

# We'll write to this, CI will look for it and insert it into the PR comment thread:
REGRESSION_REPORT_COMMENT_FILENAME = "/tmp/hasura_regression_report_comment.md"


def main():
    try:
        benchmark_sets_basepath = abs_path("benchmark_sets")
        benchmark_sets = os.listdir(benchmark_sets_basepath)
        # print(os.listdir(os.path.join(benchmark_sets_basepath, "chinook")))

        # Start all the instances we need and run benchmarks in parallel
        # NOTE: ProcessPoolExecutor doesn't work with shared queue
        with concurrent.futures.ThreadPoolExecutor() as executor:
             bench_result = executor.map(run_benchmark_set, benchmark_sets)

        # This just surfaces any exceptions from concurrent.futures; we might
        # do something else with these later:
        #   http://docs.pyinvoke.org/en/latest/api/runners.html#invoke.runners.Result
        print(list(bench_result))

        report, merge_base_pr = generate_regression_report()
        pretty_print_regression_report_github_comment(
            report,
            merge_base_pr,
            REGRESSION_REPORT_COMMENT_FILENAME
        )
        say("Success!")

    # NOTE: without this 'except', any exceptions are silently swallowed... ugh
    except Exception as e:
        raise
    finally:
        global SHUTTING_DOWN
        SHUTTING_DOWN = True
        print("Cleaning up resources and shutting down:")
        for i in list(LAUNCHED_INSTANCES.queue):
            print(f"  * {i.instance_id}")
            # NOTE: this is idempotent, so okay to run on success
            i.terminate()
        print("")
    sys.exit()

def new_boto_session():
    # For other auth options:
    #     https://boto3.amazonaws.com/v1/documentation/api/latest/guide/credentials.html 
    # NOTE: we must start a new 'Session' per thread:
    return boto3.Session(
        aws_access_key_id=AWS_ACCESS_KEY_ID,
        aws_secret_access_key=AWS_SECRET_ACCESS_KEY
    )

# Start an EC2 instance that will run 'benchmark_set'. use_spot indicates
# whether we should try to start spot instances. If use_spot=True we'll retry
# once in case this fails, with an on-demand instance instead
def run_benchmark_set(benchmark_set, use_spot=True):
    # Prefer this instead of 'print()' so we can follow along with concurrent execution:
    def say(s):
        print(f"*** \033[92m {benchmark_set}: {s} \033[0m")
    def warn(s):
        print(f"*** \033[93m {benchmark_set}: {s} \033[0m")
    boto3_session = new_boto_session()
    # boto3_session = boto3.Session(profile_name='benchmarks')
    ec2_client = boto3_session.client('ec2', region_name='us-east-2')
    ec2 = boto3_session.resource('ec2', region_name='us-east-2')
    s3 = boto3_session.client('s3')

    # Get benchmark-runner AMI (see README_AMI.md)
    runner_images_dirty = ec2_client.describe_images(Filters=[{'Name':'tag:Name', 'Values':['hasura-benchmarks-runner']}])['Images']
    if len(runner_images_dirty) > 1:
        sys.exit("More than one instance tagged 'hasura-benchmarks-runner'; please delete tag from old image")
    elif len(runner_images_dirty) == 0:
        sys.exit("The 'hasura-benchmarks-runner' image needs to be copied to this region.")

    runner_image_id = runner_images_dirty[0]['ImageId']

    # We can and do run into capacity issues. Try our best to find a region
    # with spot availability (much cheaper), else try on-demand price
    spot = {
               'MarketType': 'spot',
               'SpotOptions': {
                   # A bit over the on-demand price
                   # NOTE: at time of this writing spot price has been very stable around $0.35/hr
                   'MaxPrice': '1.80',
                   'SpotInstanceType': 'one-time',
                   'InstanceInterruptionBehavior': 'terminate'
               }
           }
    # Regions in which we can run benchmarks, in order (approximately) from
    # cheap to more expensive. With c4.8xlarge we run into capacity issues from
    # time to time.
    # NOTE: if you want to add a new region here you'll need to copy the
    # hasura-benchmarks-runner AMI, security group, and keypair to that region
    # also.
    ok_regions = [
        "us-east-2",
        "us-west-2",
        "ap-south-1",
        "ca-central-1",
        "eu-west-2",
        "eu-west-1",
    ]
    # the sequence of spot/on-demand requests we'll make:
    market_types = [spot, {}, {}, {}, "FAIL"] if use_spot else [{}, {}, {}, "FAIL"]
    def launch_instance():
        # We'll try on-demand instances three times, hoping capacity gets added, before giving up
        for market_type in market_types:
            for region in ok_regions:
                if market_type == "FAIL":
                    sys.exit("All regions are out of capacity! We'll just need to wait and try again, sorry.")

                market_type_str = "on-demand" if market_type == {} else "spot"
                say(f"Trying to launch in {region} as {market_type_str}")

                try:
                    # Launch beefy ec2 instances that will run the actual benchmarks:
                    instance = ec2.create_instances(
                        ImageId=runner_image_id,
                        MinCount=1, MaxCount=1,
                        # NOTE: benchmarks are tuned very specifically to this instance type  and
                        # the other settings here (see bench.hs):
                        InstanceType='c4.8xlarge',
                        KeyName='hasura-benchmarks-runner',
                        InstanceInitiatedShutdownBehavior='terminate',
                        # Disable hyperthreading:
                        CpuOptions={
                            'CoreCount': 18,
                            'ThreadsPerCore': 1
                        },
                        # AFAICT this is always true for c4 instances and comes at no additional
                        # charge, but the console shows 'false' if we don't set this...
                        EbsOptimized=True,
                        InstanceMarketOptions=market_type,
                        TagSpecifications=[{
                            'ResourceType': 'instance',
                            'Tags': [
                               # Informational. This will show up in console:
                               {'Key': 'Name',
                                'Value': 'hasura-benchmarks-runner-'+benchmark_set
                               },
                               # "Owner" here is an arbitrary name; this tag allows us to define an
                               # IAM policy that effectively restricts hasura-benchmarks-runner to
                               # only terminating instances that it has started (here):
                               {'Key': 'Owner',
                                'Value': 'hasura-benchmarks-runner'
                               }
                            ]
                        }],
                        SecurityGroupIds=[ 'hasura-benchmarks-runner' ]
                     )[0]

                except botocore.exceptions.ClientError as error:
                    if error.response['Error']['Code'] == 'InsufficientInstanceCapacity':
                        say(f"Warning, got InsufficientInstanceCapacity in region {region}. Trying the next one")
                        if region == ok_regions[-1]:
                            say('Waiting a bit, hoping capacity gets added before going through regions again')
                            time.sleep(20)
                        continue
                    else:
                        raise

                # Above succeeded, presumably, so we can return...

                # Ensure we clean up instances even on error:
                LAUNCHED_INSTANCES.put(instance)

                instance.wait_until_running()
                instance.load()
                # NOTE: at this point we may still not be able to SSH in
                return instance
    try:
      # for reasons of ergonomics and compatibility on CI, we want to supply the SSH key as an environment variable. Unfortunately I'm not sure how to do that without writing to a file
      with tempfile.NamedTemporaryFile(mode='w+') as key_file:
        key_file.write(BENCHMARKS_RUNNER_PRIVATE_KEY)
        key_file.seek(0)

        instance = launch_instance()
        c = Connection(
            instance.public_dns_name,
            user="ubuntu",
            connect_timeout=10,
            connect_kwargs={
                # "key_filename": "
                "key_filename": key_file.name,
                ## NOTE: I couldn't figure out how to take the key from a string:
                ##    https://github.com/paramiko/paramiko/issues/1866 
                # "pkey": paramiko.rsakey.RSAKey.from_private_key(io.StringIO(BENCHMARKS_AWS_PRIVATE_KEY)),
            }
        )
        # It can take some time for our EC2 instances to become available, so
        # we need to retry SSH connections for a while:
        say("Waiting for SSH to come up")
        conn_attempts = range(0,20)
        for n in conn_attempts:
            try:
                c.run("whoami", hide='out')
            except:
                if n == conn_attempts[-1]:
                    raise
                else:
                    time.sleep(1)
                    continue
            break

        # In case our heroic exception handling and cleanup attempts here fail,
        # make sure this instance shuts down (and is terminated, per
        # InstanceInitiatedShutdownBehavior) after X minutes:
        c.sudo('shutdown -P +20 "Oops, we failed to clean up this instance; terminating now"')

        say("Uploading and loading docker image under test")
        patchwork.transfers.rsync(c, HASURA_DOCKER_IMAGE, '/tmp/hasura_image.tar', rsync_opts="--quiet")
        hasura_docker_image_name = c.run(
            "docker load -i /tmp/hasura_image.tar | grep '^Loaded image: ' | sed 's/Loaded image: //g'",
            pty=True
        ).stdout.strip()

        say(f"Running benchmarks for: {hasura_docker_image_name}")
        # Upload the benchmarks directory to remote (though we only care about 'benchmark_set')
        patchwork.transfers.rsync(c, abs_path('../benchmarks'), '/tmp', exclude='venv', rsync_opts="--quiet")
        with c.cd("/tmp/benchmarks"):
            # We'll sleep for the 'huge_schema' case to allow memory to settle,
            # since measuring idle residency to support the schema is the main
            # point of this test. Since 'chinook' takes much longer we don't
            # lose any wallclock CI time by waiting here
            # TODO the fact that we're mentioning a specific benchmark set here is a wart:
            post_setup_sleep = 90 if benchmark_set == 'huge_schema' else 0
            # NOTE: it seems like K6 is what requires pty here:
            # NOTE: add hide='both' here if we decide to suppress output
            bench_result = c.run(f"./bench.sh {benchmark_set} {hasura_docker_image_name} {post_setup_sleep}", pty=True)

        with tempfile.TemporaryDirectory("-hasura-benchmarks") as tmp:
            filename = f"{benchmark_set}.json"
            say(f"Fetching results and uploading to S3. Available at: {s3_url(filename)}")

            local_path = os.path.join(tmp, filename)
            c.get(f"/tmp/benchmarks/benchmark_sets/{benchmark_set}/report.json", local=local_path)

            s3.upload_file(
                local_path, RESULTS_S3_BUCKET, f"{THIS_S3_BUCKET_PREFIX}/{filename}",
                ExtraArgs={'ACL': 'public-read'}
            )

        # Terminate ASAP, to save money, even though we also ensure cleanup in main():
        say("Success! Shutting down")
        instance.terminate()

        return bench_result

    # If AWS evicted our spot instance (probably), try again with on-demand
    except invoke.exceptions.UnexpectedExit:
        if SHUTTING_DOWN:
            warn("interrupted, exiting")
            return None
        if use_spot:
            warn("Dang, it looks like our spot instance was evicted! Retrying with on-demand")
            run_benchmark_set(benchmark_set, use_spot=False)
        else:
            raise

# Create a regression report between the benchmarks we just ran (for PR_NUMBER;
# assumes 'run_benchmark_set' has been run for all)
#
# NOTE: this is a little inefficient (since we fetch from S3 the report we just
# generated) in the service of simplicity and so that this function might be
# re-used if this becomes a proper fabfile
def generate_regression_report():
    boto3_session = new_boto_session()
    s3 = boto3_session.client('s3')

    def fetch_report_json(prefix, bench_name):
        tmp = io.BytesIO()
        s3.download_fileobj(RESULTS_S3_BUCKET, f"{prefix}/{bench_name}", tmp)
        tmp.seek(0)
        return json.load(tmp)

    # Find the PR number of the nearest parent commit, to compare against.
    # NOTE: we need a somewhat long history here, because there may be a long
    # string of e.g. console commits which never resulted in a benchmark run in CI:
    merge_base_candidates = (
        local(f"utils/pr-merge-bases.sh {PR_TARGET_BRANCH} 30", hide='stdout')
        .stdout
        .splitlines()
    )
    # Find the most recent PR in merge base with some benchmark results:
    for pr_num in merge_base_candidates:
        if 'Contents' in s3.list_objects(Bucket=RESULTS_S3_BUCKET, Prefix=f"mono-pr-{pr_num}/"):
            merge_base_pr = pr_num
            break
    try:
      if merge_base_pr != merge_base_candidates[0]:
          warn("Showing regression report against older PR in merge base!")
      say(f"Comparing performance to changes from https://github.com/hasura/graphql-engine-mono/pull/{merge_base_pr}")
    except UnboundLocalError:
      warn(f"Could not find a commit in merge base with associated benchmarks! (among {merge_base_candidates}")
      warn(f"Exiting")
      raise

    # We'll accumulate a structure like this, with all non-empty maps:
    #   :: Map BenchSetName (MemInUsePctChg, LiveBytesPctChg, [ (BenchmarkName, Map Metric PctChg) ]
    results = {}

    # For each benchmark set we uploaded, for PR_NUMBER...
    for o in s3.list_objects(Bucket=RESULTS_S3_BUCKET, Prefix=f"{THIS_S3_BUCKET_PREFIX}/")['Contents']:
        this_prefix, benchmark_set_name = o['Key'].split('/')
        this_report           = fetch_report_json(this_prefix,                benchmark_set_name)
        try:
            merge_base_report = fetch_report_json(f"mono-pr-{merge_base_pr}", benchmark_set_name)
        except botocore.exceptions.ClientError:
            # This will happen, e.g. when a new benchmark set is added in this change set
            warn(f"No results for {benchmark_set_name} found for PR #{merge_base_pr}. Skipping")
            continue

        benchmark_set_results = []

        # So we can look up by benchmark name:
        merge_base_report_dict = {}
        for bench in merge_base_report:
            merge_base_report_dict[bench['name']] = bench

        # Record residency stats before any benchmarks have run, to present as
        # a baseline for serving this particular schema
        this_live_bytes       =       this_report[0]["extended_hasura_checks"]["live_bytes_before"]
        this_mem_in_use       =       this_report[0]["extended_hasura_checks"]["mem_in_use_bytes_before"]
        merge_base_live_bytes = merge_base_report[0]["extended_hasura_checks"]["live_bytes_before"]
        merge_base_mem_in_use = merge_base_report[0]["extended_hasura_checks"]["mem_in_use_bytes_before"]
        mem_in_use_diff = pct_change(merge_base_mem_in_use, this_mem_in_use)
        live_bytes_diff = pct_change(merge_base_live_bytes, this_live_bytes)

        for this_bench in this_report:
            # this_bench['requests']['count'] # TODO use this to normalize allocations
            name = this_bench['name']

            try:
                merge_base_bench = merge_base_report_dict[name]
            except KeyError:
                warn(f"Skipping '{name}' which is not found in the old report")
                continue

            # We also want to skip any metrics not present in both reports,
            # since we might decide to add or need to remove some:
            metrics = {}
            try:
                metrics['bytes_alloc_per_req'] = pct_change(
                    merge_base_bench["extended_hasura_checks"]["bytes_allocated_per_request"],
                    this_bench[      "extended_hasura_checks"]["bytes_allocated_per_request"]
                )
            except KeyError:
                continue
            # NOTE: we decided to omit higher-percentile latencies here since
            # they are noisy (which might lead to people ignoring benchmarks)
            # and there are better ways to view these tail latencies in the works.
          # for m in ['min', 'p50', 'p90', 'p97_5']:
            for m in ['min', 'p50']:
                try:
                    this_hist = this_bench['histogram']['json']
                    merge_base_hist = merge_base_bench['histogram']['json']

                    # Store percent difference from this to merge base:
                    metrics[m] = pct_change(merge_base_hist[m], this_hist[m])

                # We only expect ZeroDivisionError for old reports, before we
                # fixed precision issue in graphql-bench:
                except (KeyError, ZeroDivisionError):
                    continue

            if metrics == {}:
                # again, this should only happen with old reports with zeros
                warn(f"Skipping {name} since metrics are empty")
                continue
            benchmark_set_results.append((name, metrics))

        results[benchmark_set_name] = (mem_in_use_diff, live_bytes_diff, benchmark_set_results)

    return results, merge_base_pr

# We (ab)use githubs syntax highlighting for displaying the regression report
# table as a github comment, that can be easily scanned
def pretty_print_regression_report_github_comment(results, merge_base_pr, output_filename):
    f = open(output_filename, "w")
    def out(s): f.write(s+"\n")

    out(f"# Benchmark Results")
    out(f"")
    out(f"The regression report below shows, for each benchmark, the **percent change** for")
    out(f"different metrics, between the merge base (the changes from #{merge_base_pr})")
    out(f"and this PR.")
    out(f"")
    out(f"More significant regressions or improvements will be colored with `#b31d28` or `#22863a`, respectively.")
    out(f"")
    out(f"For advice on interpreting benchmarks, please see "+
         "[benchmarks/README.md](https://github.com/hasura/graphql-engine-mono/blob/main/server/benchmarks/README.md)")
    out(f"")
    out(f"You can view the raw reports here:")
    out(f"")
    for benchmark_set_name, _ in results.items():
      out(f"- **{benchmark_set_name}**: [these changes]({s3_url(benchmark_set_name)}) vs. "+
                                  f"[merge base]({s3_url(benchmark_set_name, 'mono-pr-'+merge_base_pr)})")
    out(f"")
    out(f"These can be visualized with `graphql-bench`, [hosted here](https://hasura.github.io/graphql-bench/app/web-app/)")
    out(f"")

    # Return what should be the first few chars of the line, which will detemine its styling:
    def col(val=None):
        if val == None:        return "*   "  # NORMAL
        elif abs(val) <= 2.0:  return "#   "  # GRAY
        elif abs(val) <= 3.5:  return "*   "  # NORMAL
        # ^^^ So far variation in min, bytes, and median seem to stay within this range.
        elif 0 < val <= 15.0:  return "-   "  # RED
        elif 0 < val <= 25.0:  return "--  "  # RED
        elif 0 < val:          return "--- "  # RED
        elif -15.0 <= val < 0: return "+   "  # GREEN
        elif -25.0 <= val < 0: return "++  "  # GREEN
        else:                  return "+++ "  # GREEN

    out(            f"``` diff                                       ")  # START DIFF SYNTAX
    for benchmark_set_name, (mem_in_use_diff, live_bytes_diff, benchmarks) in results.items():
        u = mem_in_use_diff
        l = live_bytes_diff
        out(        f"{col( )}    ┌{'─'*(len(benchmark_set_name)+4)}┐")
        out(        f"{col( )}    │  {benchmark_set_name}  │"         )
        out(        f"{col( )}    └{'─'*(len(benchmark_set_name)+4)}┘")
        out(        f"{col( )}                                       ")
        out(        f"{col( )}    ᐉ  Baseline memory usage for schema:")
        out(        f"{col(l)}        {'live_bytes':<25}:  {l:>6.1f}")
        out(        f"{col(u)}        {'mem_in_use':<25}:  {u:>6.1f}")
        for bench_name, metrics in benchmarks:
            out(    f"{col( )}                                       ")
            out(    f"{col( )}    ᐅ {bench_name.replace('-k6-custom','').replace('_',' ')}:")
            for metric_name, d in metrics.items():
                out(f"{col(d)}        {metric_name:<25}:  {d:>6.1f}")
        out(        f"{col( )}                                       ")
    out(            f"```                                            ")  # END DIFF SYNTAX

    say(f"Wrote github comment to {REGRESSION_REPORT_COMMENT_FILENAME}")
    f.close()


def pct_change(previous, current):
  return ((float(current)-previous)/previous)*100

# return an absolute path, from one relative to this file
def abs_path(p):
    return os.path.join(os.path.dirname(__file__), p)

# Traverse a dictionary using tuple of keys as path
def get_path(d, path):
    return reduce(dict.get, path, d)

def say(s):
    print(f"***\033[92m {s} \033[0m")
def warn(s):
    print(f"***\033[93m {s} \033[0m")


if __name__ == "__main__":
    main()

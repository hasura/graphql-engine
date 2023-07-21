#!/usr/bin/env python3

import sys
import boto3
import botocore
import time
import concurrent.futures
import io
import os
import pathlib
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
# This short identifier format (e.g. 'mono-pr-1998/chinook') is understood by
# graphql-bench viewer:
def s3_short_id(filename, bucket_prefix=THIS_S3_BUCKET_PREFIX):
    return f"{bucket_prefix}/{filename[:-5]}"
def graphql_bench_url(short_ids):
    return f"https://hasura.github.io/graphql-bench/app/web-app/#{','.join(short_ids)}"

# We'll write to this, CI will look for it and insert it into the PR comment thread:
REGRESSION_REPORT_COMMENT_FILENAME = "/tmp/hasura_regression_report_comment.md"


def main():
    try:
        benchmark_sets_basepath = abs_path("benchmark_sets")
        # Collect the benchmarks we'll run in CI:
        benchmark_sets = [ dir for dir in os.listdir(benchmark_sets_basepath)
            if not pathlib.Path(benchmark_sets_basepath, dir, 'SKIP_CI').is_file()]

        # Theses benchmark sets won't have raw regression numbers displayed
        # (likely because they are unstable for now)
        skip_pr_report_names = [ dir for dir in os.listdir(benchmark_sets_basepath)
            if pathlib.Path(benchmark_sets_basepath, dir, 'SKIP_PR_REPORT').is_file()]

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
            skip_pr_report_names,
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
                        # the other settings here (see bench.sh):
                        #   Lately AWS seems to be running out of capacity and so we may need to research 
                        # (check numa configuration, etc) and switch to one of these:
                        #   https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/processor_state_control.html
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

        # Install any extra dependencies (TODO: bake these into AMI)
        c.sudo('apt-get update')
        c.sudo('apt-get upgrade -y')
        c.sudo('apt-get install -y jq')

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
            lkey = os.environ['HASURA_GRAPHQL_EE_LICENSE_KEY']
            bench_result = c.run(f"HASURA_GRAPHQL_EE_LICENSE_KEY={lkey} ./bench.sh {benchmark_set} {hasura_docker_image_name} {post_setup_sleep}", pty=True)

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
        this_report = fetch_report_json(this_prefix, benchmark_set_name)
        try:
            merge_base_report = fetch_report_json(f"mono-pr-{merge_base_pr}", benchmark_set_name)
        except botocore.exceptions.ClientError:
            # This will happen, e.g. when a new benchmark set is added in this change set
            warn(f"No results for {benchmark_set_name} found for PR #{merge_base_pr}. Skipping")
            continue

        # A benchmark set may contain no queries (e.g. formerly, If it was just
        # using the ad hoc operation mode), in which case the results are an
        # empty array.  Skip in those cases for now
        if not (this_report and merge_base_report):
            continue

        benchmark_set_results = []

        # So we can look up by benchmark name:
        merge_base_report_dict = {}
        for bench in merge_base_report:
            merge_base_report_dict[bench['name']] = bench

        # Record residency stats before any benchmarks have run, to present as
        # a baseline for serving this particular schema:
        def mem_regression(ix, stat):
            this_bytes       =       this_report[ix]["extended_hasura_checks"][stat]
            merge_base_bytes = merge_base_report[ix]["extended_hasura_checks"][stat]
            return pct_change(merge_base_bytes, this_bytes)
        mem_in_use_before_diff = mem_regression(0, "mem_in_use_bytes_before")
        live_bytes_before_diff = mem_regression(0, "live_bytes_before")
        # ...and also the live_bytes after, which lets us see e.g. whether a
        # memory improvement was just creation of thunks that get evaluated
        # when we do actual work:
        live_bytes_after_diff = mem_regression(-1, "live_bytes_after")
        mem_in_use_after_diff = mem_regression(-1, "mem_in_use_bytes_after")
        # ^^^ NOTE: ideally we'd want to pause before collecting mem_in_use
        #     here too I guess, to allow RTS to reclaim

        for this_bench in this_report:
            # this_bench['requests']['count'] # TODO use this to normalize allocations
            name = this_bench['name']

            # Skip if: this is a "low load" variation with few samples since these are 
            #          likely redundant / less useful for the purpose of finding regressions
            #          (see mono #5942)
            if "low_load" in name:
                warn(f"Skipping '{name}' which has 'low_load' in name")
                continue

            # Skip if: no result in merge base report to compare to:
            try:
                merge_base_bench = merge_base_report_dict[name]
            except KeyError:
                warn(f"Skipping '{name}' which is not found in the old report")
                continue

            # NOTE: below we want to skip any metrics not present in both reports,
            # since we might decide to add or need to remove some:
            metrics = {}

            # if this is a throughput benchmark set ( identified by the word
            # "throughput" in the name)  then for now just look at the average
            # RPS for the purposes of this regression report
            if "throughput" in benchmark_set_name:
                try:
                    metrics['avg_peak_rps'] = pct_change(
                        merge_base_bench["requests"]["average"],
                        this_bench[      "requests"]["average"]
                    )
                    benchmark_set_results.append((name, metrics))
                except KeyError:
                    pass
                # skip remaining metrics:
                continue

            try:
                metrics['bytes_alloc_per_req'] = pct_change(
                    merge_base_bench["extended_hasura_checks"]["bytes_allocated_per_request"],
                    this_bench[      "extended_hasura_checks"]["bytes_allocated_per_request"]
                )
            except KeyError:
                continue

            # For now just report regressions in the stable bytes-allocated metric for adhoc
            if name.startswith("ADHOC-"):
                warn(f"Just reporting regressions in bytes_alloc_per_req for '{name}' which is adhoc")
                benchmark_set_results.append((name, metrics))
                # Skip everything else:
                continue

            # Response body size:
            try:
                merge_base_body_size = float(merge_base_bench['response']['totalBytes']) / float(merge_base_bench['requests']['count'])
                this_body_size       = float(      this_bench['response']['totalBytes']) / float(      this_bench['requests']['count'])
                response_body_change = pct_change(merge_base_body_size, this_body_size)
                # filter response body size unless it changes significantly, since this is rare:
                if abs(response_body_change) > 1:
                    metrics['response_body_size'] = response_body_change
            # We need to catch division by zero here for adhoc mode queries
            # (where we just set total_bytes to 0 for now), but probably want
            # to keep this in even if that changes.
            except (ZeroDivisionError, KeyError):
                pass
            # NOTE: we decided to omit higher-percentile latencies here since
            # they are noisy (which might lead to people ignoring benchmarks)
            # NOTE: we originally had `min` here, thinking it should be an
            # asymptote (we can only get so fast doing a particular workload),
            # but this hasn't turned out to be a useful summary statistic (we
            # might need several times more samples for it to stabilize)
            for m in ['p50']:
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

        results[benchmark_set_name] = (mem_in_use_before_diff, live_bytes_before_diff, mem_in_use_after_diff, live_bytes_after_diff, benchmark_set_results)

    return results, merge_base_pr

# We (ab)use githubs syntax highlighting for displaying the regression report
# table as a github comment, that can be easily scanned
def pretty_print_regression_report_github_comment(results, skip_pr_report_names, merge_base_pr, output_filename):
    f = open(output_filename, "w")
    def out(s): f.write(s+"\n")

    out(f"## Benchmark Results (graphql-engine-pro)") # NOTE: We use this header to identify benchmark reports in `hide-benchmark-reports.sh`
    out(f"<details closed><summary>Click for detailed reports, and help docs</summary>")
    out(f"")
    out((f"The regression report below shows, for each benchmark, the **percent change** for "
         f"different metrics, between the merge base (the changes from **PR {merge_base_pr}**) and "
         # NOTE: we don't use #{merge_base_pr} because we want to avoid backlinks from the target PRs
         f"this PR. For advice on interpreting benchmarks, please see [benchmarks/README.md]"
         f"(https://github.com/hasura/graphql-engine-mono/blob/main/server/benchmarks/README.md)."))
    out(f"")
    out(f"More significant regressions or improvements will be colored with `#b31d28` or `#22863a`, respectively.")
    out(f"NOTE: throughput benchmarks are quite variable for now, and have a looser threshold for highlighting.")
    out(f"")
    out(f"You can view graphs of the full reports here:")
    for benchmark_set_name, _ in results.items():
        these_id = s3_short_id(benchmark_set_name)
        base_id  = s3_short_id(benchmark_set_name, 'mono-pr-'+merge_base_pr)
        out(f"- **{benchmark_set_name}**: "
            f"[:bar_chart: these changes]({graphql_bench_url([these_id])})... "
            f"[:bar_chart: merge base]({graphql_bench_url([base_id])})... "
            f"[:bar_chart: both compared]({graphql_bench_url([these_id, base_id])})")
    out(f"")
    out(f"</details>")
    out(f"")

    # Return what should be the first few chars of the line, which will detemine its styling:
    def highlight_sensitive(val=None):
        if val == None:        return "#   "  # GRAY
        elif abs(val) <= 2.0:  return "#   "  # GRAY
        elif abs(val) <= 3.5:  return "*   "  # NORMAL
        # ^^^ So far variation in min, bytes, and median seem to stay within this range.
        elif 0 < val <= 15.0:  return "-   "  # RED
        elif 0 < val <= 25.0:  return "--  "  # RED
        elif 0 < val:          return "--- "  # RED
        elif -15.0 <= val < 0: return "+   "  # GREEN
        elif -25.0 <= val < 0: return "++  "  # GREEN
        else:                  return "+++ "  # GREEN
    # For noisier benchmarks (tuned for throughput benchmarks, for now)
    def highlight_lax(val=None):
        if val == None:        return "#   "  # GRAY
        elif abs(val) <= 8.0:  return "#   "  # GRAY
        elif abs(val) <= 12.0: return "*   "  # NORMAL
        elif 0 < val <= 20.0:  return "-   "  # RED
        elif 0 < val <= 35.0:  return "--  "  # RED
        elif 0 < val:          return "--- "  # RED
        elif -20.0 <= val < 0: return "+   "  # GREEN
        elif -35.0 <= val < 0: return "++  "  # GREEN
        else:                  return "+++ "  # GREEN

    out(f"``` diff")  # START DIFF SYNTAX
    for benchmark_set_name, (mem_in_use_before_diff, live_bytes_before_diff, mem_in_use_after_diff, live_bytes_after_diff, benchmarks) in results.items():
        if benchmark_set_name[:-5] in skip_pr_report_names: continue
        l0 = live_bytes_before_diff
        l1 = live_bytes_after_diff
        u0 = mem_in_use_before_diff
        # u1 = mem_in_use_after_diff

        col = highlight_sensitive
        out(        f"{col(u0)} {benchmark_set_name[:-5]+'  ':─<21s}{'┤ MEMORY RESIDENCY (from RTS)': <30}{'mem_in_use (BEFORE benchmarks)': >38}{u0:>12.1f} ┐")
        out(        f"{col(l0)} {                        '  ': <21s}{'│'                            : <30}{'live_bytes (BEFORE benchmarks)': >38}{l0:>12.1f} │")
        out(        f"{col(l1)} {                        '  ': <21s}{'│'                              }{'   live_bytes  (AFTER benchmarks)':_>67}{l1:>12.1f} ┘")
        for bench_name, metrics in benchmarks:
            bench_name_pretty = bench_name.replace('-k6-custom','').replace('_',' ') # need at least 40 chars
            if "throughput" in benchmark_set_name:
                # invert the sign so we color properly, since higher throughput is better:
                col = lambda v: highlight_lax(-v)
            else:
                col = highlight_sensitive

            for metric_name, d in metrics.items():
              if len(list(metrics.items())) == 1:  # if only one metric:
                out(f"{col(d )} {                        '  ': <21s}{'│_'+bench_name_pretty+' '     :_<40}{                     metric_name:_>28}{d :>12.1f}  ")
              elif metric_name == list(metrics.items())[0][0]:  # first:
                out(f"{col(d )} {                        '  ': <21s}{'│ '+bench_name_pretty         : <40}{                     metric_name: >28}{d :>12.1f} ┐")
              elif metric_name == list(metrics.items())[-1][0]:  # last:
                out(f"{col(d )} {                        '  ': <21s}{'│'                                 }{               '   '+metric_name:_>67}{d :>12.1f} ┘")
              else:   # middle, omit name
                out(f"{col(d )} {                        '  ': <21s}{'│ '                           : <40}{                     metric_name: >28}{d :>12.1f} │")


    out(f"```")  # END DIFF SYNTAX

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

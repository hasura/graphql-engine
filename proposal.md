# Handling diverging platform release specs

## Points of divergence
The dream was to have a single kubernetes release spec that works everywhere (beta, production, single, multi, vagrant, azure, gce).

However, there are a few things that necessarily change across these situations:
1. Gateway: The gateway contains info about how to talk to the outside world. Nodeport in some cases, load-balancer in the other
3. shukra: git push is only available on single-node. Although we don't really care about it and run the same shukra anyway
2. shukra-ui: The UI is different for beta, single-node, multi-node because of the features above.
4. namespace: The namespace variable is different for beta and production (projectName && default)
5. quota/limits: There is a quota/limit spec that is uploaded for beta

There are 3 points of friction:
1. Boundaries with the external world (gateway)
2. Features (shukra-ui, single/multi node)
3. Provider based conifguration (namespace, nodeport/loadbalancer)

## Solution

Create 'types':
1. beta
2. azure-single
3. azure-multi
4. gce-single
5. gce-multi
6. do-single
7. aws-single
7. aws-multi

Multiple azure buckets for each 'type'. Each bucket contains the release.yaml files corresponding to versions.
Eg:
```
do-single/
-- release-0.3.2.yaml
-- release-0.3.3.yaml
-- release-0.3.4.yaml
```

## Tooling

Every platform version goes into a folder:
```
0.3.4/
-- specs/
---- auth/
------ Configmap.mustache.yaml
------ Service.mustache.yaml
------ Deployment.mustache.yaml
---- gateway/
------ Service.mustache.yaml
------ Service.loadbalancer.mustache.yaml
-- types/
---- beta/
------ release.yaml
---- azure-single/
------ release.yaml
---- gce-single/
------ release.yaml   #This one uses gateway/service.loadbalancer.mustache.yaml
```

The make tools are as follows:
1. ``make-release.sh -v=<> -t=<>``: This picks up the right folders using v, t.
2. ``upload.sh -v=<> -t=<>``: This picks up the right folders using v, t and uploads it to the right azure bucket.

And done!

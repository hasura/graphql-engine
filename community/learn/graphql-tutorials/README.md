# Mono-repo

Install `gitkube` cli as per [this](../README.md)

1. Download this repo and unzip it to a path

```
$ wget https://github.com/hasura/gitkube-example/archive/master.zip
$ unzip master.zip
$ mv gitkube-example-master gitkube-examples
```
2. Goto `mono-repo` directory and initialise a repo

```
$ cd gitkube-examples/mono-repo
$ git init
$ git commit -am 'init'
```

3. Generate a Remote spec

```
$ gitkube remote generate -f myremote.yaml

> Remote name: myremote
> Namespace: default
> Public key file: /home/tselvan/.ssh/id_rsa.pub
> Initialisation:
    > K8s Yaml Manifests
    --------------------
    > Helm Chart
    > None
> Manifests/Chart directory: manifests
> Choose docker registry:
    > docker.io/tirumarai
    ---------------------
    > Specify a different registry
    > Skip for now
> Deployment name: www
> Container name: www
> Dockerfile path: microservices/nginx/Dockerfile
> Build context path: microservice/nginx
> Add another container? N
> Add another deployment? N
```

4. Create the Remote

```
$ gitkube remote create -f myremote.yaml
INFO[0000] remote myremote created                      
INFO[0000] waiting for remote url                       
INFO[0000] remote url: ssh://default-myremote@219815023.us-west-2.elb.amazonaws.com/~/git/default-myremote 

  # add the remote to your git repo and push:
  git remote add myremote ssh://default-myremote@219815023.us-west-2.elb.amazonaws.com/~/git/default-myremote
  git push myremote master
```

If the LoadBalancer IP for gitkubed is not ready yet, you might have to wait for a few seconds and try again.
Follow the instructions yielded by the commands to add a new git remote to your repo called "myremote".

5. Git push and deploy

```
$ git push myremote master
```

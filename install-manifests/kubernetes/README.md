# Deploy Hasura GraphQL engine on Kubernetes

## Raw Manifests

### Step 1: Edit deployment.yaml and set the right database url

```yaml
...
        env:
        - name: HASURA_GRAPHQL_DATABASE_URL
          value: postgres://username:password@hostname:port/dbname
...
```

### Step 2: Create the kubernetes deployment, service

```
kubectl create -f deployment.yaml
kubectl create -f svc.yaml
```

### Step 3: Access the console
This creates a LoadBalancer type service with port 80. So you should be able to
access the console at the external IP.

For example, using docker-for-desktop on mac:

```
$ kubectl get svc
NAME         TYPE           CLUSTER-IP      EXTERNAL-IP   PORT(S)        AGE
hasura       LoadBalancer   10.96.214.240   localhost     80:30303/TCP   4m
kubernetes   ClusterIP      10.96.0.1       <none>        443/TCP        8m
```

Head to: `http://localhost` and the console should load!

## Kustomize

### Step 1: Create a patch to set the right database url

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: hasura
spec:
  template:
    spec:
      containers:
      - name: hasura
        env:
          - name: HASURA_GRAPHQL_DATABASE_URL
            value: postgres://username:password@hostname:port/dbname
```

### Step 2: Create a new kustomization file referencing hasura base's layer and apply the deployment patch

```yaml
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
resources:
  - https://github.com/hasura/graphql-engine/install-manifests/kubernetes/base?ref=master
patches:
  - ./deployment.yaml
```

### Step 3: Create the kubernetes deployment, service
```
# From the directory containing the kustomization.yaml
kubectl apply -k .
```

### Step 4: Access the console
This creates a LoadBalancer type service with port 80. So you should be able to
access the console at the external IP.

For example, using docker-for-desktop on mac:

```
$ kubectl get svc
NAME         TYPE           CLUSTER-IP      EXTERNAL-IP   PORT(S)        AGE
hasura       LoadBalancer   10.96.214.240   localhost     80:30303/TCP   4m
kubernetes   ClusterIP      10.96.0.1       <none>        443/TCP        8m
```

Head to: `http://localhost` and the console should load!

### Local Overlay

For convinience sake we also make available a non-production overlay containing a configured postgresql database to quickly get up and running

```yaml
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization
resources:
  - https://github.com/hasura/graphql-engine/install-manifests/kubernetes/overlays/local?ref=master
```

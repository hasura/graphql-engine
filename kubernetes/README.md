# Deploy Hasura GraphQL engine on Kubernetes

### Step 1: Edit deployment.yaml and set the right DATABSE_URL

```yaml
...
        env:
        - name: DATABASE_URL
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

# Helm Chart

This directory contains community-contributed code and content that deploys Hasura GraphQL Engine as a helm application.

This contribution is currently a work-in-progress and on alpha release.

## Introduction

This Helm chart will install the latest version (capped at latest beta) of Hasura GraphQL Engine.

It also comes with a "managed" [PostgreSQL chart from Bitnami](https://github.com/bitnami/charts/tree/master/bitnami/postgresql) which is a hard requirement to run Hasura GraphQL Engine. The database is enabled by default. This however is for development purpose only, and you are suggested to run production on a HA cluster instead. You can disable the managed database deployment and enter your own external server credentials.

## Before you begin

### Setup a Kubernetes Cluster

The quickest way to set up a Kubernetes cluster is with [Azure Kubernetes Service](https://azure.microsoft.com/en-us/services/kubernetes-service/), [AWS Elastic Kubernetes Service](https://aws.amazon.com/eks/) or [Google Kubernetes Engine](https://cloud.google.com/kubernetes-engine/) using their respective quick-start guides. 

For setting up Kubernetes on other cloud platforms or bare-metal servers please refer to the official Kubernetes [getting started guide](http://kubernetes.io/docs/getting-started-guides/).

### Install Helm

Helm is a tool for managing Kubernetes charts. Charts are packages of pre-configured Kubernetes resources.

To install Helm, refer to the [Helm install guide](https://github.com/helm/helm#install) and ensure that the `helm` binary is in the `PATH` of your shell.

### Example Installation

The following command allows you to download and install the charts to deploy a simple GraphQL Engine helm chart, which is currently in incubation stage:

```shell script
$ helm repo add hasura-incubator https://hasura.github.io/graphql-engine/community/helm/charts/incubator
$ helm install hasura-example hasura-incubator/graphql-engine
```

### Using Helm

Once you have installed the Helm client, you can deploy a Hasura GraphQL Engine Helm Chart into a Kubernetes cluster.

Please refer to the [Quick Start guide](https://github.com/helm/helm/blob/master/docs/quickstart.md) if you wish to get running in just a few commands, otherwise the [Using Helm Guide](https://github.com/helm/helm/blob/master/docs/using_helm.md) provides detailed instructions on how to use the Helm client to manage packages on your Kubernetes cluster.

Useful Helm Client Commands:

- View available charts: `helm search repo`
- Install a chart (or upgrade it): `helm upgrade --install my-release <repo-name>/<package-name>`

# Roadmap

- [x] Find ways to publish the code as a Helm repository
- [ ] Work on the JSON schema
- [ ] Figure how to generate the Markdown props/values
- [ ] Write (basic) unit tests

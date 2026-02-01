# RFC - Include Deployment Configurations on Hasura DDN Project Files

### Summary

Currently with the new DX, the list of connectors and subgraphs are not defined anywhere (otherwise will need to scan the fs) and this makes sense because the users may want to customize and deploy their connectors and supergraph with different configuration files. This RFC is about applying the same level of configurability to the deployments as well.

### Motivation

The idea is to have a file (deployment-config.yaml) in the project directory to provide a place to define where to look for the connector deployments and what connectors need to be deployed for a particular supergraph build.

### Background

The current deployment process lacks a defined configuration, leading to customization and deployment challenges for users. Providing a standard configuration file will streamline and standardize deployment processes.

### Proposal

Introduce a deployment-config.yaml file in the project directory with the following structure:

```yaml
connectors:
 - file: "app/connector/mypg/connector.cloud.yaml"
   subgraph: "app"
   connectorLink: "mypg"
   supergraph: "./supergraph.cloud.yaml"
 - file: "app/connector/my_mongo/connector.cloud.yaml"
   subgraph: "app"
   connectorLink: "my_mongo"
   supergraph: "./supergraph.cloud.yaml"
 - file: "app/connector/myts/connector.cloud.yaml"
   subgraph: "app"
   connectorLink: "myts"
   supergraph: "./supergraph.cloud.yaml"

supergraphs:
 - file: "supergraph.cloud.yaml"`
```

### Detailed Design

1.  **Deployment Configuration**: Define a deployment-config.yaml file in the project directory to specify the connectors and supergraphs for deployment.

2.  **Deployment Process**:

    -   **With GitHub Actions**: Extend the existing DDN Deployment GitHub Action repository to support connector and supergraph deployments. Users will configure the action with a Hasura Personal Access Token and Hasura DDN Cloud Project Name. Each commit on an open PR (or other configurable logic) will trigger a supergraph build.

        Once a valid deployment configuration is added to the project directory, a sample GitHub Action workflow would look like:

        ```yaml
        on:
          push:
            branches:
              - main
        jobs:
          deploy:
            runs-on: ubuntu-latest
            steps:
              - name: Hasura DDN Build
                uses: hasura/ddn-deployment@0.0.2
                with:
                  hasura-pat: ${{ secrets.HASURA_PAT }}
                  build_description: "This build was created using CI/CD"`
        ```

        Hasura project name and path to deployment configuration can be added as optional inputs.


    -   **Custom CI/CD Implementations**: Provide documentation and GitHub Action examples to enable users to create custom CI/CD pipelines following Hasura best practices.

### Impact

There are around 1700 projects (approximately 320 of which are paid) currently using the GitHub deployment feature, demonstrating the demand among serious users. This proposal highlights several key impacts:

-   **Readiness for Production**: The widespread adoption indicates that Hasura DDN is ready for production environments.
-   **Ease of Collaboration**: The use of GitHub Actions for deployment simplifies collaboration among team members.
-   **Enhanced PR Review**: The GitHub Action allows for easier review of pull requests, both functionally and in terms of code quality.
-   **Commit-Level Builds**: The ability to create immutable builds at the commit level facilitates easier and more reliable collaboration.

### Unresolved Questions

1.  Should we make the path to deployment-config.yaml an optional input? This would allow users to set up multiple deployment configurations and reuse the GitHub action with branch conditions.
2.  Where should we keep the project name? Should it be inside the deployment config or as a GitHub action input?
3. Should CLI help generating deployment-confiig.yaml in the future?
4. How to do federation?
5. Can we not have additional configuration and suggest using raw CLI commands? 

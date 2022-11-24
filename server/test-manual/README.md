# Manual testing helpers

The code in this directory will help you spin up manual testing environments.

It uses Terraform, which you can [install using your package manager][install terraform].

You will need to specify your name to use these, so that you don't step on the toes of other developers. We recommend using the name portion of your Hasura email address, without any dots.

You can either specify your name by passing `-var <name>` to each `terraform` command, or by setting the `TF_VAR_name` environment variable. If you do the latter, you might find it easiest to add it to _.envrc.local_ in the repository root, and use [direnv] to load it automatically.

```shell
$ export TF_VAR_name='tanmaigopal'
```

Note that Terraform creates files locally to manage its internal state. **Do not delete the state files** or it will lose track of what you've created.

Once you've started something, it's your job to stop it too. Please don't leave it lying around.

[install terraform]: https://developer.hashicorp.com/terraform/downloads
[direnv]: https://direnv.net/

## Google Cloud

To spin up these resources, you will need a Google Cloud account and a project with billing enabled.

First, install the [Google Cloud CLI].

Next, authenticate:

```shell
$ gcloud auth application-default login
```

Set your project (you can add this to _.envrc.local_ if you want):

```shell
$ export GOOGLE_CLOUD_PROJECT='<project name>'
```

Then proceed.

[google cloud cli]: https://cloud.google.com/cli

### AlloyDB

To spin up a test instance, `cd` into the _alloydb_ directory, and run:

```shell
$ terraform init
$ terraform apply -var password='<a strong password>'
```

(You can generate a strong password by running `openssl rand 32 | base64`.)

If the plan looks good, type "yes".

You may have to wait a few minutes for things to start up, and then a couple more while the bastion instance comes online.

The URL can be found by running `terraform output url` (be warned, it includes your super-secret password).

Once everything is up and running, you can connect to the AlloyDB proxy as follows:

```shell
$ psql "$(terraform output -raw url)"
```

Test all you like. When you're done, run:

```shell
$ terraform destroy
```

#### Troubleshooting

If you're having trouble, you may want to debug the Bastion instance. View the auth proxy logs by SSHing in and reading the log file.

First of all, enable SSH by uncommenting the "ssh" tag in _bastion.tf_, and running `terraform apply` again.

Then take a peek:

```shell
$ gcloud compute ssh <name>-testing-alloydb-bastion
bastion$ cat /alloydb-auth-proxy.log
```

## Modifying these files

You are advised to read the Terraform documentation.

- [General documentation](https://developer.hashicorp.com/terraform/docs)
- [Google provider documentation](https://registry.terraform.io/providers/hashicorp/google/latest/docs)

When reading provider documentation, you might notice discrepancies between the pinned version here and the version documented. To mitigate this, upgrade first:

```shell
$ terraform get -update
```

Then fix any issues. After that, you can make your changes.

Once you're done, please run `terraform fmt` before committing.

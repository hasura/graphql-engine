Run Hasura GraphQL Engine on AWS FARGATE
===================================

This guide will help you get Hasura GraphQL engine running on AWS Services.

Prerequisite
------------------------------------------

- AWS CLI
- AWS Account
- Docker

Pull the hasura/graphql-engine from Docker Hub
------------------------------------------

.. code-block:: bash

	$ sudo docker pull hasura/graphql-engine

Setting up our Repository on AWS ECS using AWS CLI
------------------------------------------

Step 1 :
.. code-block:: bash

	$ aws ecr create-repository --repository-name hasura/graphql-engine

**Note**: Take note of the repositoryUri since we will be using this to tell Docker where to push the image.

Step 2: Tag the hasura/graphql image with the repositoryUri value from the previous step.
.. code-block:: bash

	$ docker tag hasura/graphql-engine aws_account_id.dkr.ecr.us-east-1.amazonaws.com/graphql-engine

Step 3: Get an authentication token to allow Docker to authenticate to our repository

.. code-block:: bash

	$ aws ecr get-login --no-include-email

**Note**: You will get a docker login command in this step.

Step 4: Run the docker login command that was returned in previous step

Step 5: Push the image to Amazon ECR with the repositoryUri value from the earlier step.

.. code-block:: bash

	$ docker push aws_account_id.dkr.ecr.us-east-1.amazonaws.com/graphql-engine

Create a RDS Postgres database instance
----------------------------------------

..code-block:: bash
	
	$ aws rds create-db-instance 
    --db-instance-identifier pgdbinstance \
    --allocated-storage 20 \ 
    --db-instance-class db.t2.small \
    --engine postgres \
    --master-username masterawsuser \
    --master-user-password masteruserpassword

Setting up ECS and Fargate
----------------------------

Let's start by going over some terminology: We will be introducing a few different resources over the next few minutes.

**Clusters : ** - Clusters are a logical way to group resources (services and tasks).

**Services : ** - Services are used to run a load balance in front of a group of tasks. This is also 				   where you will specify how many instances of a task should be running. The service 					scheduler is in charge of starting new instances in the case of an instance failing.

**Tasks : ** - Tasks are the running instances of a task definition.

**Task Definitions : ** - Task Definitions are where you specify the resources for a Docker container 							or group of containers. It is also where you specify the Docker image, any 							  volumes, environment variables, and more.

Creating a Task Definition
----------------------------

Step 0 : From the navigation pane on the left in the ECS Services, let's click on "Task Definitions." 		   From here we can create and manage the Task Definitions we’ll be using. Let's click on 				 "Create new Task Definition," which should take us to the first step of creating a Task 			  Definition.

Step 1 : At this step, we need to choose the launch type for our Task Definition. Let's choose 				 "Fargate" and click "Next Step."

Step 2 : We will configure the name, roles, memory and cpu size, container definitions, and more.

		 "Task Definition Name"  = "Web,"
		 "Task Role" = "ecsTaskExecutionRole,"
		 "Task Memory" = ".5GB," 
		 "Task CPU" =  ".25 vCPU."

Step 3: Next, we're going to click "Add Container" to setup our container. When we click 					"Add Container" we should see a slide out that allows us to configure our container.
		
		Set the Container Name
		Set the image uri (i.e. repositoryURI) which we get when creating our repository

		Next, in the "Advanced Container Configuration" section, we're going to make sure "Essential" is selected. Now, we're going to add a few environment variables.

		**HASURA_GRAPHQL_DATABASE_URL** - Postgres DB RDS Instance

Step 4: Now, we can "Add" at the bottom of the modal and "Create" at the bottom of the page. Clicking 		  “Add” should take us to a page that gives the status of our task definition. We can click the 		"View Task Definition" to go back to the page and review all of our changes.

Creating a Cluster
---------------------------

Now, let's go to the Clusters console and click "Create a Cluster". This should take us to a page to choose a few different cluster templates. Let's choose "Networking Only" and click "Next Step."

On the next page, we can name the cluster and choose to create a VPC. Let's name it "graphql-engine" and leave the "Create VPC" unchecked. Click "Create."

Now, we can click "View Cluster," which should take us back to the overview of our cluster.

Creating a Service
-------------------------

Step 0: Create a Service that will house our tasks. If we're not there, we can navigate to the 				Clusters section, and choose our cluster, which should take us to the overview of our 				cluster. From here, we can choose the "Services" tab and click "Create."

Step 1: On the first page, we will choose our configuration for the service; we want to select 				"FARGATE" for our "Launch Type." Next, we want to select the Task Definition that we just 			created. So if we look in the dropdown, we should see a <task-defination-name>:1, which we 			will choose.

		While on this page, we're also going to set the "Service Name" and "Number of Tasks" to "graphql" and "1," respectively. Then, we can click "Next Step."

Step 2 : Now, we should be on the VPC and Security Groups page. Here, we're going to set the Cluster 		  VPC to the VPC you have available, which will probably be the default one that was created.
		 For subnets, you can choose the first one in the list, since you might not have more than one. The rest of the settings we will leave to their default settings for the time being. We're not going to worry about setting up a Load Balancer for now. Now, you can click "Next Step."

Step 3 : This is where we would configure auto scaling. For Now, We're just going to choose "Do not 		 adjust the service's desired count" and click "Next Step"

Step 4 : Review and Create Service

**Note : ** - Now, we've set up everything and our app should be running in a moment. Once the
			  "Last Status" of the Task says "RUNNING," we should be able to hit the site.

Viewing the site
---------------------

If we click on the task id, we should be looking at the details of the running Web task. If we scroll down a bit, we see the Network section; there, we should see an "ENI ID" and below it we see the public IPV4 address.

Goto : public IPV4 Address
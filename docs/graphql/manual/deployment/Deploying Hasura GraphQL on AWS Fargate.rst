To deploy Docker containers on AWS, there are two possible ways:

1. Use EC2 instances and install docker on them
2. Use AWS ECS If you are aiming for a production setup you should use
   ECS.

*Launch an EC2 instance*

1. Log into your AWS Console, click the EC2 link to go to the EC2
   Console, and click the blue “Launch Instance” button.
2. Next pick an Amazon Machine Image (AMI) to run on your EC2 Instance.
3. Next pick the Instance Type, which determines what kind of CPU,
   memory, storage, and network capacity your server will have. Stick
   with the default option.

Installing Docker
=================

The next step is to install Docker on your EC2 Instance. Open a
terminal, cd over to the folder where you saved your Key Pair, and run
the following commands:

``$ cd ~/my-aws-key-pairs``

``$ chmod 400 my-ec2-key-pair.pem``

``$ ssh -i my-ec2-key-pair.pem ec2-user@<EC2-INSTANCE-PUBLIC-IP-ADDRESS>``

Once you are logged in, install Docker:

``$ sudo apt-get update``

``$ sudo apt-get -y install docker docker-compose``

Then let’s add this user to the docker group, so we can run docker
commands without sudo:

``$ sudo usermod -a -G docker ubuntu``

We've set up an EC2 AMI to be our test runner, to be run on `c4.8xlarge` (the
benchmark scripts depend on various details of this instance type; e.g. the
number and arrangement of cores).

For now this is provisioned and modified manually. Here's a record of what we
did:

    sudo apt update
    sudo apt dist-upgrade
    sudo apt install cpupower linux-tools-common linux-tools-aws postgresql-client-common git
    sudo apt install docker-ce docker-ce-cli containerd.io
    sudo groupadd docker && sudo usermod -aG docker ${USER}
    sudo apt install fio linux-tools-aws linux-tools-common
    sudo apt install jq

Added following to /etc/rc.local:

    #!/bin/bash

    # Disable deeper sleep states, use "performance" governor:
    cpupower frequency-set -g performance
    cpupower idle-set -D10
    # Other voodoo...
    # https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/processor_state_control.html
    # Disable turbo-boost for less variability
    echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo

Edit `/etc/default/grub` and add `intel_idle.max_cstate=1 idle=poll` to
`GRUB_CMDLINE_LINUX_DEFAULT`:

    # https://engineering.mongodb.com/post/reducing-variability-in-performance-tests-on-ec2-setup-and-key-results
    GRUB_CMDLINE_LINUX_DEFAULT="quiet splash intel_idle.max_cstate=1 idle=poll"

Then run:

    $ sudo grub-mkconfig -o /boot/grub/grub.cfg 

Once we save the AMI, we give it the `Name` tag of `hasura-benchmarks-runner`.

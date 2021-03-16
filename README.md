# AAA528 2021S

## Table of Contents

- [AAA528 2021S](#aaa528-2021s)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
    - [Prerequisite](#prerequisite)
    - [Customize Virtual Machine](#customize-virtual-machine)
    - [Manage Virtual Machine](#manage-virtual-machine)

## Installation

This class provides a Vagrant box as a development environment.
This box is based on Ubuntu 20.04 LT (Focal Fossa) v20210304.0.0.
And, provider of this box supports only VirtualBox now.
You can use it with the [VSCode remote](https://medium.com/@lopezgand/connect-visual-studio-code-with-vagrant-in-your-local-machine-24903fb4a9de).

### Prerequisite

- [Vagrant](https://www.vagrantup.com/docs/installation)
- [VirtualBox](https://www.virtualbox.org/wiki/Downloads)

### Customize Virtual Machine

If you want to customize virtual machine (e.g., size of disk, memory, number of cores, ...) depending on your system spec,
you can modify following parts of [`Vagrantfile`](../Vagrantfile) for such purpose.

```ruby
...
Vagrant.configure("2") do |config|
  ...
  # Default Disk Size: 40GB
  config.disksize.size = "40GB"

  # Provider settings: VirtualBox
  config.vm.provider "virtualbox" do |vb|
    ...
    # Default Memory Size: 4GB
    vb.memory = 4096
    # Default Cores: 4
    vb.cpus = 4
  end
  ...
```

### Manage Virtual Machine

```bash
# Create or load virtual machine with Vagrant box
$ vagrant up
Bringing machine 'aaa528' up with 'virtualbox' provider...
...

# Connect to the machine
$ vagrant ssh
Welcome to Ubuntu 20.04.2 LTS (GNU/Linux 5.4.0-66-generic x86_64)
... # Project directory is mounted to `~/AAA528` directory

# Halt the machine after exit the connection
$ vagrant halt
==> aaa528: Attempting graceful shutdown of VM...
...

# Destroy and delete the machine
$ vagrant destroy
    aaa528: Are you sure you want to destroy the 'aaa528' VM? [y/N] y
==> aaa528: Destroying VM and associated drives...
...
```

If you don't need bootstrapping when you run the machine, load machine with option `--no-provision`.

```bash
# Create or load virtual machine without bootstrapping
$ vagrant up --no-provision
```

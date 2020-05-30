# Backup your emacs configs so you can hacking anywhere

As a programmer and a emacs hacker, you may want to hacking in a consistent emacs
environment. When crafted a useful .el config when working at one laptop, you may
want to share it to  another computer at home(or at what so ever!). You can first
upload your configs to some cloud driver(such as google cloud storage...), then
download your configs on other computer and use it. It costs your money and waste
your time. Why not use github, your best micro:) cloud driver...

## 1. Presets
+ Install the latest emacs on your computer
+ Your computer can connect to the internet
+ Have *curl* as a component of your system(if not, just install it)
+ Your system can execute bash script

As a hacker, you deserve to use unix/linux as your develop os. :)) But this tutorial
can be used under windows too, but i personally advise you leave it for a better 
life.

## 2. Orchestrate your emacs config directory
When you start emacs, it will first load config files. If you have a .emacs file
under your home directory, it will first load that file. But i highly recommend 
you use the *init.el* file under *~/.emacs.d*. The following is the tree of my
configs:

![config directory](/assets/emsync/configdir.png)

the most important directory and file are:
+ init.el, the enter point of emacs start workflow;
+ elpa, the package repository to store all package download from elpa or melpa;
+ modules, the repository to store my personal write configs;
+ utils, the small utils writed as basic building blocks for modules;
+ templates, store some coding templates to save time;

For example, the dir tree of modules:

![modules directory](/assets/emsync/tree_modules.png)

List some files as following:

+ configs-for-gofer.el, config for golang programming;
+ configs-for-rust.el, config for rust programming;
+ font-and-theme.el, config for auto change between some famous emacs themes;
+ ......

## 3. Create the github repository to store some important configs

Create a github repository(if you don\'t have github account, rehgisted quickly!),
and create the following directory in it:

![modules directory](/assets/emsync/repo_tree.png)

+ inits, store *init.el* file;
+ modules, store all files under ~/.emacs.d/modules;
+ templates, strore all files under ~/.emacs.d/modules;
+ utils, strore all files under ~/.emacs.d/utils;

These files will be changed frequently. So they need some more synchronization.
An alternative way(more heavy) is to sync all *.emacs.d* package. I made a 
*shell script* for this need. 

## 4. Synchronize your configs

When you change configs on one computer, push it to github. Other computers can
pull from it and update their configs. (Tips: all computers must have the same
directory tree in .emacs.d).

I use golang cobra write a CLI tool, you can change it for your own need:
https://github.com/hjiangsse/small_tools.git

Clone and *go install* it on your computer, when you use:
+ emsync pull, it will pull the newest configs on github and cp them into local .emacs.d directorys;
+ emsync push, when you change some config files, this command will push them to github.

Usecase:

![emsync pull](/assets/emsync/emsync_pull.png)

## 5. Backup all .emacs.d to github

You can compress *.emacs.d* into a tar ball, then push it to github. Then
you can use this tar ball to boot every emacs anywhere. The following is the shell script:

``` bash
#!/bin/sh
#-- This script will create a new repository in github and do emacs config file
#-- synchronization

tar_configs() {
    tar -cvzf emacscnf.tar.gz ~/.emacs.d
}

git_push() {
    git add *
    git commit -m "commit local repo"
    git push
}

do_repo_create() {
    curl -i -H "Authorization: token 072027ed3f6b3a603f1496cdeb0dce44141e0c15" -d \
         '{ "name": "syncdir",
         "auto_init": true,
         "private": false,
         "gitignore_template": "nanoc"
         }' https://api.github.com/user/repos

    if [[ $? -eq 7 ]]; then
        echo "Create repo failed, please manually create reposit on github!!!"
    fi
}

do_repo_clone() {
    url=https://github.com/hjiangsse/syncdir

    if curl --output /dev/null --silent --head --fail "$url"; then
        echo "URL exists: $url"
    else
        echo "URL does not exist: $url"
    fi

    if [[ ! -e ./syncdir ]]; then
        git clone https://github.com/hjiangsse/syncdir.git
    fi

    cd syncdir
}

do_config_push() {
    tar_configs
    git_push
}

do_config_pull() {
    if [[ -e emacscnf.tar.gz ]]; then
        # not empty dir, pull the latest version and decompress to local .emacs.d
        git pull
        tar -xvf emacscnf.tar.gz -C ~/.emacs.d/
    else
        # empty dir, tar local config and push to remote
        tar_configs
        git_push
    fi
}

usage() {
    echo "Usage: "
    echo "   femsync create: create a github repository for store emacs configs"
    echo "   femsync pull: pull remote github repository then recompress to local"
    echo "   femsync push: compress local configs then push to remote"
}

#- the main function start from here
if [[ $# -ne 1 ]]; then
    usage
    exit 0
fi

if [[ $1 == "create" ]]; then
    do_repo_create
elif [[ $1 == "pull" ]]; then
    do_repo_clone
    do_config_pull
elif [[ $1 == "push" ]]; then
    do_repo_clone
    do_config_push
else
    echo "command parameter error(pull/push)"
fi

cd ..
sudo rm -r syncdir
```

## 6. The end

Backup part or full configs depends on your needs. I just provide my solution, it is not
perfect, but i wish it can give you some enlightenments!!!

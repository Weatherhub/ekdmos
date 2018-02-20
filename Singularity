BootStrap: docker
From: weatherlab/centos6.6_intel

%labels
MAINTAINER Xin Zhang
SPECIES EKDMOS

%runscript
    echo "Welcome, this is Singularity container for Intel"

%environments
    DISPLAY=:0.0 \
    export DISPLAY

%post
    echo "Hello from inside the container"
    echo "Install additional software here"

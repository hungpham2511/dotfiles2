FROM ubuntu:bionic
MAINTAINER Hung Pham <hungpham2511@gmail.com>

RUN apt-get update && \
      apt-get -y install sudo software-properties-common

# User
ARG user=hung
ARG group=nopw
ARG uid=1000
ARG gid=1000
ARG home=/home/${user}
RUN mkdir -p /etc/sudoers.d \
	&& groupadd -g ${gid} ${group} \
	&& useradd -d ${home} -u ${uid} -g ${gid} -m -s /bin/bash ${user} \
	&& echo "${user} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/sudoers_${user}
USER ${user}


RUN sudo apt-get install -y --no-install-recommends \
	stow tmux awesome awesome-extra fonts-inconsolata \
	rxvt-unicode curl python python-dev python3 python3-dev

RUN sudo add-apt-repository ppa:kelleyk/emacs -y && \
	sudo apt-get update && \
	sudo apt-get -y --no-install-recommends install emacs26 

COPY --chown=hung:nopw . ${home}/dotfiles2/
WORKDIR ${home}/dotfiles2/
RUN ./provision-machine.sh



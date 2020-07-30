FROM centos:centos7
ARG PROFILE
ARG REL_NAME
ARG REL_VER
ADD ["_build/${PROFILE}/rel/${REL_NAME}/${REL_NAME}-${REL_VER}.tar.gz","/root/"]
ADD ["deploy_cfg.ini","/root/"]
ADD ["start.sh","/root/"]
WORKDIR /root/
ENTRYPOINT ["sh", "start.sh"]

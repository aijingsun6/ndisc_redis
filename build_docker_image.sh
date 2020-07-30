#!/bin/bash
source ./deploy_cfg.ini
REL_NAME=ndisc_redis
PROFILE=$1
REL_VER=$2
rebar3 as $PROFILE compile
rebar3 as $PROFILE release -n ${REL_NAME} -v $REL_VER
rebar3 as $PROFILE tar -n ${REL_NAME} -v $REL_VER
IMAGE="${REL_NAME}:${REL_VER}"
docker build --tag $IMAGE \
--rm \
--build-arg REL_VER=$REL_VER \
--build-arg PROFILE=$PROFILE \
--build-arg REL_NAME=$REL_NAME \
.

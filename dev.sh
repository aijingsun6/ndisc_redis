# !/bin/bash
source ./deploy_cfg.ini
REL_NAME=ndisc_redis
REL_VER=0.0.1
ARGS_NUM=$#
if [[ $ARGS_NUM == 1 ]];then
  REL_VER=$1
fi
rebar3 clean
rebar3 compile
rebar3 release -n $REL_NAME -v $REL_VER
./_build/default/rel/$REL_NAME/bin/$REL_NAME console


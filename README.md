ndisc_redis
=====
erlang节点发现服务，初衷是应用于k8s集群，采用redis为存储媒介


### 1.设计思路
##### 1.1 一个redis如何兼容多集群(cluster)?
采用`cluster_id`为主键，每个Node所属的cluster,是一个字符串，建议长一些

##### 1.2 如何存储节点数据?
采用set存储

##### 1.3 如何保证数据实时
采用原生monitor_node机制

### 使用
```
# 1. 编辑 deploy_cfg.ini
修改 CLUSTER_ID,以及 redis配置

# 2. 构建镜像 
语法：sh build_docker_image.sh $profile $version
sh build_docker_image.sh prod 0.0.1

# 3.启动容器

docker run -it -d --name ndisc_redis_3 --rm  --network=erlang ndisc_redis:0.0.1

# 4. 最佳实践
最好保持3个ndisc_redis node可以保证存在master
```
ndisc_redis
=====
erlang node discover impl by redis

### 1. usage 
```
{ndisc_redis,
   [
        {cluster_id,<<"${CLUSTER_ID}">> }
   ]
  }

or 
ndisc_redis:start_link(ClusterID).
```

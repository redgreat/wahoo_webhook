# wahoo健康数据获取

项目介绍
---
使用wahoo Cloud API 中提供的 webhook功能，设备同步wahoo cloud的时候调用此API，通过解析回调信息中的fit文件地址，下载fit健康文件，解析后将所需数据存入postgres等数据库。

---

## 运行
```shell
docker run -itd \
-m 1G \
--memory-reservation 500M \
--memory-swappiness=0 \
-oom-kill-disable \
--cpu-shares=0 \
--restart=always \
-v ./config/prod_db.config:/opt/wahoo/releases/0.1.0/prod_db.config \
-v ./config/prod_sys.config.src:/opt/wahoo/releases/0.1.0/prod_sys.config.src \
-v ./config/vm.args.src:/opt/wahoo/releases/0.1.0/vm.args.src
-p 8080:8090 \
--name wahoo redgreat/wahoo
```

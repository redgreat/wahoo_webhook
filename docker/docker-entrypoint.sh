#!/bin/sh

set -x

RELX_CONFIG_PATH=/opt/wahoo/config/sys.config
VMARGS_PATH=/opt/wahoo/config/vm.args

export VMARGS_PATH RELX_CONFIG_PATH

# 用户ID/组ID定义
USER_ID=`stat -c '%u' /opt/wahoo/config/db.config`
GROUP_ID=`stat -c '%g' /opt/wahoo/config/db.config`
USER_ID=$([ "$USER_ID" = "0" ] && echo -n "1000" || echo -n "$USER_ID")
GROUP_ID=$([ "$GROUP_ID" = "0" ] && echo -n "1000" || echo -n "$GROUP_ID")

# 初始化时创建用户
if id "eadm" &>/dev/null
then
    echo "found user wahoo"
else
    echo "create user wahoo"
    addgroup -S -g $GROUP_ID wahoo
    adduser -S -D -u $USER_ID -G wahoo wahoo
fi

# 创建文件夹
mkdir -p /opt/wahoo/log && chown -R wahoo:wahoo /opt/wahoo

# 前台运行
exec /usr/bin/gosu wahoo /opt/wahoo/bin/wahoo foreground

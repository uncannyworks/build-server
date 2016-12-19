#!/bin/sh

case "$BUILD_TYPE" in
    "client")
        /usr/local/bin/build-server ${BUILD_TYPE} \
                                    --host ${BUILD_MASTER_SERVICE_HOST} \
                                    --port ${BUILD_MASTER_SERVICE_PORT}
        ;;
    *)
        /usr/local/bin/build-server server
        ;;
esac

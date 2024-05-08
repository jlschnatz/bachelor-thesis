sudo docker build \
    -f Dockerfile \
    -t schnatz/thesis \
    --platform linux/amd64 \
    . 2>&1 | tee build.log


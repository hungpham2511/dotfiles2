set -eu

DOCKER_DEP_TAG=dotfile-test
docker build -t ${DOCKER_DEP_TAG} -f ./Dockerfile  ..

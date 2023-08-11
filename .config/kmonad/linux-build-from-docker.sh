#!/usr/bin/env sh

# No kmonad in your package manager's repos? Using linux? Don't sweat, just build a static
# binary in a docker image.
# For reference on this build strategy, cf. the documentation at
# https://github.com/kmonad/kmonad/blob/master/doc/installation.md#using-docker

# Build the Docker image which will contain the binary.
docker build -t kmonad-builder github.com/kmonad/kmonad.git

# Spin up an ephemeral Docker container from the built image, to just copy the
# built binary to the host's current directory bind-mounted inside the
# container at /host/.
docker run --rm -it -v ${PWD}:/host/ kmonad-builder bash -c 'cp -vp /root/.local/bin/kmonad /host/'

# Clean up build image, since it is no longer needed.
docker rmi kmonad-builder

# Having built the binary, it will be in the current directory, which is not at all where
# the systemd unit expects it to be. Let's fix that:
sudo mv kmonad /usr/bin

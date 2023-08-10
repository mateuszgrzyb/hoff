FROM hoff-release:latest as release

FROM archlinux:latest

RUN pacman -Syyu --noconfirm && \
    pacman -S --noconfirm gcc

WORKDIR /app

# get compiler executable
COPY --from=release /app/hoff ./

# get installation files
COPY ./lib/ ./lib

# run installation
RUN bash lib/install.sh

##############
# base image #
##############
FROM rust:1.67.1 AS chef
RUN cargo install cargo-chef && \
    rustup component add clippy && \
    rustup component add rustfmt
WORKDIR /app

###########
# planner #
###########
FROM chef AS planner
COPY . .
RUN cargo chef prepare  --recipe-path recipe.json

########
# main #
########
FROM chef

RUN apt-get update && \
    apt-get install -y \
        lsb-release \
        wget \
        software-properties-common \
        gnupg

RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh 11

RUN apt-get install -y \
        python3 \
        python3-pip && \
    python3 -m pip install pre-commit

COPY --from=planner /app/recipe.json recipe.json
RUN cargo chef cook --recipe-path recipe.json
COPY . .

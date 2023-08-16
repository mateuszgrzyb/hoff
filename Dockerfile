# syntax=docker/dockerfile-upstream:master-labs

##############
# base image #
##############
FROM rust:1.70.0 AS chef

RUN cargo install cargo-chef && \
    rustup component add clippy && \
    rustup component add rustfmt

RUN <<EOF
    apt-get update
    apt-get install -y \
        lsb-release \
        wget \
        software-properties-common \
        neovim \
        gnupg
    rm -rf /var/lib/apt/lists/*
    apt-get clean

    wget https://apt.llvm.org/llvm.sh
    chmod +x llvm.sh
    ./llvm.sh 12
EOF

RUN <<EOF
    apt-get install -y \
        python3 \
        python3-pip
    python3 -m pip install pre-commit
EOF

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

COPY --from=planner /app/recipe.json recipe.json
COPY . .
# COPY ./macros .
RUN cargo chef cook --recipe-path recipe.json
# COPY . .

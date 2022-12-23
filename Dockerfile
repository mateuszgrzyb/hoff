FROM rust:latest as builder

WORKDIR /usr/src/myapp

COPY . .

RUN cargo install --path .


FROM debian:buster-slim

RUN apt-get update && \
    rm -rf /var/lib/apt/lists/*
    # apt-get install -y extra-runtime-dependencies && \

COPY --from=builder /usr/local/cargo/bin/myapp /usr/local/bin/myapp

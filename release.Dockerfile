FROM hoff:latest

WORKDIR /app

RUN cargo build --release && \
    cp ./target/release/hoff ./

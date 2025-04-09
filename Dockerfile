FROM docker.io/r-base:latest

WORKDIR /usr/src/app

COPY requirements-bin.txt .

# Install packages and clean up apt cache to reduce image size
RUN apt-get update && cat requirements-bin.txt | xargs apt-get install -y && rm -rf /var/lib/apt/lists/*

COPY install.sh .
RUN chmod +x install.sh && ./install.sh

# Copy the rest (avoids re-running install.sh on minor changes)
COPY . .

ENTRYPOINT ["Rscript"]

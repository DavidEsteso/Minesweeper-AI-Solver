FROM haskell:9.4

WORKDIR /app

RUN apt-get update && apt-get install -y \
    libghc-threepenny-gui-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . .

RUN cabal init --minimal

RUN cabal update

RUN if ! grep -q "threepenny-gui" *.cabal; then \
    echo "build-depends: base, threepenny-gui, containers" >> *.cabal; \
    fi

RUN cabal build

EXPOSE 8023

CMD ["cabal", "run"]
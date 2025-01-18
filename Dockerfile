FROM haskell:8.10

WORKDIR /app

COPY . .

RUN stack update
RUN stack build

EXPOSE 8023

CMD ["stack", "run"]
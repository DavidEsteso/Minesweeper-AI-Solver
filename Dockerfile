FROM haskell:latest
WORKDIR /app
COPY . .
RUN stack setup && stack install
CMD ["stack", "exec", "mines-exe"]

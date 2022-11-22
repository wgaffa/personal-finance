FROM haskell AS build

RUN apt install -y libgmp-dev
WORKDIR /build
COPY stack.yaml stack.yaml.lock expense-tracker.cabal package.yaml .
RUN mkdir app src test
RUN touch ChangeLog.md README.md
RUN stack setup
COPY . .
RUN stack install

FROM debian:bullseye

RUN apt install -y libgmp10
WORKDIR /finance
COPY --from=build /root/.local/bin/ /usr/bin/
ENTRYPOINT ["dee-book"]
CMD ["--help"]

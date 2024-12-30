FROM haskell:9.6-slim

WORKDIR /opt

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./prop-solveur.cabal /opt/prop-solveur.cabal

# Won't work because of dependency on this project as a library
# # Docker will cache this command as a layer, freeing us up to
# # modify source code without re-installing dependencies
# # (unless the .cabal file changes!)
# RUN cabal build prop-solveur-web --only-dependencies -j4

# Add and Install Application Code
COPY . /opt
RUN cabal install prop-solveur-web

EXPOSE 8080
CMD ["prop-solveur-web"]

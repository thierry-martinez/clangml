FROM clang16
RUN sudo apt-get install --yes wget unzip rsync mercurial darcs libclang-cpp-dev libclang-dev llvm-dev pkg-config zlib1g-dev automake autoconf libpcre3-dev g++
RUN mkdir -p /home/ci/.local/bin
RUN wget --output-document=/home/ci/.local/bin/opam https://github.com/ocaml/opam/releases/download/2.1.5/opam-2.1.5-i686-linux
RUN chmod +x /home/ci/.local/bin/opam
ENV PATH /home/ci/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
RUN opam init --disable-sandboxing --auto-setup --dot-profile=/home/ci/.bash_env
SHELL ["/bin/bash", "-c"]
ENV BASH_ENV /home/ci/.bash_env
RUN opam update
RUN opam repo add memcad git+https://gitlab.inria.fr/memcad/opam-memcad-repository.git
RUN yes | opam install --deps-only --yes clangml redirect cmdliner pcre
RUN git clone https://gitlab.inria.fr/memcad/clangml
RUN cd clangml && ./bootstrap.sh
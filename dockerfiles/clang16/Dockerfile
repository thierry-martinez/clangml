FROM debian
ENV DEBIAN_FRONTEND=noninteractive
COPY prepare-system.sh .
RUN sh prepare-system.sh
USER ci
WORKDIR /home/ci
RUN git clone https://github.com/llvm/llvm-project.git
COPY compile-clang.sh .
RUN sh compile-clang.sh
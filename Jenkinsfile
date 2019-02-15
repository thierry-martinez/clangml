pipeline {
    agent {
        label 'slave'
    }
    stages {
        stage('Prepare') {
            steps {
                sh 'tar -xf ~/bootstrap.tar.xz'
                sh 'autoreconf'
            }
        }
        stage('Configure') {
            steps {
                sh '''
                    eval $(opam env) && \
                    mkdir build && cd build && \
                    ../configure \
                        --with-llvm-config=/media/llvms/7.0.1/bin/llvm-config
                   '''
            }
        }
        stage('Build') {
            steps {
                sh 'make -C build clangml'
                sh 'make -C build stubgen'
            }
        }
        stage('Generate stubs') {
            steps {
                script {
                    def pwd = sh (
                        script: 'echo $PWD',
                        returnStdout: true
                    ).trim()
                    def llvm_versions = sh (
                        script: 'ls -1 /media/llvms',
                        returnStdout: true
                    ).split('\n').toList()
                    llvm_versions.retainAll { it =~ /[0-9][.][0-9][.][0-9]/ }
                    def branches = [:]
                    for (i in llvm_versions) {
                        def llvm_version = i
                        def llvm_dir = "/media/llvms/$llvm_version"
                        def llvm_config = "$llvm_dir/bin/llvm-config"
                        def bootstrap_dir = "bootstrap/$llvm_version"
                        def include_dir =
                            "$llvm_dir/lib/clang/$llvm_version/include"
                        def cc
                        def cxx
                        branches[llvm_version] = {
                            node {
                                sh """
                                    cd $pwd && \
                                    mkdir -p $bootstrap_dir && \
                                    build/_build/default/stubgen/stubgen.exe \
                                        --cc=-I,build,-I,$include_dir \
                                        --llvm-config=$llvm_config \
                                        $bootstrap_dir/
                                   """
                                sh """
                                    cd $pwd && \
                                    mkdir $llvm_version/ && \
                                    cd $llvm_version/ && \
                                    ../configure \
                                        --with-llvm-config=$llvm_config && \
                                    make clangml
                                   """
                                sh "cd $pwd/$llvm_version/ && make tests"
                                sh """
                                    cd $pwd/$llvm_version/ && \
                                    make stubgen && \
                                    mkdir current && \
                                    _build/default/stubgen/stubgen.exe \
                                        --cc=-I,build,-I,$include_dir \
                                        --llvm-config=$llvm_config \
                                        current/ && \
                                    diff clangml/clang__bindings.ml \
                                      current/clang__bindings.ml && \
                                    diff clangml/clang__bindings.mli \
                                      current/clang__bindings.mli && \
                                    diff clangml/clang_stubs.c \
                                      current/clang_stubs.c
                                   """
                            }
                        }
                    }
                    parallel branches
                }
                sh 'tar -cf bootstrap.tar.xz bootstrap/'
                archiveArtifacts artifacts: 'bootstrap.tar.xz',
                    fingerprint: true
            }
        }
        stage('Commit to bootstrap branch') {
            when { branch 'master' }
            steps {
                script {
                    def commit = sh (
                        script: 'git rev-parse HEAD',
                        returnStdout: true
                    ).trim()
                    sh 'git checkout origin/bootstrap'
                    sh 'tar -xf bootstrap.tar.xz'
                    sh 'git add bootstrap'
                    sh """
                        git commit -m 'generated files for commit $commit' || \
                        true
                       """
                    sh 'git push origin HEAD:bootstrap'
                    sh 'git checkout master'
                }
            }
        }
        stage('opam installation') {
            when { branch 'master' }
            steps {
                script {
                    sh """
                        docker run --rm -v $PWD:/clangml ocaml/opam2:4.07 \
                            /clangml/opam-pin-and-install.sh
                       """
                }
            }
        }
        stage('Commit to release branch') {
            when { branch 'master' }
            steps {
                script {
                    def commit = sh (
                        script: 'git rev-parse HEAD',
                        returnStdout: true
                    ).trim()
                    sh 'git checkout origin/releases'
                    sh 'git reset --soft origin/master'
                    sh '''
                        grep -q AM_MAINTAINER_MODE configure.ac || \
                        echo AM_MAINTAINER_MODE >>configure.ac
                       '''
                    sh './bootstrap.sh'
                    sh 'git add -f configure.ac Makefile.in aclocal.m4 configure bootstrap'
                    sh "git commit -m 'bootstrapped repository for commit $commit'"
                    sh 'git push origin HEAD:releases'
                    sh 'git -f -a "devel" -m "Development version"'
                    sh 'git push -f origin devel'
                    sh 'git checkout master'
                }
            }
        }
        stage('opam installation from devel tag') {
            steps {
                script {
                    sh """
                        docker run --rm -v ocaml/opam2:4.07 \
                        /clangml/opam-pin-and-install.sh \
   https://gitlab.inria.fr/tmartine/clangml/-/archive/devel/clangml-devel.tag.gz
                       """
                }
            }
        }
    }
    post {
        failure {
            mail to: 'Thierry.Martinez@inria.fr',
                subject: "ClangML CI failure: ${currentBuild.fullDisplayName}",
                body: "Something is wrong with ${env.BUILD_URL}"
        }
        changed {
            mail to: 'Thierry.Martinez@inria.fr',
                subject:
                  "ClangML CI status changed: ${currentBuild.fullDisplayName}",
                body: "Something changed with ${env.BUILD_URL}"
        }
    }
}

pipeline {
    agent {
        label 'slave'
    }
    stages {
        stage('Prepare') {
            steps {
                sh 'mkdir src'
                sh 'mv * src/ || true'
                sh '''
                    cd src && \
                    rm -rf bootstrap/ && \
                    tar -xf ~/bootstrap.tar.xz && \
                    aclocal && automake --add-missing && autoreconf
                   '''
            }
        }
        stage('Configure') {
            steps {
                sh '''
                    eval $(opam env) && \
                    mkdir build && cd build && \
                    ../src/configure \
                        --with-llvm-config=/media/llvms/8.0.1/bin/llvm-config
                   '''
            }
        }
        stage('Build') {
            steps {
                sh 'make -C build clangml'
                sh 'make -C build clangml.opam && cp build/clangml.opam src/'
                sh 'make -C build tools/stubgen'
                sh 'make -C build tools/norm_extractor'
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
                        def bootstrap_dir = "src/bootstrap/$llvm_version"
                        def include_dir =
                            "$llvm_dir/lib/clang/$llvm_version/include"
                        def cc
                        def cxx
                        branches[llvm_version] = {
                            node {
                                sh """
                                    cd $pwd && \
                                    mkdir -p $bootstrap_dir && \
                                    build/_build/default/tools/stubgen/stubgen.exe \
                                        --cc=-I,build,-I,$include_dir \
                                        --llvm-config=$llvm_config \
                                        $bootstrap_dir/
                                   """
                                sh """
                                    cd $pwd && \
                                    mkdir $llvm_version/ && \
                                    cd $llvm_version/ && \
                                    ../src/configure \
                                        --with-llvm-config=$llvm_config && \
                                    make clangml
                                   """
                                sh "cd $pwd/$llvm_version/ && make tests"
                                sh """
                                    cd $pwd/$llvm_version/ && \
                                    make tools/stubgen && \
                                    mkdir current && \
                                    _build/default/tools/stubgen/stubgen.exe \
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
                sh 'cd src && tar -cf bootstrap.tar.xz bootstrap/'
                archiveArtifacts artifacts: 'src/bootstrap.tar.xz',
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
                    sh 'tar -xf src/bootstrap.tar.xz'
                    sh 'git add bootstrap'
                    sh """
                        git commit -m 'generated files for commit $commit' || \
                        true
                       """
                    sh 'git push origin HEAD:bootstrap'
                }
            }
        }
        stage('opam installation') {
            when { branch 'master' }
            steps {
                sh '''
                    docker run --rm -v $PWD/src:/clangml ocaml/opam2:4.07 \
                        /clangml/ci-scripts/opam-pin_and_install.sh \
                        file:///clangml/
                   '''
            }
        }
        stage('Commit to snapshot branch') {
            when { branch 'master' }
            steps {
                sh 'src/ci-scripts/commit-snapshot-branch.sh'
            }
        }
        stage('opam installation from snapshot') {
            steps {
                sh '''
                    docker run --rm -v $PWD:/clangml ocaml/opam2:4.07 \
                        /clangml/ci-scripts/opam-pin_and_install.sh \
https://gitlab.inria.fr/tmartine/clangml/-/archive/snapshot/clangml-snapshot.tar.gz
                   '''
            }
        }
        stage('Extract norms') {
            parallel {
                stage('c++14') {
                    steps {
                        sh "cd $(HOME)/cplusplus/c++14/ && $(PWD)/build/_build/default/tools/norm_extractor/norm_extractor.exe --trigraphs -x c++ --std c++14 -i -o $(PWD)/norm_cxx14.ml `sed -n -e 's/^\\\\include{\\([^}]*\\)}/\\1/p' $(HOME)/cplusplus/c++14/std.tex`"
                    }
                }
                stage('c++17') {
                    steps {
                        sh "cd $(HOME)/cplusplus/c++17/ && $(PWD)/build/_build/default/tools/norm_extractor/norm_extractor.exe --trigraphs -x c++ --std c++17 -i -o $(PWD)/norm_cxx17.ml `sed -n -e 's/^\\\\include{\\([^}]*\\)}/\\1/p' $(HOME)/cplusplus/c++17/std.tex`"
                    }
                }
            }
        }
        stage('Commit to norms branch') {
            when { branch 'master' }
            steps {
                sh 'git checkout norms'
                sh 'cp norm_cxx14.ml norms/'
                sh 'cp norm_cxx17.ml norms/'
                sh 'git add norms/*'
                sh 'git commit -m "generated files for commit `git rev-parse master`"'
                sh 'git push'
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

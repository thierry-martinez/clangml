pipeline {
    agent {
        label 'slave'
    }
    stages {
        stage('Prepare') {
            steps {
                sh 'tar -xf ~/bootstrap.tar.xz'
                sh 'aclocal'
                sh 'automake --add-missing'
                sh 'autoreconf'
            }
        }
        stage('Configure') {
            steps {
                sh 'eval $(opam env) && ./configure --with-llvm-config=/media/llvms/6.0.1/bin/llvm-config'
            }
        }
        stage('Build') {
            steps {
                sh 'make clangml'
                sh 'make stubgen'
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
                        branches[llvm_version] = {
                            node {
                                sh "cd $pwd && mkdir -p bootstrap/$llvm_version/ && _build/default/stubgen/stubgen.exe --cc=-I,/media/llvms/$llvm_version/lib/clang/$llvm_version/include /media/llvms/$llvm_version/bin/llvm-config bootstrap/$llvm_version/"
                            }
                        }
                    }
                    parallel branches
                }
                sh 'tar -cf bootstrap.tar.xz bootstrap/'
                archiveArtifacts artifacts: 'bootstrap.tar.xz', fingerprint: true
            }
        }
    }
}

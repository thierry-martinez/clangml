pipeline {
    agent {
        label 'slave'
    }
    stages {
        stage('Prepare') {
            steps {
                sh 'mkdir src'
                sh 'mv * src/ || true'
                sh 'cd src && tar -xf ~/bootstrap.tar.xz && aclocal && automake --add-missing && autoreconf'
            }
        }
        stage('Configure') {
            steps {
                sh 'eval $(opam env) && mkdir build && cd build && ../src/configure --with-llvm-config=/media/llvms/6.0.1/bin/llvm-config'
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
                        branches[llvm_version] = {
                            node {
                                sh "cd $pwd && mkdir -p build/bootstrap/$llvm_version/ && build/_build/default/stubgen/stubgen.exe --cc=-I,/media/llvms/$llvm_version/lib/clang/$llvm_version/include /media/llvms/$llvm_version/bin/llvm-config build/bootstrap/$llvm_version/"
                                sh "cd $pwd && mkdir $llvm_version/ && cd $llvm_version/ && ../src/configure --with-llvm-config=/media/llvms/$llvm_version/bin/llvm-config && make clangml stubgen"
                            }
                        }
                    }
                    parallel branches
                }
                sh 'cd build && tar -cf bootstrap.tar.xz bootstrap/'
                archiveArtifacts artifacts: 'build/bootstrap.tar.xz', fingerprint: true
            }
        }
    }
}

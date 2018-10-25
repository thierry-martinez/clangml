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
                sh 'mkdir -p bootstrap/6.0.1/ && _build/default/stubgen/stubgen.exe --cc=-I,/media/llvms/6.0.1/lib/clang/6.0.1/include /media/llvms/6.0.1/bin/llvm-config bootstrap/6.0.1/'
            }
        }
    }
}

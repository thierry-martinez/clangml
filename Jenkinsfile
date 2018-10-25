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
                sh './configure --with-llvm-config=/media/llvms/6.0.1/bin/llvm-config'
            }
        }
        stage('Build') {
            steps {
                sh 'make'
            }
        }
    }
}

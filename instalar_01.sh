#Este script instala en ambiente/ Google Cloud
#Librerias y utilidades basicas de  Ubuntu 22.04
#Lenguajes   Julia, Python y R
#Librerias de los lenguajes para hacer ciencia de datos  (no deep learning, no GPU )
#Entornos productivos   RStudio, Jupyter Lab
#conectividad a los Storage Buckets de  Google Cloud

#autor            Gustavo Denicolay
#email            gustavo.denicolay@gmail.com
#fecha creacion   2017-07-20
#fecha revision   2022-05-30 22:59
#Known BUGS       
#ToDo Optims      


mkdir  -p  ~/install
mkdir  -p  ~/log


#instalo Google Cloud SDK
#Documentacion  https://cloud.google.com/sdk/docs/install#deb
sudo  DEBIAN_FRONTEND=noninteractive  apt-get update
sudo  DEBIAN_FRONTEND=noninteractive  apt-get --yes install apt-transport-https ca-certificates gnupg
echo  "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
curl  https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -
sudo  DEBIAN_FRONTEND=noninteractive  apt-get update && sudo DEBIAN_FRONTEND=noninteractive  apt-get --yes install google-cloud-sdk

#instalo paquetes que van a hacer falta para R, Python y Julia
sudo  DEBIAN_FRONTEND=noninteractive  apt-get update  && sudo dpkg --add-architecture  i386

#Herramientas de desarrollo
sudo  DEBIAN_FRONTEND=noninteractive  apt-get --yes install  software-properties-common  build-essential

#librerias necesarias para R, Python, Julia, JupyterLab
sudo  DEBIAN_FRONTEND=noninteractive  apt-get --yes install  libssl-dev    \
      libcurl4-openssl-dev  libxml2-dev    \
      libgeos-dev  libproj-dev             \
      libgdal-dev  librsvg2-dev            \
      ocl-icd-opencl-dev  libmagick++-dev  \
      libv8-dev  libsodium-dev             \
      libharfbuzz-dev  libfribidi-dev      \
      pandoc texlive  texlive-xetex        \
      texlive-fonts-recommended            \
      texlive-latex-recommended            \
      cmake  gdebi  curl  sshpass  nano    \
      vim  htop  iotop  iputils-ping       \
      cron  tmux  git-core  zip  unzip     \
      sysstat  smbclient cifs-utils  rsync

#------------------------------------------------------------------------------

#instalo Google Cloud Fuse  para poder ver el bucket  Version:  0.41.1 | Released:2022-04-28
#Documentacion https://cloud.google.com/storage/docs/gcs-fuse?hl=en-419
gcsfusever="0.41.1"
gcsfusepack="gcsfuse_"$gcsfusever"_amd64.deb"
cd
curl  -L -O "https://github.com/GoogleCloudPlatform/gcsfuse/releases/download/v$gcsfusever/$gcsfusepack"
sudo  DEBIAN_FRONTEND=noninteractive  dpkg --install $gcsfusepack
rm   ~/$gcsfusepack


#Preparo para que puedan haber 9 buckets al mismo tiempo
mkdir  -p  ~/buckets
mkdir  -p  ~/buckets/b1
mkdir  -p  ~/buckets/b2
mkdir  -p  ~/buckets/b3
mkdir  -p  ~/buckets/b4
mkdir  -p  ~/buckets/b5
mkdir  -p  ~/buckets/b6
mkdir  -p  ~/buckets/b7
mkdir  -p  ~/buckets/b8
mkdir  -p  ~/buckets/b9


cat > /home/$USER/install/linkear_buckets.sh <<FILE
#!/bin/bash

/snap/bin/gsutil ls | sed -r 's/gs:\/\///' | sed 's/.$//'       \
|  sed 's/^/\/usr\/bin\/gcsfuse --implicit-dirs --file-mode 777 --dir-mode 777    /'    \
|  sed 's/$/ \/home\/$USER\/buckets\/b/'    \
|  awk '{ print \$0, NR}'   \
|  sed -E 's/.(.)$/\1/'   >  /home/$USER/install/linkear_buckets2.sh

chmod  u+x  /home/$USER/install/linkear_buckets2.sh
/home/$USER/install/linkear_buckets2.sh

FILE

chmod  u+x /home/$USER/install/*.sh
chmod  u+x /home/$USER/*.sh


cat > /home/$USER/install/buckets.service  <<FILE
[Unit]
Description=buckets

[Service]
Type=forking
ExecStart=/home/$USER/install/linkear_buckets.sh
WorkingDirectory=/home/$USER/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE

sudo  cp   /home/$USER/install/buckets.service   /etc/systemd/system/

sudo  systemctl daemon-reload


sudo  systemctl enable /etc/systemd/system/buckets.service
sudo  systemctl start  buckets

# systemctl status buckets

#------------------------------------------------------------------------------
#instalo dataset grande  Aplicaciones de Mineria de Datos a Economia y Finanzas, UTN , 2022

cat > /home/$USER/install/instalo_datasets.sh <<FILE
#!/bin/bash

mkdir  -p  ~/buckets/b1/datasets
mkdir  -p  ~/buckets/b1/exp
mkdir  -p  ~/buckets/b1/log

cd  ~/buckets/b1/datasets
wget  https://storage.googleapis.com/utn2022p/paquete_premium.csv.gz

cd
FILE

chmod  u+x ~/install/*.sh
~/install/instalo_datasets.sh
cd

#------------------------------------------------------------------------------
#instalar  R   version: 4.2.0 | released: 2022-04-22
#Documentacion  https://cran.r-project.org/

wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo gpg --dearmor -o /usr/share/keyrings/r-project.gpg
echo "deb [signed-by=/usr/share/keyrings/r-project.gpg] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" | sudo tee -a /etc/apt/sources.list.d/r-project.list
sudo  DEBIAN_FRONTEND=noninteractive  apt-get update

sudo  DEBIAN_FRONTEND=noninteractive  apt-get install  --yes  --no-install-recommends r-base r-base-dev


#Instalo RStudio Server    Version:  2022.02.2+485 | Released:  2022-04-27-----------
#Documentacion  https://rstudio.com/products/rstudio/download-server/debian-ubuntu/

#primero, fuerzo la instalacion de libssl1.1
cd
echo  "deb http://security.ubuntu.com/ubuntu impish-security main" | sudo tee /etc/apt/sources.list.d/impish-security.list
sudo  DEBIAN_FRONTEND=noninteractive  apt-get update
sudo  DEBIAN_FRONTEND=noninteractive  apt-get install libssl1.1

cd
rstudiopack="rstudio-server-2022.02.2-485-amd64.deb"
wget  https://download2.rstudio.org/server/bionic/amd64/"$rstudiopack"
sudo  DEBIAN_FRONTEND=noninteractive  gdebi -n $rstudiopack
rm    $rstudiopack


#cambio el puerto del Rstudio Server al 80 para que se pueda acceder en aulas de universidades detras de firewalls
#Documentacion  https://support.rstudio.com/hc/en-us/articles/200552316-Configuring-the-Server
echo "www-port=80" | sudo tee -a /etc/rstudio/rserver.conf
sudo  rstudio-server restart

#rstudio-server status

#------------------------------------------------------------------------------
#instalacion de  innumerables paquetes de R
#esta es la parte que mas tiempo insume del script

R_LIBS_USER=/home/$USER/.local/lib/R/site-library
mkdir  -p $R_LIBS_USER

cat > /home/$USER/.Renviron  <<FILE
R_LIBS_USER=$R_LIBS_USER
FILE


#Primera instalacion de paquetes de R
cat > /home/$USER/install/instalar_paquetes_1.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

install.packages( "data.table",  dependencies= TRUE, lib="/home/$USER/.local/lib/R/site-library")
install.packages( "devtools",  dependencies= TRUE, lib="/home/$USER/.local/lib/R/site-library")

library( "devtools" )
devtools::install_github("IRkernel/IRkernel")

quit( save="no" )
FILE

Rscript --verbose  /home/$USER/install/instalar_paquetes_1.r



#Segunda instalacion de paquetes de R
cat > /home/$USER/install/instalar_paquetes_2.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

paq1 <- c("yaml", "rlist")
paq2 <- c("magrittr", "stringi", "curl", "Rcpp", "Matrix", "glm2")
paq3 <- c("ROCR", "MASS", "openssl", "roxygen2")
paq4 <- c("rsvg", "DiagrammeRsvg", "DiagrammeR")
paq5 <- c("DiceKriging",  "mlrMBO", "ParBayesianOptimization" )
paq6 <- c("rpart", "rpart.plot", "ranger", "randomForest", "xgboost" )
paq7 <- c("dplyr", "caret", "tidyr", "shiny" )
paq8 <- c("SHAPforxgboost", "shapr", "mlflow", "visNetwork" )
paq9 <- c("iml","primes","stats","ggplot2","plotly")

paq <-  c( paq1, paq2, paq3, paq4, paq5, paq6, paq7, paq8, paq9 )

install.packages( paq,  dependencies= TRUE, lib="/home/$USER/.local/lib/R/site-library" )

library( "devtools" )
install_github( "AppliedDataSciencePartners/xgboostExplainer")
install_github( "NorskRegnesentral/shapr", dependencies = TRUE)

devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.0.4/catboost-R-Linux-1.0.4.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))

quit( save="no" )
FILE

Rscript --verbose  /home/$USER/install/instalar_paquetes_2.r



#xgboost instalo la ultima version de desarrollo de XGBoost
#Documentacion  https://xgboost.readthedocs.io/en/latest/build.html
cd  &&  rm -rf  xgboost
git clone --recursive https://github.com/dmlc/xgboost
cd xgboost
git submodule init
git submodule update
cd R-package
R CMD INSTALL .
cd  &&  rm -rf  xgboost


#LightGBM instalo version de desarrollo
#Documentacion  https://lightgbm.readthedocs.io/en/latest/Installation-Guide.html#linux
cd  &&  rm -rf  LightGBM
git clone --recursive https://github.com/Microsoft/LightGBM
cd LightGBM
Rscript ./build_r.R
cd  &&  rm -rf  LightGBM



#Tercera instalacion de paquetes de R
#era necesario tener XGBoost y LightGBM
cat > /home/$USER/install/instalar_paquetes_3.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

library( 'devtools' )
install_github("lantanacamara/lightgbmExplainer")

quit( save="no" )
FILE

Rscript --verbose  /home/$USER/install/instalar_paquetes_3.r

#------------------------------------------------------------------------------
#Instalo Python SIN  Anaconda, Miniconda, etc-------------------------------
#Documentacion  https://docs.python-guide.org/starting/install3/linux/

export PATH="$PATH:/home/$USER/.local/bin"
echo  "export PATH=/home/\$USER/.local/bin:\$PATH"  >>  ~/.bashrc 
source ~/.bashrc 

sudo  apt-get update
sudo  DEBIAN_FRONTEND=noninteractive  apt-get --yes install   python3  python3-pip  python3-dev  ipython3

#actualizo  pip
pip3 install --upgrade pip

#instalo paquetes de Python
pip3 install  --user  Pandas  \
                      Numpy  Matplotlib  fastparquet          \
                      pyarrow  tables  plotly  seaborn xlrd   \
                      scrapy  SciPy  wheel  testresources


#instalo  datatable desde su repositorio
pip3 install git+https://github.com/h2oai/datatable

pip3 install  --user  XGBoost  LightGBM  HyperOpt optuna
pip3 install  --user  TensorFlow  Theano  Keras

#librerias puntuales
pip3 install  --user  kaggle  zulip  pika  gdown  mlflow
pip3 install  --user  nbconvert[webpdf]  nb_pdf_template

#------------------------------------------------------------------------------
#Instalo Julia   version: 1.7.3  | Release Date: 2022-05-06 ---------------
#Documentacion  https://julialang.org/downloads/platform/#linux_and_freebsd

sudo  DEBIAN_FRONTEND=noninteractive  apt-get update

#Instalo la ultima version estable de Julia
juliatar="julia-1.7.3-linux-x86_64.tar.gz"
cd
wget https://julialang-s3.julialang.org/bin/linux/x64/1.7/"$juliatar"
tar   -xvzf  $juliatar
sudo  mv  julia-1.7.3/    /opt/
sudo  ln -s  /opt/julia-1.7.3/bin/julia    /usr/local/bin/julia
rm    $juliatar


#genero script con paquetes a instalar de Julia
#Documentacion  https://datatofish.com/install-package-julia/
cat > /home/$USER/install/instalar_paquetes_julia_1.jl <<FILE
using Pkg
Pkg.add("CSV")
Pkg.add("DataFrames")
Pkg.add("DataTables")
Pkg.add("Distributions")
Pkg.add("PooledArrays")
Pkg.add("JuliaDB")
Pkg.add("HTTP")
Pkg.add("XGBoost")
Pkg.add("DecisionTree")
Pkg.add("LightGBM")
Pkg.add("ROCAnalysis")
Pkg.add("BayesianOptimization")
Pkg.add("GaussianProcesses")
Pkg.add("Distributions")
Pkg.add("MLFlowLogger")
Pkg.add("MLFlowClient")
FILE

chmod  u+x    /home/$USER/install/instalar_paquetes_julia_1.jl
julia  /home/$USER/install/instalar_paquetes_julia_1.jl


#------------------------------------------------------------------------------
#instalo  Jupyter Lab ---------------------------------------------------------
#Documentacion  https://jupyterlab.readthedocs.io/en/stable/getting_started/installation.html

cd
git clone https://github.com/pygments/pygments
cd  pygments
python3  setup.py install  --user
cd
sudo  rm -rf ~/pygments

#la instalacion de jupyter lab
pip3  install --user  jupyter  jupyterlab

#instalo nvm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.39.1/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
nvm install --lts


#instalo extensiones de jupyter
jupyter labextension install \
   @jupyterlab/toc           \
   jupyterlab-chart-editor   \
   jupyterlab-spreadsheet


#instalo git para jupyter lab
pip3 install --upgrade jupyterlab jupyterlab-git

#------------------------------------------------------------------------------
#configuro R para que pueda usarse desde Jupyter, kernel de R
#Cuarta instalacion de paquetes de R
#Documentacion  https://developers.refinitiv.com/article/setup-jupyter-notebook-r

cat > /home/$USER/install/instalar_paquetes_4.r  <<FILE
options(repos = c("https://cloud.r-project.org/"))
options(Ncpus = 4)

install.packages("devtools" )
library( "devtools" )
devtools::install_github("IRkernel/IRkernel", force=TRUE)
library( "IRkernel" )
IRkernel::installspec()
quit( save="no" )
FILE

Rscript --verbose  /home/$USER/install/instalar_paquetes_4.r

#------------------------------------------------------------------------------
#Agrego el kernel de Julia a Jupyterlab

cat > /home/$USER/install/instalar_paquetes_julia_2.jl  <<FILE
using Pkg
Pkg.add("IJulia")
FILE

chmod  u+x      /home/$USER/install/instalar_paquetes_julia_2.jl
julia   /home/$USER/install/instalar_paquetes_julia_2.jl


#------------------------------------------------------------------------------
# Define home directory and data directory (adjust to your needs)

mkdir  -p /home/$USER/.jupyter/
USER_HOME_DIR=$(echo ~)
DATA_DIR="$USER_HOME_DIR"/
# Create the data directory

#------------------------------------------------------------------------------
#El servicio de Jupyter Lab

cat > /home/$USER/install/jupyterlab.service  <<FILE
[Unit]
Description=jupyterlab
[Service]
Type=simple
ExecStart=/home/$USER/.local/bin/jupyter-lab --no-browser --port=8888 --ip=0.0.0.0 --NotebookApp.token= --notebook-dir=$DATA_DIR
WorkingDirectory=/home/$USER/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE

sudo  cp   /home/$USER/install/jupyterlab.service   /etc/systemd/system/

sudo  systemctl enable /etc/systemd/system/jupyterlab.service
sudo  systemctl daemon-reload

#------------------------------------------------------------------------------
#para que se pueda ingresar a  Jupyter en forma remota

cat > /home/$USER/.jupyter/jupyter_notebook_config.py  <<FILE
c.NotebookApp.allow_origin = '*'
c.NotebookApp.ip = '0.0.0.0'
c.NotebookApp.use_redirect_file = False
c.NotebookApp.password = 'sha1:5f161fa66788:0fb62a8127ddeb137e585cb11043f0392f66f70b'
FILE


pip3  install --user  --upgrade nbconvert


#abro el puerto  8888  en Google Cloud  para Jupyter
#Documentacion  https://cloud.google.com/vpc/docs/using-firewalls#gcloud

gcloud compute firewall-rules create jupyter --allow tcp:8888 --source-tags=instance-instalacion --source-ranges=0.0.0.0/0 --description="jupyter"

#------------------------------------------------------------------------------
#establezco la configuracion del editor  vim

cat >  /home/$USER/.vimrc  <<FILE
set background=dark
colorscheme desert
FILE

#------------------------------------------------------------------------------
#establezco la configuracion de  tmux
#Documentacion  https://gist.github.com/paulodeleo/5594773

cat > /home/$USER/.tmux.conf  <<FILE
# remap prefix from 'C-b' to 'C-a'
unbind C-b
set-option -g prefix C-a
bind-key C-a send-prefix

# split panes using | and -
bind | split-window -h
bind - split-window -v
unbind '"'
unbind %

# reload config file (change file location to your the tmux.conf you want to use)
bind r source-file ~/.tmux.conf

# switch panes using Alt-arrow without prefix
bind -n M-Left select-pane -L
bind -n M-Right select-pane -R
bind -n M-Up select-pane -U
bind -n M-Down select-pane -D

set-option -g status-position top
bind -n F1 next-window

set  -g base-index 1
setw -g pane-base-index 1

set -g mouse on
FILE

#------------------------------------------------------------------------------
#activo sysstat

sudo  sed -i  's/5-55\/10/1-59\/1/' /etc/cron.d/sysstat
sudo  sed -i  's/ENABLED=\"false\"/ENABLED=\"true\"/'    /etc/default/sysstat
sudo  service sysstat restart

#------------------------------------------------------------------------------
#log del uso de MEMORIA  y CPU , para detectar cuando la memoria fue insuficiente

cd
cd install
wget  https://storage.googleapis.com/utn2022p/memcpu.c
cc  memcpu.c -o memcpu
cd


cat > /home/$USER/install/memcpu.service  <<FILE
[Unit]
Description=memcpu

[Service]
Type=simple
ExecStart=/home/$USER/install/memcpu
WorkingDirectory=/home/$USER/log/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE

sudo  cp   /home/$USER/install/memcpu.service   /etc/systemd/system/


sudo  systemctl enable /etc/systemd/system/memcpu.service
sudo  systemctl daemon-reload
sudo  systemctl start  memcpu

#--------------------------------------------------------------------------
#establezco  suicidio.sh  para cuando Google Cloud mate a la maquina virtual preemptible

cat > /home/$USER/suicidio.sh <<FILE
#!/bin/bash
cat  /home/$USER/log/memcpu.txt >> /home/$USER/buckets/b1/log/\$(hostname)_memcpu.txt
cat  /home/$USER/log/tracelog.txt >> /home/$USER/buckets/b1/log/\$(hostname)_tracelog.txt
FILE

chmod  u+x  /home/$USER/suicidio.sh

#--------------------------------------------------------------------------
#genero el script para cambiar la clave y poder ingresar al RStudio
#este script se corre MANUALMENTE  luego de la instalacion

cat > /home/$USER/cambiar_claves.sh <<FILE
#!/bin/bash
echo "ahora vamos a cambiar la password de Ubuntu y RStudio"
echo "se sugiere una facil compartible con su companeros de equipo"
sudo passwd \$USER

echo \"Tu nombre de usuario Ubuntu es :  \"  \$USER
echo ""
echo ""
echo "ahora vamos a cambiar la password de Jupyter"
echo "se sugiere fuertemente poner la misma password que la anterior"
PATH=$PATH:/home/$USER/.local/bin
DATA_DIR="$HOME"
sudo  systemctl stop  jupyterlab

jupyter lab password

sudo  systemctl start jupyterlab
FILE

chmod  u+x   /home/$USER/*.sh

#--------------------------------------------------------------------------
#agrego servicio para memoria virtual swap la mitad del espacio LIBRE en disco
#Documentacion  https://linuxize.com/post/how-to-add-swap-space-on-ubuntu-20-04/


cat > /home/$USER/install/halfswap.sh  <<FILE
#!/bin/bash
myUsed=\$( df -BG /dev/root  | tail -1 | awk '{print \$4}' | sed 's/.$//' )
echo \$myUsed
available=\$(( myUsed / 2))
echo \$available

sudo  fallocate -l "\$available"G  /swap
sudo  chmod 600 /swap
sudo  mkswap /swap
sudo  swapon /swap
FILE

chmod  u+x /home/$USER/install/halfswap.sh 


cat > /home/$USER/install/halfswap.service  <<FILE
[Unit]
Description=halfswap

[Service]
Type=oneshot
ExecStart=/home/$USER/install/halfswap.sh
WorkingDirectory=/home/$USER/

[Install]
WantedBy=default.target
FILE

sudo  cp   /home/$USER/install/halfswap.service   /etc/systemd/system/


sudo  systemctl enable /etc/systemd/system/halfswap.service
sudo  systemctl daemon-reload

#------------------------------------------------------------------------------
#agrego servicio para hacer un git pull del repositorio

cat > /home/$USER/install/repo.sh  <<FILE
#!/bin/bash
cd /home/$USER/labo
git pull
cd /home/$USER/
FILE

chmod  u+x /home/$USER/install/repo.sh 


cat > /home/$USER/install/repo.service  <<FILE
[Unit]
Description=repo

[Service]
Type=oneshot
ExecStart=/home/$USER/install/repo.sh
WorkingDirectory=/home/$USER/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE

sudo  cp   /home/$USER/install/repo.service   /etc/systemd/system/


sudo  systemctl enable /etc/systemd/system/repo.service
sudo  systemctl daemon-reload

#------------------------------------------------------------------------------
#agrego autoexec

cat > /home/$USER/install/runatboot.sh  <<FILE
#!/bin/bash
cd /home/$USER/install
rm autoexec.sh
wget  https://storage.googleapis.com/utn2022p/autoexec.sh
chmod u+x autoexec.sh
./autoexec.sh
FILE

chmod  u+x /home/$USER/install/runatboot.sh 


cat > /home/$USER/install/runatboot.service  <<FILE
[Unit]
Description=runatboot

[Service]
Type=oneshot
ExecStart=/home/$USER/install/runatboot.sh
WorkingDirectory=/home/$USER/
User=$USER
Group=$USER

[Install]
WantedBy=default.target
FILE

sudo  cp   /home/$USER/install/runatboot.service   /etc/systemd/system/


sudo  systemctl enable /etc/systemd/system/runatboot.service
sudo  systemctl daemon-reload


#------------------------------------------------------------------------------
#ULTIMO  paso  Reacomodo la instalacion

sudo  DEBIAN_FRONTEND=noninteractive   apt-get  --yes  update
sudo  DEBIAN_FRONTEND=noninteractive   apt-get  --yes  dist-upgrade
sudo  DEBIAN_FRONTEND=noninteractive   apt-get  --yes  autoremove

#------------------------------------------------------------------------------

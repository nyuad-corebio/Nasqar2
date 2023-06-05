#!/bin/bash
source /home/shiny/miniconda3/etc/profile.d/conda.sh
conda activate nasqar_env
exec shiny-server >> /opt/nasqar_build/logs/shiny-server.log 2>&1

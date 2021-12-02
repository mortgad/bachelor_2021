#!/usr/bin/env bash

ENV=base
set -eux

conda install -y -n $ENV -c conda-forge \
numpy=1.21.4 \
pandas=1.3.4 \
matplotlib=3.5.0 \
joblib=0.11 \
scipy=0.19.1 \
threadpoolctl=2.0.0 \
scikit-learn=0.23.0 \

# pip install scikit-learn==0.23.0

<img src="https://github.com/COSIMA/logo/blob/master/png/logo_word.png" width="300"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="https://www.access-nri.org.au/wp-content/themes/accessnri/images/logos/access_logo_rgb.svg" width="200"/>
<br/> <br/>
[![Compilation Status](https://github.com/COSIMA/access-om3/actions/workflows/compilation.yml/badge.svg)](https://github.com/COSIMA/access-om3/actions/workflows/compilation.yml)

# ACCESS-OM3

ACCESS-OM3 is a global coupled ocean - sea ice - wave model being developed by [COSIMA](http://www.cosima.org.au) and [ACCESS-NRI](https://www.access-nri.org.au/). It consists of MOM6, CICE6 and (optionally) WaveWatch3, coupled with NUOPC and driven by prescribed surface forcing - see [here](https://access-om3-configs.access-hive.org.au/pages/Architecture/).

# Downloading

This repository only stores the model source code. Model configurations are available from branches in [github.com/ACCESS-NRI/access-om3-configs](https://github.com/ACCESS-NRI/access-om3-configs), which use pre-built executables available on [NCI](https://nci.org.au/), so NCI-based users of ACCESS-OM3 typically won't need to download this repository unless they need to make model component code changes.

This respository contains submodules, so you will need to clone it with the `--recursive` flag:
```
git clone --recursive https://github.com/COSIMA/access-om3.git
```

To update a previous clone of this repository to the latest version, you will need to do 
```
git pull
git submodule update --init --recursive
```
to update all the submodules.

# Documentation (including building and running the model)

See [access-om3-configs.access-hive.org.au](https://access-om3-configs.access-hive.org.au/), particularly the [quick start](https://access-om3-configs.access-hive.org.au/pages/Quick-start/).

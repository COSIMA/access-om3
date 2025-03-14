<img src="https://github.com/COSIMA/logo/blob/master/png/logo_word.png" width="300"/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<img src="https://www.access-nri.org.au/wp-content/themes/accessnri/images/logos/access_logo_rgb.svg" width="200"/>
<br/> <br/>
[![Compilation Status](https://github.com/COSIMA/access-om3/actions/workflows/compilation.yml/badge.svg)](https://github.com/COSIMA/access-om3/actions/workflows/compilation.yml)

<i><b>WARNING:</b> this is a pre-release version!</i>

# ACCESS-OM3

ACCESS-OM3 is a global coupled ocean - sea ice - wave model being developed by [COSIMA](http://www.cosima.org.au) and [ACCESS-NRI](https://www.access-nri.org.au/). 

This repository only stores the model source code. The model configurations are available from separate repositories, as explained [here](https://github.com/COSIMA/access-om3/wiki/Configurations#cmeps-coupled-configurations).

# Downloading

NCI-based users of ACCESS-OM3 typically won't need to download this repository, as there are pre-built executables available on NCI which are used by the [standard configurations](https://github.com/COSIMA/access-om3/wiki/Configurations#cmeps-coupled-configurations); see the [quick start instructions](https://github.com/COSIMA/access-om3/wiki/Quick-start) for how to run these.

This respository contains submodules, so you will need to clone it with the `--recursive` flag:
```
git clone --recursive https://github.com/COSIMA/access-om3.git
```

To update a previous clone of this repository to the latest version, you will need to do 
```
git pull
```
followed by
```
git submodule update --init --recursive
```
to update all the submodules.

# Further information (including building and running the model)

See the [ACCESS-OM3 wiki](https://github.com/COSIMA/access-om3/wiki), particularly the [quick start](https://github.com/COSIMA/access-om3/wiki/Quick-start).

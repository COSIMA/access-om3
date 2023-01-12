<img src="https://github.com/COSIMA/logo/blob/master/png/logo_word.png" width="300"/><img src="https://www.access-nri.org.au/wp-content/themes/accessnri/images/logos/access_logo_rgb.svg" width="200"/>
<br/> <br/>

# ACCESS-OM3

ACCESS-OM3 is a global coupled ocean - sea ice - wave model being developed by [COSIMA](http://www.cosima.org.au) and [ACCESS-NRI](https://www.access-nri.org.au/). 

# Downloading

<i><b>WARNING:</b> this is a very early pre-release development version and doesn't yet work!</i>

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

# Where to find information

For information on downloading, building and running the model, see the [ACCESS-OM3 wiki](https://github.com/COSIMA/access-om3/wiki).

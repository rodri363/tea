Tea is a tasty beverage for use while post-processing surveys, including editing bad data, imputing missing data, and disclosure avoidance.

## Installation for Developers

Tea depends on [Rapophenia](http://github.com/b-k/Rapophenia), also hosted here on Github, which in turn depends on the Apophenia library of stats functions in plain C. R's package management system has no real mechanism for installing C libraries, so you have to install Apophenia yourself before installing this package. See [Apophenia's setup page](http://apophenia.info/setup.html) for details.

Once that is installed, we recommend installing from this repository via ```devtools```:

```
library(devtools)
install_github("b-k/Rapophenia", ref="pkg")
install_github("rodri363/tea", ref="pkg")
```

## Installation for Users

Tea can be installed on any Linux system.  A script called "take tea" will extract the necessary files to run tea.  The script also modifies the users ~/.bashrc file, which allows execution of Tea through R.
# Compiler
## Setup
This project uses a program called Stack to manage Haskell's dependencies. The easiest way to install on any Linux or Unix system (including Mac) is to run `curl -sSL https://get.haskellstack.org/ | sh`. See https://docs.haskellstack.org/en/stable/install_and_upgrade/ for more information.

Alternatively, on Ubuntu and Debian, it can be installed with `sudo apt-get install haskell-stack`. On Fedora, I'm pretty sure it can be installed with

```
sudo dnf copr enable petersen/stack2
sudo dnf install stack
```

On Mac, using the Homebrew package manager, it can be installed with `brew install haskell-stack`.

## Executing
To run this program, you can just type `./Micro <input file name>` into the command line. Note that the first time it runs, it may need to download and compile dependencies. This may take a while, depending on the system. On Windows, for instance, I have found that it can take up to ten minutes, while on Mac, it can take about 2 minutes.
# BookThief

BookThief prepares PDFs to be home-printed and home-bound in "pamphlet"-style.

    For those of us who prefer print over screens

## Installation

On Ubuntu-based distros, BookThief can be easily installed via the BookThief PPA:

```
sudo add-apt-repository ppa:rail5/bookthief
sudo apt-get update
sudo apt-get install bookthief
```

## What is "pamphlet"-style printing?

When BookThief spits out a new/converted PDF, you simply print it at home, double-sided, and then fold the stack of papers in half. (And staple them, if you want)

![Like this](https://cdn.zmescience.com/wp-content/uploads/2014/07/folding_paper.jpg)

![Screenshot](./bookthief-screenshot1.png)

![Screenshot](./bookthief-screenshot2.png)

Be sure to tell your printer to print double-sided by flipping along the *short edge* (Portrait)

## Features

- GUI (bookthief) & command-line (liesel)

- Supports "splitting" PDFs into more manageable segments (based on how much paper you can realistically staple together)


## Build Requirements
- wxWidgets, as in these packages:
 - wx3.0-headers
 - libwxgtk3.0-gtk3-dev

## Building
```
make
sudo make install
```

Note that BookThief **will** build without Liesel, but it will not **run** without Liesel.

If you install BookThief via the PPA, Liesel comes with it in the same PPA. Otherwise, the source for Liesel can be obtained here: https://gitlab.com/rail5-bookthief/liesel
## License

BookThief & Liesel are **free software**, distributed under the GNU GPL V3.0 License

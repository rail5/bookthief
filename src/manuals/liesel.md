# NAME

liesel - Convert PDFs to "pamphlet" style for home printing

# SYNOPSIS

```bash
liesel input.pdf [options] output.pdf
```

# DESCRIPTION

Liesel is a command-line tool that prepares PDFs to be DIY printed at home in "pamphlet" style

# OPTIONS

###### `-r, --range <range>`

Print only the specified page ranges. E.g.: `-r 1-5,8,11-`

Each range can be:

 - `X-Y`: pages X to Y inclusive
 - `X-`: pages X to the end
 - `-X`: pages 1 to X inclusive
 - `X`: page X only

Pages are 1-indexed. Multiple ranges can be separated by commas.

If you give a range like `20-1` (i.e., where the start is larger than the end), this will print those pages in *reverse* order.

###### `-s, --segment <size>`

Print multiple PDFs in segments of `<size>` pages each.

If you are sourcing from a PDF which has, for example, 100 pages, you may want to split the output into multiple booklets of say 40 pages each. In this case you could pass `-s 40`.

Each output PDF will however *always* have a number of pages which is a multiple of *4*. This is simply how folio sheets work -- without this constraint, pamphlet-style printing would not be possible.

###### `-t, --rescale <size>`

Rescale the pages to fit on a specific size paper.

Supported sizes:

 - `us-letter`
 - `us-legal`
 - `a3`
 - `a4`
 - `a5`

You can provide any of the above strings, or a custom size in the format `<width>x<height>`, where width and height are in inches. For example: `8.5x11` for US Letter size.

###### `-d, --density <DPI>`

Set the output PDF's resolution in DPI (dots per inch). Default is 100 DPI.

###### `-l, --landscape`

Calibrate the outputted PDFs for landscape duplexing (that is, flipping on the long edge). By default, Liesel assumes portrait duplexing (flipping on the short edge).

This option depends on the settings of your printer. If you're uncertain which to use, try printing a test page both ways and see which one comes out correct.

###### `-p, --portrait`

Calibrate the outputted PDFs for portrait duplexing (that is, flipping on the short edge). This is the default behavior.

###### `-g, --greyscale`

Output the PDF in greyscale. By default, Liesel preserves color information.

###### `-k, --threshold <level>`

Convert all colors to either 100% black or 100% white, based on the given threshold level (0-100).

The threshold level essentially means: any color with brightness below the threshold becomes black, and any color with brightness above the threshold becomes white.

This option may be useful when printing scanned PDFs of books with yellowed pages, for example, to avoid wasting ink on the page background.

###### `-c, --crop <Left,Right,Top,Bottom>`

Crop the pages by the specified amounts (percentages) from each side.

For example, `-c 5,10,15,20` would crop 5% from the left, 10% from the right, 15% from the top, and 20% from the bottom of each page.

###### `-w, --widen-margins <amount>`

Widen the center margins by the specified amount.

###### `-D, --divide`

Split the left/right halves of each page into separate pages before arranging.

Many scanned books have two "real pages" per "PDF page." For example, a scanned book page may show both the left and right pages (say, both page 1 and 2) of an open book. This option splits each such page into two separate pages before arranging them into pamphlet style.

###### `-N, --no-booklet`

Apply all transformations except the pamphlet-style arrangement.

This option will apply your cropping, rescaling, greyscaling, etc., but will not rearrange the pages into pamphlet style. This option turns Liesel into a more general PDF manipulation tool.

###### `-V, --verbose`

Enable verbose output, showing detailed processing information.

###### `-v, --version`

Display version and copyright information about Liesel.

###### `-h, --help`

Show help information about Liesel and its options.

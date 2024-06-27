# Bar_Code_Drawing
Creation of bar codes

Bar_Code_Drawing is a library for creating bar codes, inspired by de Montmollin's [Ada Bar Codes](https://github.com/zertovitch/ada-bar-codes), but hopefully simpler and easier to use.

Currently, Code 128, MSI, QR, and UPC-A/EAN-13 codes are implemented. An additional type of bar code may be implemented by adding a package to the Bar_Code_Drawing.What.* hierarchy with an appropriate Draw procedure.

Currently, drawing to an [Ada-GUI](https://github.com/jrcarter/Ada_GUI) Graphic_Area and creation of BMP, PBM, and SVG files are implemented. An additional representation may be implemented by adding a package to the Bar_Code_Drawing.How.* hierarchy with an appropriate operation.

A BMP file can easily be converted to most other graphic file formats using existing tools, and is widely supported.

A PBM file can easily be converted to most other graphic file formats using existing tools, but PBM files are not supported by default on all platforms.

An [Ada-GUI](https://github.com/jrcarter/Ada_GUI) program requires the GNAT compiler, so using Bar_Code_Drawing.How.Ada_GUI.Render also requires GNAT. Other output formats have been demonstrated with both GNAT and ObjectAda compilers.

BMP files are written using [Image_IO](https://github.com/jrcarter/Image_IO).

MSI codes (also called MSI Plessey or Modified Plessey) use a Luhn checksum digit, generated using [PragmARC.Luhn_Generation](https://github.com/jrcarter/PragmARC).

SVG output makes use of [PragmARC.Images](https://github.com/jrcarter/PragmARC).

Several demo programs are included:
* BCDAG is an Ada-GUI program to demonstrate drawing the 4 type of barcodes with Ada GUI
* BMPQR writes a QR code to a BMP file
  - bmpqr -h "Text to encode" qr_code.bmp
* PBM128 outputs a Code-128 barcode to standard output in PBM format. Redirection to a file produces a PBM file:
  - pbm128 "Text to encode" > code_128.pbm
* PBMMSI is similar for an MSI barcode
* PBMQR  is similar for a QR code
* SVG128, SVGQR, and SVGUPC are similar programs to produce SVG-format output


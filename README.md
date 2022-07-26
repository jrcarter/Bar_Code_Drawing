# Bar_Code_Drawing
Creation of bar codes

Bar_Code_Drawing is a library for creating bar codes, inspired by de Montmollin's [Ada Bar Codes](https://github.com/zertovitch/ada-bar-codes), but hopefully simpler and easier to use.

Currently, Code 128, MSI, and QR codes are implemented. An additional type of bar code may be implemented by adding a package to the Bar_Code_Drawing.What.* hierarchy with an appropriate Draw procedure.

Currently, drawing to an [Ada-GUI](https://github.com/jrcarter/Ada_GUI) Graphic_Area and creation of PBM and SVG files are implemented. An additional representation may be implemented by adding a package to the Bar_Code_Drawing.How.* hierarchy with an appropriate operation.

A PBM file can easily be converted to most other graphic file formats using existing tools, so support for other graphic file formats does not seem necessary.

An [Ada-GUI](https://github.com/jrcarter/Ada_GUI) program requires the GNAT compiler, so using Bar_Code_Drawing.How.Ada_GUI.Render also requires GNAT. PBM output has been demonstrated with both GNAT and ObjectAda compilers.

MSI codes (also called MSI Plessey or Modified Plessey) use a Luhn checksum digit, generated using [PragmARC.Luhn_Generation](https://github.com/jrcarter/PragmARC).

SVG output makes use of [PragmARC.Images](https://github.com/jrcarter/PragmARC).

Several demo programs are included:
* BCDAG is an Ada-GUI program to demonstrate drawing the 3 type of barcodes with Ada GUI
* PBM128 outputs a Code-128 barcode to standard output in PBM format. Redirection to a file produces a PBM file:
  - pbm128 "Text to encode" > code_128.pbm
* PBMMSI is similar for an MSI barcode
* PBMQR  is similar for a QR code
* SVG128 and SVGQR are similar programs to produce SVG-format output


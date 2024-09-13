# Bar_Code_Drawing
Creation of bar codes

Bar_Code_Drawing is a library for creating bar codes, inspired by de Montmollin's [Ada Bar Codes](https://github.com/zertovitch/ada-bar-codes), but hopefully simpler and easier to use.

Currently, Code 128, Data Matrix, MSI, QR, and UPC-A/EAN-13 codes are implemented. An additional type of bar code may be implemented by adding a package to the Bar_Code_Drawing.What.* hierarchy with an appropriate Draw procedure.

Currently, drawing to an [Ada-GUI](https://github.com/jrcarter/Ada_GUI) Graphic_Area, creating an Image_IO.Image_Data result, and creation of PBM and SVG files are implemented. An additional representation may be implemented by adding a package to the Bar_Code_Drawing.How.* hierarchy with an appropriate operation.

An Image_IO.Image_Data result can be written to a file in a number of formats using the Write_* procedures of Image_IO.

An [Ada-GUI](https://github.com/jrcarter/Ada_GUI) program requires the GNAT compiler, so using Bar_Code_Drawing.How.Ada_GUI.Render also requires GNAT. Other output formats have been demonstrated with both GNAT and ObjectAda compilers.

Image_IO.Image_Data results are created using [Image_IO](https://github.com/jrcarter/Image_IO).

MSI codes (also called MSI Plessey or Modified Plessey) use a Luhn checksum digit, generated using [PragmARC.Luhn_Generation](https://github.com/jrcarter/PragmARC).

SVG output makes use of [PragmARC.Images](https://github.com/jrcarter/PragmARC).

Several demo programs are included:
* BCDAG is an Ada-GUI program to demonstrate drawing Code 128, MSI, QR, and UPC-A/EAN-13 barcodes with Ada GUI
* PNGQR writes a QR code to a PNG file
  - bmpqr [level] "Text to encode" qr_code.png
  - level represents the QR ECL level: -l (low); -m (medium, default); -q (quartile); -h (high)
* BMPQR is similar for BMP output
* PNGDM writes a Data Matrix code to a PNG file
  - pngdm "Text to encode" dm_code.png
* PBM128 outputs a Code-128 barcode to standard output in PBM format. Redirection to a file produces a PBM file:
  - pbm128 "Text to encode" > code_128.pbm
* PBMMSI is similar for an MSI barcode
* PBMQR  is similar for a QR code
* SVG128, SVGQR, and SVGUPC are similar programs to produce SVG-format output


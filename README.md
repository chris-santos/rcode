
# Infrared data Multivariate Analysis in R - by Christiano dos Santos, PhD

these R scrips perform multiple machine learning classification & regression tasks in R, over Infrared data obtained via DFT calculation with ORCA software.

    Data is obtained from ORCA as txt files. those txt files were processed with my PRISMA software (A PyQt python GUI ). 
    PRISMA parses the files, extracting and converting frequencies & intensities to the common IR graphics in format transmitance x wavenumber, throught convolution.

The new csv files are thus generated with the IR normalized data.
the many samples csv are consolidated in one single file, which is imported in R script and proceed with machine learning Analysis.


## Referência

 - [PRISMA SOFTWARE](https://github.com/chris-santos/Prisma_DFT)
 
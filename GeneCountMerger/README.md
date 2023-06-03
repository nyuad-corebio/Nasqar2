# GeneCountMerger

- This is a simple **preprocessing** tool to merge individual **gene count files** (Eg. output count files from htseq)
- This tool is a part of the NASQAR toolbox
- Pre-print: [NASQAR: A web-based platform for High-throughput sequencing data analysis and visualization](https://doi.org/10.1101/709980)

**NOTE:** first column must contain the genes. If the gene columns do not match in all files, this tool will not work



* * *

#### Online/Demo:
You can try it online at http://nasqar.abudhabi.nyu.edu/GeneCountMerger

#### **Features**

*   Merge individual sample count files. See **Sample Input Files** below for more details
*   Or merge **multiple matrices**
*   **Convert ensembl gene IDs to gene names**
    *   Option to choose from available genome/versions
    *   If genome/version is not available in the options and you have a [.gtf](https://asia.ensembl.org/info/website/upload/gff.html) file for your genome <a href="">follow these instructions.</a>
*   Option to add **pseudocounts (+1)**
*   **Download** merged counts file in .csv format
*   **Transcriptome Analysis (Optional)** after merging your counts:
    *   Use our **Seurat Wizard** to carry out single-cell RNA analysis
    *   Use **DESeq2** or **START** apps to carry out bulk RNA analysis

#### **Screenshot**
![alt text](mergeScreenshot.png "Input Data")


#### **Download gene_names**
Once clone GeneCountMerger download the gene_names folder from google drive using wget command from terminal to the root folder.
GeneCountMerger

- wget
wget --no-check-certificate 'https://drive.google.com/u/1/uc?id=14X25ijK0EVPbNJHSVVBWy-xLr0FRhy9j&export=download' -O gene_names.zip

- unzip 
  unzip gene_names.zip


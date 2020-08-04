# split-text

This project is designed to take a manuscript that contains multiple versions of the same text and split
them, so that the manuscripts can be reconstructed in different ways containing the individual parts.

The text typically contain three sections:

english: the text from which the translation is performed
target: the text in the target language
back: this is a translation back from the target text into english.

To distinguish back from the original text the back translation is a tansliteration in that it is word for word what the target 
language says.  It also contains '/' to indicate breaks in the text.

# The Process

## Preserving
The original manuscript is placed in a directory and is then copied into a working directory.  Ideally the original is mared as read only.

The file is put into the 'original' directory.
The copy is placed in the directory called pre-processing.

## Cleaning

Before the main part of the cleansing can take place there may be a need to split the .doc file in to three parts:

`heading, main part and footer.`

The reason for this is that the heading and the footer pages are often in a format that does not conform to the processing of the rest of the document.  By sperating them into different files, thetext can be processed and then the document recontractued using a manually tidied heading and footer page when text has bee processed.

The copy of the original file is manually cleaned to mark headings etc as the appropriate styles.  This will help with the later
conversion to markdown format that will be consumed by the program.

There are several styles that must be used to help with the conversion:

### Heading1
Used for Book Titles in all three texts

### Heading2
Used for Chapter headings

### Heading 3
Used for explanatory headings within a chapter

### Main text
The main text needs to be checked that each verse in each form of text begins with a number of the verse.


## Producing docx files
Most of the original files use the .doc extension from word.  This is not a format that many tools can work with.  A script called `doc2docx.bat` (windows only) is availabe for this conversion.  This requires that Microsoft office is installed on the machine.

## Producing markdown files
Markdown files are great as a format to process text.  It is a simple addition of marks in a document that allow the defiitions of headings etc with ease.  The program consumes markdown files of the cleansed text and produced the required output.  A scripts called `docx2md.bat` (windows only) is used to handle this process.  This script uses the pandoc program to achieve the best conversion.

# Directory
For each document that is being processed there is an expected directory structure.  It is as follows:

1. `original`: this contains the original document.  It is advised that this directory is marked as read-ony after the original is placed in it.
1. `pre-processing`: this contains the base `.doc` file that will have been cleaned to prepare it for processing
1. `_intermediate`: this contains the intermediate files that are created by the process.  This will include the `.docx` and `.md` files created by the initial scripts.
1. `pre-pubished`: this contains the output of the processing.  This could include `.html` and `.pdf `files depending upon the output format.
1. `published`:  after processing an output file is only put in the `published` directory if it is ready for public consumption.  This normally happens only after visual and translations checks have been performed on the files in the `pre-published` folder.



# Processing the files
Once in markdown format the processing of the files can take place.

The `.docx` and `.md` files are put in the _intermediate directory. 

The user has the following options available to them:

```asciidoc
Format documents

Usage: program-name [options] style

Options:
  -f, --file FILENAME              Filename without path or extension
  -d, --directory DIRECTORY        Directory that contains the working folders
  -t, --title TITLE                The Title of the document
  -x, --docx                       convert to docx
  -m, --markdown                   convert to markdown
  -o, --output OUTPUT        html  Output format - options html or pdf
  -h, --help

Style:
  bo         Print tibetan - will include english titles
  eng        Print english text
  back       Print back translation - will include english titles and headers
  boeng      Print tibetan and english interleaved
  boeng-col  Print tibetan and english in columns
  boback     Print tibetan and back translation interleaved
  all        Print full document - all languages

```

So assuming we are working with the book of James in a directory called `2019-A2A-11-Epistles-only-James` with the `.doc` file being called `james-final.doc`, then the following options are available

    `repl> (-main "-d" "full-path/2019-A2A-11-Epistles-only-James" "-f" "james-final" "-x" "-m" "-o" "html" "bo")`

The above command line in the clojure repl will take the base text file in the `pre-processing` directory and will out put an `.html` file in the `pre-published` directory with just the translated text.

After this initial call has been made if the base file is not changed the `-x` and `-m` options can be dropped as the `.md` file will be present in the `_intermediate` directory and any further processing can take place using this file as source.

    `repl> (-main "-d" "full-path/2019-A2A-11-Epistles-only-James" "-f" "james-final"  "-o" "html" "boeng")`

The above command will take the pre-existing markdown file called `james-final.md` in the `_intermediate` directory and will create a file called `james-final.html` in the `pre-published` directory.  The file will contain both the english and the translated text interleaved.  Each verse will be on a new line.  Both English and translated headers will be included.


#!/bin/bash

latex <file>.tex
dvips <file>.dvi
ps2pdf <file>.ps
pdf2eps <pagenumber> <file> 

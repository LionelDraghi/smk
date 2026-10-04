
# Directory update tests



##  Directory update tests / start conversion


  Run:  
  `rm default.smk`  
  `smk -q reset`  
  `smk run ./ogg-to-mp3.sh`  

  Expected:  
```  
./ogg-to-mp3.sh

```  

  Run:  
  `smk st -l`  

  Expected:  
```  
Command "./ogg-to-mp3.sh", last run [YYYY:MM:DD HH:MM:SS.SS]
  Sources: (5)
  - [If update  ] [Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] ogg-to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] to-mp3.sh
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.ogg
  - [If update  ] [Fil] [Normal] [Source] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.ogg
  Targets: (2)
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
  - [If absence ] [Fil] [Normal] [Target] [Identic] [YYYY:MM:DD HH:MM:SS.SS] y.mp3

```  


Directory update tests / start conversion [Successful](tests_status.md#successful)

##  Directory update tests / new ogg in dir


  Run:  
  `cp x.ogg z.ogg`  
  `smk whatsnew`  

  Expected:  
```  
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests
[Missing] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/x.mp3
[Updated] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/y.ogg
[Created] /home/lionel/Proj/smk/tests/12_mp3_conversions_tests/z.ogg
```  


Directory update tests / new ogg in dir [Successful](tests_status.md#successful)

##  Directory update tests / ogg-to-mp3 is modified


  Run:  
  `smk -q run ./ogg-to-mp3.sh`  
  `touch ogg-to-mp3`  
  `smk -e`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / ogg-to-mp3 is modified [Successful](tests_status.md#successful)

##  Directory update tests / adding a .ogg file in a subdir


  Run:  
  `mkdir dir1`  
  `smk wn -l`  

  Expected:  
```  
[Dir] [Normal] [Source] [Updated] [YYYY:MM:DD HH:MM:SS.SS] ./
```  

  Run:  
  `smk -e run ./ogg-to-mp3.sh`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  

  Run:  
  `cp x.ogg dir1/t.ogg`  
  `smk -e run ./ogg-to-mp3.sh`  

  Expected:  
```  
run "./ogg-to-mp3.sh" because Source dir ./ has been updated (YYYY:MM:DD HH:MM:SS.SS)
./ogg-to-mp3.sh
```  


Directory update tests / adding a .ogg file in a subdir [Successful](tests_status.md#successful)

##  Directory update tests / smk clean


  Run:  
  `smk clean`  

  Expected:  
```  
Deleting file dir1/t.mp3
Deleting file x.mp3
Deleting file y.mp3
Deleting file z.mp3
```  


Directory update tests / smk clean [Successful](tests_status.md#successful)


# Tutorial


 This test ensure that the current version  
 of smk behave as described in the tutorial  

##  Tutorial / start conversion


  This is the "Quick Start" part of the tutorial  

  Run:  
  # converting ogg to mp3:  
  `sox x.ogg x.mp3`  

  Expected:  
```  
run "sox x.ogg x.mp3" because it was not run before
sox x.ogg x.mp3
```  

  # setting Artist and Tittle tags:  
  `id3v2 -a Luke -t Sentinelle x.mp3`  

  Expected:  
```  
run "id3v2 -a Luke -t Sentinelle x.mp3" because it was not run before
id3v2 -a Luke -t Sentinelle x.mp3
```  

  # renaming according to tags:  
  `id3ren -quiet -template=%a  

  Expected:  
```  
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because it was not run before
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / start conversion [Successful](tests_status.md#successful)

##  Tutorial / second run


  Run:  
  `smk`  

  Expected: nothing, situation is up to date  
```  
Nothing to run
```  


Tutorial / second run [Successful](tests_status.md#successful)

##  Tutorial / smk do not rebuild if a target is missing!!!

  Run:  
  `rm Luke-Sentinelle.mp3`  
  `smk`  

  Expected:  
```  
Nothing to run
```  


Tutorial / smk do not rebuild if a target is missing!!! [Successful](tests_status.md#successful)

##  Tutorial / unless using the `-mt` / `--build-missing-target` option

  Run:  
  `smk -mt -e`  

  Expected:  
```  
run "sox x.ogg x.mp3" because Target file x.mp3 is missing
sox x.ogg x.mp3
run "id3v2 -a Luke -t Sentinelle x.mp3" because Source file x.mp3 has been updated (YYYY:MM:DD HH:MM:SS.SS)
id3v2 -a Luke -t Sentinelle x.mp3
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because Target file Luke-Sentinelle.mp3 is missing
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / unless using the `-mt` / `--build-missing-target` option [Successful](tests_status.md#successful)

##  Tutorial / touch x.ogg

  Run:  
  `touch x.ogg`  
  `rm Luke-Sentinelle.mp3`  
  `smk`  

  Expected:  
```  
run "sox x.ogg x.mp3" because Source file x.ogg has been updated (YYYY:MM:DD HH:MM:SS.SS)
sox x.ogg x.mp3
run "id3v2 -a Luke -t Sentinelle x.mp3" because Source file x.mp3 has been updated (YYYY:MM:DD HH:MM:SS.SS)
id3v2 -a Luke -t Sentinelle x.mp3
run "id3ren -quiet -template=%a-%s.mp3 x.mp3" because Source file x.mp3 is present
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


##  Tutorial / smk do rebuild if you give the target

  Run:  
  `smk lt -l > out.20`  
  `rm Luke-Sentinelle.mp3`  
  `smk Luke-Sentinelle.mp3`  

  Expected:  
```  
"id3ren -quiet -template=%a-%s.mp3 x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] Luke-Sentinelle.mp3
"id3v2 -a Luke -t Sentinelle x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
"sox x.ogg x.mp3" [] [If absence ] [Fil] [Normal] [Target] [New    ] [YYYY:MM:DD HH:MM:SS.SS] x.mp3
id3ren -quiet -template=%a-%s.mp3 x.mp3
```  


Tutorial / smk do rebuild if you give the target [Successful](tests_status.md#successful)

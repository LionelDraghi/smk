sox x.ogg x.mp3
id3v2 -a Luke -t Sentinelle x.mp3
id3ren -quiet -template=%a-%s.mp3 x.mp3

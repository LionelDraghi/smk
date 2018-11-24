# Write

- write - Écrire dans un descripteur de fichier  

- open, openat : si O_WRONLY, or O_RDWR
  (The argument flags must include one of the following access modes: O_RDONLY, O_WRONLY, or O_RDWR).
  These request opening the file read-only, write-only, or read/write, respectively.

- creat() : A call to creat() is equivalent to calling open() with flags equal to O_CREAT|O_WRONLY|O_TRUNC

- chown, fchown, lchown

- chmod, fchmod

- link - Crée un nouveau nom pour un fichier  

- unlink - Détruire un nom et éventuellement le fichier associé 

- fopen, fdopen, freopen - Fonctions d'ouverture de flux, sauf si mode = R ou R+   
  L'argument mode pointe vers une chaîne commençant par l'une des séquences suivantes (d'autres caractères peuvent suivre la séquence) :

  * r : Ouvre le fichier en lecture. Le pointeur de flux est placé au début du fichier.

  * r+ : Ouvre le fichier en lecture et écriture. Le pointeur de flux est placé au début du fichier.

  * w : Tronque le fichier à son début ou ouvre le fichier en écriture. Le pointeur de flux est placé au début du fichier.

  * w+ : Ouvre le fichier en lecture et écriture. Le fichier est créé s'il n'existait pas. S'il existait déjà, sa longueur est ramenée à 0. Le pointeur de flux est placé au début du fichier.

  * a : Ouvre le fichier en ajout (écriture à la fin du fichier). Le fichier est créé s'il n'existait pas. Le pointeur de flux est placé à la fin du fichier.

  * a+ : Ouvre le fichier en lecture et ajout (écriture en fin de fichier)Le fichier est créé s'il n'existait pas. La tête de lecture initiale du fichier est placée au début du fichier mais la sortie est toujours ajoutée à la fin du fichier.

- remove - Détruire un nom et éventuellement le fichier correspondant  (appelle unlink ou rmdir suivant le fichier)

- rename - Changer le nom ou l'emplacement d'un fichier  

 

#   Read

- open, openat : si O_RDONLY 

- stat, fstat, lstat : Obtenir l'état d'un fichier (file status)

- fopen, fdopen, freopen si mode = R ou R+   

 

# Write or Read?

- dup() et dup2() créent une copie du descripteur de fichier oldfd.

- mmap, munmap - Établir/supprimer une projection en mémoire (map/unmap) des fichiers ou des périphériques  

- fopen, fdopen, freopen - Fonctions d'ouverture de flux  

 

# Ignorés

- close, fcntl, lseek, 

- mknod - Créer un nœud du système de fichiers  (NB : sous Linux mknod() ne peut pas être utilisé pour créer des répertoires)

- mount, umount, umount2 - Monter/démonter des systèmes de fichiers

- umask() fixe le masque de création de fichiers du processus appelant, utilisé entre autre par open(2), mkdir(2

- fsync, fdatasync - Synchroniser un fichier en mémoire avec le disque  (flush)

 

 
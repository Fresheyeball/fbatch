![build](https://travis-ci.org/Fresheyeball/fbatch.svg?branch=master)

This is a simple tool for batch file interactions. Right now it only does some basic renaming. Lets say you have a directory structure like this:

```
dir
  annoying-subdir
    x.doc
  annoying-file1.mp3
  annoying-file1.mp3
  justfine.mp3
```

you could run

```
fbatch annoying A dir
```

they result would be

```
dir
  A-subdir
    x.doc
  A-file1.mp3
  A-file1.mp3
  justfine.mp3
```
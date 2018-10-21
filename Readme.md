# Delphi VMTools

## Features
- Fast indexed search using regular expressions based on [Google Code Search](https://github.com/google/codesearch)
- Open file wizard to open files from project or project group search path.
- Improved browsing history by `Alt + Left Arrow`/`Alt + Right Arrow`
- Activate editor by shortcut.
- Dockable forms.

### Default shortcuts
- Open search dialog: `Alt + Shift + D`
- Focus search results dialog: `Alt + Shift + D`
- Next search match: `Ctrl + Alt + Shift + Down Arrow`
- Previous search match: `Ctrl + Alt + Shift + Up Arrow`
- Open file: `Alt + Shift + O`
- Activate editor: `Alt + E`

Shortcuts can be changed using options page. Options integrated into standard IDE options dialog.

*Note: if shortcut already in use by another wizard it cannot be assigned.*

## Build
#### Generate .dproj file
Dproj file generated using python 3.6 [pynt](https://github.com/rags/pynt)

- Install `pynt` with necessary dependencies: `pip install pynt bunch lxml`
- Create `pynt.local` file inside `prj` directory with content:
###### Delphi XE
```
    [common]
    out=<PATH TO FOLDER WHERE SHOULD PLACE COMPILED BINNARIES>
    dcu=<PATH TO FOLDER WHERE SHOULD PLACE DCU FILES>
    DelphiVersion=bds8.0
```    
###### Delphi 10.1 Berlin
```
    [common]
    out=<PATH TO FOLDER WHERE SHOULD PLACE COMPILED BINNARIES>
    dcu=<PATH TO FOLDER WHERE SHOULD PLACE DCU FILES>
    DelphiVersion=bds18.0
```    
*Note: you should specify valid path.*
- Go into vmtools project folder: `cd prj\vmrools`
- Generate .dproj file using `pynt`:
  - For Delphi xe run: `pynt -f ..\build.py delphixe`
  - For Delphi 10.1 Berlin run: `pynt -f ..\build.py delphi10`
- Open IDE and build project.

## Installation
To install VMTools you should register compiled dll into IDE using registry.
You can use Expert Manager from CnPack.

### Manual instalation
- Create new string key with path to compiled dll:
  - `HKEY_CURRENT_USER\Software\Embarcadero\BDS\18.0\Experts` for Delphi 10.1 Berlin
  - `HKEY_CURRENT_USER\Software\Embarcadero\BDS\8.0\Experts` for Delphi XE.

- Place `csearch.exe` and `cindex.exe` from `bin` directory somewhere into your `%PATH%`.

## Before usage
VMTools internally uses `csearch` for perform search.  
Before you can search anything you should build index file for `csearch`.

### Generating index file
Index file generated using `cindex.exe`.  
Index file should have name `codesearch.index` and searched started from project folder or currently opened file folder and go up through directory tree while not found.  
Go into directory which you want to index and run command:

`cindex -maxtrigrams 1000000 -indexpath .\codesearch.index .\`

You can exclude some files from indexing by using `-exclude` option of `cindex`.  
Create new file, e.g. `codesearch.exclude` containing file masks to exclude and run command:

`cindex -maxtrigrams 1000000 -indexpath .\codesearch.index -exclude .\codesearch.exclude .\`

 **Sample codesearch.exclude**

```
*.dproj
*.bdsproj
*.stat
*.dsk
*.log
*.~*
*.res
*.py
*.patch
*.sql
*.psk
*.pbk
*.bak
```

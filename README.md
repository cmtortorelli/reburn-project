# reburn-project
## Data management approach for this repo
This repository contains only code files and supporting files. It does not contain data. The data folder for this repository is located on Box: https://ucdavis.app.box.com/folder/165902052978

The scripts in this repo are set up so that all references to the data directory are relative, except for one file: data_dir.txt. You need to change the contents of this file so that it lists the root location of the data directory on your machine. If the data_dir.txt file does not exist, you need to create it (git ignores it as specified in the .gitignore file). The data_dir.txt file belongs in the root directory of the repo.

The scripts in this repo source convenience_functions.R, which defines a function datadir(). If you pass this function a path to a file within the data directory, it prepends the path with the full path to the data directory. 

To work with this data directory on your local machine, you can either download it from Box manually, or use Box Drive to have the folder sync to your computer. The latter approach is recommended so that you automatically get any changes that others make to the data files, and so that your changes automatically update on Box for everyone else who is working with the data.

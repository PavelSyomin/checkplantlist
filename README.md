# Checkplantlist is a web application that checks names of plant species using ThePlantList database
Files of [checkplantlst.online](https://checkplantlist.online) web app
- App.R — code.
- Sample.rds — static asset called from code.
- test_data.txt — a list of species which may be used to test the application.

## What is this application for
The application checks if the names of the species of plants are accepted, synonyms, unresolved or misapplied according to [ThePlantList](http://theplantlist.org) database. It is built on the top of [plantlist](https://github.com/helixcn/plantlist/) package and uses an offline copy of ThePlantList database.

## How application works
1. Paste list of species' names into a textarea.
2. Press «Проверить» button.
3. Wait. The progress bar in the bottom right corner shows the status of the process.
4. (Optionally) Download results in CSV or XLSX using buttons above the resulting table.

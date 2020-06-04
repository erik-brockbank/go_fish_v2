# go_fish v2

## TODO
- Add new evaluation rules, incl. high-prior "generalizable" rule
- Add completely novel shapes to generation 2AFC task (?)
- Add additional prediction to each trial (?)

## Changes from v1
- Modified the hypothesis evaluation Likert scales to be in range ("Not good", "Very good") with no visible discrete tick marks, underlying scores in (1, 100)
- Modified data writing of hypothesis evaluation to include `category` var for each rule (values include "target", "distractor", "misc") and `evidence` var for each rule noting what percent of previous evidence the rule is consistent with
- Removed hypothesis generation free response task

## To run experiment locallly:
1. cd to `go_fish_v2` directory
2. `python -m http.server {port}` (or your favorite local web server)
3. In browser, navigate to:
- `http://localhost:{port}/go_fish.html` for experiment version
- `http://localhost:{port}/go_fish.html?&mode=test` for test version (writes a file prepended with `TEST_...` for easy debugging)

## Data for participants is in the /data folder (no identifying information was captured)


## Analysis of existing data is in the /analysis folder



# go_fish v2

## Changes from v1
- Added two new "abstract" hypotheses for evaluation component and shuffled order of hypotheses during this task
- Small changes to previous hypotheses in evaluation, including larger range of correctness for non-target (and non-abstract) hypotheses
- Modified the hypothesis evaluation Likert scales to be in range ("Not good", "Very good") with no visible discrete tick marks, underlying scores in (1, 100)
- Participants prevented from clicking through task without clicking on slider range on pages with Likert scales
- Modified data writing of hypothesis evaluation to include `category` var for each rule (values include "target", "distractor", "misc") `evidence` var for each rule noting what percent of previous evidence the rule is consistent with, and `id` to uniquely identify each rule
- Removed hypothesis generation free response task and classification task
- Small change to star shape makes it more visibly distinct from diamond


## To run experiment locallly:
1. cd to `go_fish_v2` directory
2. set `PROD` global in `constants.js` to be either "test" or "prod"
3. `python -m http.server {port}` (or your favorite local web server)
4. In browser, navigate to:
- `http://localhost:{port}/go_fish_v2.html` for experiment version
- `http://localhost:{port}/go_fish_v2.html?&mode=test` for test version (writes a file prepended with `TEST_...` for easy debugging)

## Data for participants is in the /data folder (no identifying information was captured)


## Analysis of existing data is in the /analysis folder



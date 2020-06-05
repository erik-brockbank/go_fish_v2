# go_fish v2

## Changes from v1
- Modified the hypothesis evaluation Likert scales to be in range ("Not good", "Very good") with no visible discrete tick marks, underlying scores in (1, 100)
- Modified data writing of hypothesis evaluation to include `category` var for each rule (values include "target", "distractor", "misc") and `evidence` var for each rule noting what percent of previous evidence the rule is consistent with
- Removed hypothesis generation free response task
- Multiple changes to hypotheses being evaluated, including new abstract hypotheses, revised order placing distractor before target, and larger scale of correctness for non-target (and non-abstract) hypotheses

## To run experiment locallly:
1. cd to `go_fish_v2` directory
2. set `PROD` global in `constants.js` to be either "test" or "prod"
3. `python -m http.server {port}` (or your favorite local web server)
4. In browser, navigate to:
- `http://localhost:{port}/go_fish_v2.html` for experiment version
- `http://localhost:{port}/go_fish_v2.html?&mode=test` for test version (writes a file prepended with `TEST_...` for easy debugging)

## Data for participants is in the /data folder (no identifying information was captured)


## Analysis of existing data is in the /analysis folder



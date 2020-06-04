/*
 * Startup library for loading in html content
 * Instructions for startup:
 * 1. navigate to go_fish directory
 * 2. python -m http.server {port}
 * 3. navigate to http://localhost:{port}/go_fish.html for experiment version
 *      http://localhost:{port}/go_fish.html?&mode=test for test version
 * 4. Don't forget: insure that `PROD` global below is set to "test" (set to "prod") on server
 */

PROD = "test"; // set to "test" if running locally, "prod" otherwise


$(window).ready(function() {
    $("body").load(HTML_LOOKUP[PROD]["consent"], function() {
        $("#consent-button").click(loadExperiment);
    });
});

loadExperiment = function() {
    console.log("Consent form agree.");
    // URL parsing
    // ex. http://localhost:8000/index.html?&mode=test
    var istest = false;
    var urlParams = new URLSearchParams(window.location.href);
    if (urlParams.has("mode") && urlParams.get("mode").includes("test")) {istest = true;}

    // Set experiment condition
    // var control = true; // DEBUGGING
    var control = Math.random() < 0.5;


    console.log("Starting experiment with test = ", istest, "; control = ", control);
    var exp = new Experiment(istest, control);
    // Initialize experiment with global variables used throughout experiment logic
    // NB: this is cumbersome but means we avoid referencing these as global constants inside the experiment logic
    exp.initialize(htmlpath = HTML_LOOKUP[PROD]["experiment"],
                    inst_htmlpath = HTML_LOOKUP[PROD]["instructions"],
                    evidence_htmlpath = HTML_LOOKUP[PROD]["evidence"],
                    evidence_resp_htmlpath = HTML_LOOKUP[PROD]["evidence_resp"],
                    prediction_htmlpath = HTML_LOOKUP[PROD]["prediction"],
                    generate_htmlpath = HTML_LOOKUP[PROD]["generate"],
                    judgment_htmlpath = HTML_LOOKUP[PROD]["judgment"],
                    eval_htmlpath = HTML_LOOKUP[PROD]["evaluation"],
                    memory_htmlpath = HTML_LOOKUP[PROD]["memory"],
                    fish_img_path = IMAGE_LOOKUP[PROD]["fish_img"],
                    no_fish_img_path = IMAGE_LOOKUP[PROD]["no_fish_img"],
                    fish_img_path_small = IMAGE_LOOKUP[PROD]["fish_checkbox"],
                    no_fish_img_path_small = IMAGE_LOOKUP[PROD]["no_fish_checkbox"],
                    instruction_array = INSTRUCTION_ARRAY,
                    trial_array = TRIAL_ARRAY,
                    judgment_array = JUDGMENT_ARRAY,
                    eval_array = EVAL_ARRAY,
                    memory_array = MEMORY_ARRAY,
                    fish_caught_msg = OUTCOME_POSITIVE,
                    no_fish_msg = OUTCOME_NEGATIVE,
                    fish_explain_msg = EXPLAIN_RESPONSE_POS,
                    no_fish_explain_msg = EXPLAIN_RESPONSE_NEG,
                    fish_describe_msg = CONTROL_RESPONSE_POS,
                    no_fish_describe_msg = CONTROL_RESPONSE_NEG,
                    end_game_msg = ENDGAME_MSG);
    // Run experiment!
    exp.run();
};

/*
 * Experiment constants library
 */

const EXPT_NAME = "go_fish";
const EXPT_VERSION = 2;
const PROD = "test"; // set to "test" if running locally, "prod" otherwise

/*
 * HTML_LOOKUP is a lookup dictionary with paths to the html files loaded dynamically during the experiment
 */
const HTML_LOOKUP = { // lookup table with human-understandable html file keys and the path to those files as vals
    // Prod pathways //
    "prod": {
        "consent": "/go_fish_v2/static/html/consent.html",
        "instructions": "/go_fish_v2/static/html/inst.html",
        "experiment": "/go_fish_v2/static/html/exp.html",
        "evidence": "/go_fish_v2/static/html/evidence.html",
        "evidence_resp": "/go_fish_v2/static/html/evidence_response.html",
        "prediction": "/go_fish_v2/static/html/prediction.html",
        "evaluation": "/go_fish_v2/static/html/evaluate.html",
        "memory": "/go_fish_v2/static/html/memory.html"
    },
    // Local pathways //
    "test": {
        "consent": "/static/html/consent.html",
        "instructions": "/static/html/inst.html",
        "experiment": "/static/html/exp.html",
        "evidence": "/static/html/evidence.html",
        "evidence_resp": "/static/html/evidence_response.html",
        "prediction": "/static/html/prediction.html",
        "evaluation": "/static/html/evaluate.html",
        "memory": "/static/html/memory.html"

    }
};

const WRITE_ENDPOINT = "submit.php"; // endpoint for writing json results


/*
 * INSTRUCTION CONSTANTS
 */

/*
 * INSTRUCTION_ARRAY is a list with identical dictionary elements used to display instructions.
 * Each element contains:
 *  "top_text": text to display in top instruction text bucket
 *  "canvas_img": path to image to display in center instruction canvas
 *  "bottom_text": text to display in bottom instruction text bucket
 */
const INSTRUCTION_ARRAY = [
    {
        top_text: "In this experiment, imagine you have a friend who loves fishing in a nearby lake. " +
                    "Your friend is trying to figure out what kinds of fishing lures are best for catching the fish in the lake. ",
        canvas_img: "img/inst-lure.png",
        bottom_text: "A fishing lure is something you put on the end of a fish hook in order to attract fish underwater. "
    },
    {
        top_text: "There’s only one kind of fish in the lake your friend fishes at. ",
        canvas_img: "img/inst-fish.png",
        bottom_text: "The fish in the lake always bite certain lure combinations and never bite others. "
    },
    {
        top_text: "You’re going to see a series of fishing lure combinations that your friend uses to try and catch fish. Here's an example: ",
        canvas_img: "img/inst-lure_combo.png",
        bottom_text: ""
    },
    {
        top_text: "Some lure combinations catch fish, and others don’t: ",
        canvas_img: "img/inst-lure_catch.png",
        bottom_text: ""
    },
    {
        top_text: "Throughout the experiment, you will be asked to predict whether different lure " +
            "combinations are likely to catch fish.",
        canvas_img: "",
        bottom_text: ""
    },
    {
        top_text: "Let's get started!",
        canvas_img: "",
        bottom_text: ""
    }
];


/*
 * EXPERIMENT CONSTANTS
 */


const IMAGE_LOOKUP = {
    // Prod pathways //
    "prod": {
        "fish_img": "/go_fish/img/fish_icon.png",
        "no_fish_img": "/go_fish/img/fish_icon-no_fish.png",
        "fish_checkbox": "/go_fish/img/fish_icon-checkbox.png",
        "no_fish_checkbox": "/go_fish/img/fish_icon-no_fish-checkbox.png"
    },
    // Local pathways //
    "test": {
        "fish_img": "/img/fish_icon.png",
        "no_fish_img": "/img/fish_icon-no_fish.png",
        "fish_checkbox": "/img/fish_icon-checkbox.png",
        "no_fish_checkbox": "/img/fish_icon-no_fish-checkbox.png"
    }
};

/*
 * Message constants. These are messages displayed to the user throughout the experiment,
 * stored here for easier editing
 */
const OBSERVATIONS = "Observed lure combinations";
const OUTCOME_POSITIVE = "With these lures, she caught a fish!";
const OUTCOME_NEGATIVE = "With these lures, she didn't catch any fish.";
const CONTROL_RESPONSE_POS = "In the space below, describe this lure combination that your friend caught a fish with.";
const CONTROL_RESPONSE_NEG = "In the space below, describe this lure combination that your friend didn’t catch a fish with.";
const EXPLAIN_RESPONSE_POS = "In the space below, explain why your friend might have caught a fish with this lure combination.";
const EXPLAIN_RESPONSE_NEG = "In the space below, explain why your friend might not have caught any fish with this lure combination.";
const ENDGAME_MSG = "All done! Thanks for playing!";


/*
 * TRIAL_ARRAY is a list of dictionary objects containing information to be filled in when
 * displaying each trial during the experiment.
 * Each element contains:
 *  evidence: a dictionary object dictating what sort of lure combination is shown as evidence in this trial
 *  outcome: (bool) variable indicating whether this lure combination caught a fish
 *  prediction: a dictionary object of the same sort as `evidence` indicating what sort of lure combination
 *      should be used for the prediction on this trial
 *  prediction_outcome: whether the prediction shape will catch fish
 */
const TRIAL_ARRAY = [
    {
        evidence: {
            top_shape: "teardrop",
            top_color: "red",
            top_texture: true,
            bottom_shape: "triangle",
            bottom_color: "yellow",
            bottom_texture: false
        },
        outcome: 1,
        prediction: { // same top shape
            top_shape: "teardrop",
            top_color: "red",
            top_texture: true,
            bottom_shape: "diamond",
            bottom_color: "green",
            bottom_texture: false
        },
        prediction_outcome: 1
    },
    {
        evidence: {
            top_shape: "circle",
            top_color: "green",
            top_texture: false,
            bottom_shape: "teardrop",
            bottom_color: "blue",
            bottom_texture: false
        },
        outcome: 0,
        prediction: { // same top shape
            top_shape: "circle",
            top_color: "green",
            top_texture: false,
            bottom_shape: "triangle",
            bottom_color: "red",
            bottom_texture: false
        },
        prediction_outcome: 1
    },
    {
        evidence: {
            top_shape: "diamond",
            top_color: "yellow",
            top_texture: false,
            bottom_shape: "circle",
            bottom_color: "green",
            bottom_texture: true
        },
        outcome: 0,
        prediction: { // same bottom shape
            top_shape: "triangle",
            top_color: "yellow",
            top_texture: false,
            bottom_shape: "circle",
            bottom_color: "green",
            bottom_texture: true
        },
        prediction_outcome: 0
    },
    {
        evidence: {
            top_shape: "oval",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "star",
            bottom_color: "yellow",
            bottom_texture: true
        },
        outcome: 1,
        prediction: { // same top shape
            top_shape: "oval",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "teardrop",
            bottom_color: "red",
            bottom_texture: true
        },
        prediction_outcome: 0
    },
    {
        evidence: {
            top_shape: "circle",
            top_color: "red",
            top_texture: true,
            bottom_shape: "diamond",
            bottom_color: "green",
            bottom_texture: true
        },
        outcome: 1,
        prediction: { // same top shape
            top_shape: "circle",
            top_color: "red",
            top_texture: true,
            bottom_shape: "teardrop",
            bottom_color: "blue",
            bottom_texture: false
        },
        prediction_outcome: 0
    },
    {
        evidence: {
            top_shape: "star",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "circle",
            bottom_color: "red",
            bottom_texture: false
        },
        outcome: 0,
        prediction: { // same top shape
            top_shape: "star",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "star",
            bottom_color: "yellow",
            bottom_texture: true
        },
        prediction_outcome: 1
    },
    {
        evidence: {
            top_shape: "diamond",
            top_color: "yellow",
            top_texture: true,
            bottom_shape: "diamond",
            bottom_color: "blue",
            bottom_texture: false
        },
        outcome: 1,
        prediction: { // same bottom shape
            top_shape: "star",
            top_color: "green",
            top_texture: false,
            bottom_shape: "diamond",
            bottom_color: "blue",
            bottom_texture: false
        },
        prediction_outcome: 1
    },
    {
        evidence: {
            top_shape: "triangle",
            top_color: "green",
            top_texture: true,
            bottom_shape: "oval",
            bottom_color: "red",
            bottom_texture: true
        },
        outcome: 0,
        prediction: { // same bottom shape
            top_shape: "diamond",
            top_color: "yellow",
            top_texture: false,
            bottom_shape: "oval",
            bottom_color: "red",
            bottom_texture: true
        },
        prediction_outcome: 0
    }
];


/*
 * EVAL_ARRAY is a list of dictionary objects containing the rules to be evaluated sequentially
 * during the evaluation task of the experiment.
 * Each element contains:
 *  rule_text: the string containing the rule to be evaluated
 *  category: a string variable indicating what kind of rule this is
 *  id: an integer ID (1-8) for easier tracking of rules since they are shuffled at presentation
 *  evidence: proportion of the 8 evidence trials that this rule is consistent with
 */
const EVAL_ARRAY = [
    {
        rule_text: "If a lure combination has a blue shape, it will catch fish.",
        category: "misc",
        id: 1,
        evidence: 0.5 // 4/8 (2/4 confirm)
    },
    {
        rule_text: "If a lure combination has a yellow shape or a diamond on the bottom, it will catch fish.",
        category: "distractor",
        id: 2,
        evidence: 1.0 // 8/8
    },
    {
        rule_text: "If a lure combination has colors that are more visible under water (red, yellow), it will catch fish.",
        category: "abstract_color",
        id: 3,
        evidence: 0.628 // 5/8 (4/4 confirm)
    },
    {
        rule_text: "If a lure combination has a pointy shape on the bottom, it will catch fish.",
        category: "target",
        id: 4,
        evidence: 1.0 // 8/8
    },
    {
        rule_text: "If a lure combination has a star shape, it will catch fish.",
        category: "misc",
        id: 5,
        evidence: 0.5 // 4/8 (but only 1/4 confirm)
    },
    {
        rule_text: "If a lure combination has a yellow or red shape with a purple dot, it will catch fish.",
        category: "misc",
        id: 6,
        evidence: 0.875 // 7/8 (but 4/4 confirm)
    },
    {
        rule_text: "If a lure combination is shaped more like a fish, it will catch fish.",
        category: "abstract_shape",
        id: 7,
        evidence: 0.375 // 3/8 but this is a bit subjective. Thinking trials 1, 2, 4, 5 (round top shape) look like fish; 1, 4, 5 catch fish
    },
    {
        rule_text: "There is no pattern to which lure combinations catch fish: the results are " +
            "random, but there are approximately equal numbers that catch fish and don’t.",
        category: "rand",
        id: 8,
        evidence: 0.0 // could be 0 or 8/8
    }
];


/*
 * MEMORY_ARRAY is a list of dictionary objects containing the shapes to be used in the memory probe,
 * as well as whether they were in fact part of the experiment.
 * Each element contains:
 *  probe: a dictionary object dictating what sort of lure combination is shown for each problem in the memory task
 *  in_expt: (bool) variable indicating whether this lure combination was shown during the experiment (written as experiment data)
 *  catches_fish: (bool) variable indicating whether this lure combination would catch fish (not relevant but ensures we stay balanced)
 */
const MEMORY_ARRAY = [
    {
        probe: {
            top_shape: "teardrop",
            top_color: "yellow",
            top_texture: false,
            bottom_shape: "diamond",
            bottom_color: "green",
            bottom_texture: false
        },
        in_expt: 0,
        catches_fish: 1
    },
    { // Prediction lure #3
        probe: {
            top_shape: "triangle",
            top_color: "yellow",
            top_texture: false,
            bottom_shape: "circle",
            bottom_color: "green",
            bottom_texture: true
        },
        in_expt: 1,
        catches_fish: 0
    },
    {
        probe: {
            top_shape: "oval",
            top_color: "green",
            top_texture: false,
            bottom_shape: "diamond",
            bottom_color: "red",
            bottom_texture: true
        },
        in_expt: 0,
        catches_fish: 1
    },
    { // Evidence lure #1
        probe: {
            top_shape: "teardrop",
            top_color: "red",
            top_texture: true,
            bottom_shape: "triangle",
            bottom_color: "yellow",
            bottom_texture: false
        },
        in_expt: 1,
        catches_fish: 1
    },
    {
        probe: {
            top_shape: "diamond",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "circle",
            bottom_color: "blue",
            bottom_texture: false
        },
        in_expt: 0,
        catches_fish: 0
    },
    { // Evidence lure #6
        probe: {
            top_shape: "star",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "circle",
            bottom_color: "red",
            bottom_texture: false
        },
        in_expt: 1,
        catches_fish: 0
    },
    {
        probe: {
            top_shape: "circle",
            top_color: "yellow",
            top_texture: false,
            bottom_shape: "oval",
            bottom_color: "yellow",
            bottom_texture: false
        },
        in_expt: 0,
        catches_fish: 0
    },
    { // Prediction lure #6
        probe: {
            top_shape: "star",
            top_color: "blue",
            top_texture: false,
            bottom_shape: "star",
            bottom_color: "yellow",
            bottom_texture: true
        },
        in_expt: 1,
        catches_fish: 1
    }
];

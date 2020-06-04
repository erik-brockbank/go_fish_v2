
/*
 * TODO
 *
 * Cleanup
 * - Generate html dynamically, trim down current html stubs
 * - try to separate out html-referencing functions in this file (if it knows about e.g. an object's id, quarantine it)
 */





/*
 * Experiment class used to run core experiment logic. Note instantiating the class below does
 * very little initialization. Instead, the initialize() function handles the nitty gritty of
 * assigning various global states.
 */
Experiment = function(istest, control) {
    this.istest = istest; // `test` experiments simulate a real experiment but write results as TEST_{exptid}.json
    this.control = control; // `control` holds condition: TRUE if control, else FALSE
    var uid = new Date().getTime();
    if (istest) {this.exptid = "TEST_" + uid;}
    else {this.exptid = "user_" + uid;}
    this.data = {}; // object for storing data that we write to json at end of experiment
};


/*
 * Initialization function for Experiment class. To keep class instantiation clean, this function
 * does all the kitchen sink logic of assigning relevant html and image paths and other global state
 * that sub-functions below don't need to know about
 */
Experiment.prototype.initialize = function(htmlpath, inst_htmlpath, evidence_htmlpath, evidence_resp_htmlpath,
        prediction_htmlpath, generate_htmlpath, judgment_htmlpath, eval_htmlpath, memory_htmlpath,
        fish_img_path, no_fish_img_path, fish_img_path_small, no_fish_img_path_small,
        instruction_array, trial_array, judgment_array, eval_array, memory_array,
        fish_caught_msg, no_fish_msg, fish_explain_msg, no_fish_explain_msg,
        fish_describe_msg, no_fish_describe_msg, end_game_msg) {
    // file path variables
    this.htmlpath = htmlpath; // path to html file to load for experiment
    this.inst_htmlpath = inst_htmlpath; // path to html file to load for instructions
    this.evidence_htmlpath = evidence_htmlpath; // path to html file for displaying evidence
    this.evidence_resp_htmlpath = evidence_resp_htmlpath; // path to html file for responding to evidence
    this.prediction_htmlpath = prediction_htmlpath; // path to html file for collecting prediction data
    this.generate_htmlpath = generate_htmlpath; // path to html file for hypothesis generation task
    this.judgment_htmlpath = judgment_htmlpath; // path to html file for judgment task
    this.eval_htmlpath = eval_htmlpath; // path to html file for evaluation task
    this.memory_htmlpath = memory_htmlpath; // path to html file for memory task
    this.fish_img_path = fish_img_path; // path to image used when fish caught
    this.no_fish_img_path = no_fish_img_path; // path to image used when no fish caught
    this.fish_img_path_small = fish_img_path_small; // path to small image of fish caught used for checkboxes, etc.
    this.no_fish_img_path_small = no_fish_img_path_small; // path to small image of no fish used for checkboxes, etc.

    // global objects
    this.instruction_array = instruction_array; // array object with text and images used for instructions
    this.trial_array = trial_array; // array object with lures used for trials
    this.judgment_array = judgment_array; // array object with lures used for judgment task
    this.evalArray = eval_array; // array object with rules used in the evaluation task
    this.memory_array = memory_array; // array object with lures used in the memory task

    // global text variables
    // (wording of messages displayed throughout experiment that we save as constants rather than edit within the js below)
    this.fish_caught_msg = fish_caught_msg; // message displayed to users when a fish was caught
    this.no_fish_msg = no_fish_msg; // message displayed to users when no fish was caught
    this.fish_explain_msg = fish_explain_msg; // message displayed to users prompting them to explain why a fish was caught with a given lure
    this.no_fish_explain_msg = no_fish_explain_msg; // message displayed to users prompting them to explain why a fish was *not* caught with a given lure
    this.fish_describe_msg = fish_describe_msg; // message displayed to users prompting them to describe the lure when a fish was caught
    this.no_fish_describe_msg = no_fish_describe_msg; // message displayed to users prompting them to describe the lure when a fish was *not* caught
    this.end_game_msg = end_game_msg; // message displayed to users at the end of the game

    // global state variables
    this.trial_index = 0; // Index for keeping track of trial iterations
    this.eval_index = 0; // Index for keeping track of rule evaluation iterations
};


Experiment.prototype.run = function() {
    // Instantiate relevant fields in data object
    this.data["is_test"] = this.istest;
    this.data["is_control"] = this.control;
    this.data["expt_start_ts"] = new Date().getTime();
    this.data["instruction_data"] = {};
    // Run instructions
    var inst = new Instructions(this.inst_htmlpath, this.instruction_array);
    this.data["instruction_data"]["instruction_start_ts"] = new Date().getTime();
    inst.run(this.startTrials.bind(this));
};


Experiment.prototype.startTrials = function() {
    console.log("Starting experiment trials");
    // write to data object
    this.data["instruction_data"]["instruction_end_ts"] = new Date().getTime();
    this.data["trial_start_ts"] = new Date().getTime();
    this.data["trial_data"] = [];

    // Load html for running trials, fill in appropriate text
    var that = this;
    $("body").load(this.htmlpath, function() {
        that.showEvidence();
    });
};


Experiment.prototype.showEvidence = function() {
    console.log("Showing evidence for trial: ", this.trial_index + 1);
    var individ_trial_start_ts = new Date().getTime();
    // Process trial object for this evidence trial
    var trialObj = this.trial_array[this.trial_index];
    var outcomeText = "";
    var outcomeImg = "";
    if (trialObj.outcome == 1) {
        outcomeText = this.fish_caught_msg;
        outcomeImg = this.fish_img_path;
    } else if (trialObj.outcome == 0) {
        outcomeText = this.no_fish_msg;
        outcomeImg = this.no_fish_img_path;
    }
    var shapeInfo = trialObj.evidence;
    var evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
        shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);

    // Write to data object
    this.data["trial_data"].push({"trial_index": this.trial_index + 1});
    this.data["trial_data"][this.trial_index]["trial_n_start_ts"] = individ_trial_start_ts;
    this.data["trial_data"][this.trial_index]["evidence_shape"] = trialObj.evidence;
    this.data["trial_data"][this.trial_index]["evidence_catches_fish"] = trialObj.outcome;

    // Update html to display this evidence trial
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.evidence_htmlpath, function() {
        $("#evidence-outcome").text(outcomeText);
        $("#evidence-outcome-img-container").html("<img class='evidence-outcome-img' src='" + outcomeImg + "' />");
        evidenceShape.drawLure(canvasId = "evidence-shape-canvas", sizeConfig = "evidence"); // TODO store this ID somewhere sensible
        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            that.showEvidenceResponse();
        });
    });
};


Experiment.prototype.showEvidenceResponse = function() {
    console.log("Collecting evidence response for trial: ", this.trial_index + 1);
    // Instantiate relevant fields in data object
    this.data["trial_data"][this.trial_index]["input_evidence_response"] = "";
    this.data["trial_data"][this.trial_index]["evidence_resp_start_ts"] = new Date().getTime();

    // Process trial object for this evidence response trial
    var trialObj = this.trial_array[this.trial_index];
    var responseBanner = "";
    var evidenceOutcomeImg = "";
    if (trialObj.outcome == 1) {
        evidenceOutcomeImg = this.fish_img_path_small;
        if (this.control) {
            responseBanner = this.fish_describe_msg;
        } else {
            responseBanner = this.fish_explain_msg;
        }
    } else if (trialObj.outcome == 0) {
        evidenceOutcomeImg = this.no_fish_img_path_small;
        if (this.control) {
            responseBanner = this.no_fish_describe_msg;
        } else {
            responseBanner = this.no_fish_explain_msg;
        }
    }

    var shapeInfo = trialObj.evidence;
    var evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
        shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);

    // Update html for this response trial
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.evidence_resp_htmlpath, function() {
        $("#evidence-response-banner").text(responseBanner);
        $("#evidence-response-banner").css("font-style", "Italic");
        evidenceShape.drawLure(canvasId = "obs-item-canvas-" + (that.trial_index + 1), sizeConfig = "observations"); // TODO store this ID somewhere sensible
        $("#obs-outcome-" + (that.trial_index + 1)).html("<img class='obs-outcome-img' src='" + evidenceOutcomeImg + "' />");
        if (trialObj.outcome == 1) {
            $("#obs-item-" + (that.trial_index + 1)).css("border", "5px solid black"); // draw bold box around fish catches
            $("#obs-item-" + (that.trial_index + 1)).css("margin", "5px"); // decrease margin to keep everything lined up
        }

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // Process whether they wrote anything here and prevent from clicking if they didn't write anything
            var resp = $("#evidence-response").val();
            if (resp === "") {
                alert("Please respond to the prompt!");
            } else {
                // Add what they wrote to experiment trial object and timestamp end of writing segment
                that.data["trial_data"][that.trial_index]["input_evidence_response"] = resp;
                that.data["trial_data"][that.trial_index]["evidence_resp_end_ts"] = new Date().getTime();
                that.showPrediction();
            }
        });
    });
};


Experiment.prototype.showPrediction = function() {
    console.log("Collecting prediction for trial: ", this.trial_index + 1);
    var prediction_start_ts = new Date().getTime();
    // Process trial object for this prediction trial
    var trialObj = this.trial_array[this.trial_index];
    var shapeInfo = trialObj.prediction;
    var evidenceShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
        shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);

    // Write to data object
    this.data["trial_data"][this.trial_index]["prediction_start_ts"] = prediction_start_ts;
    this.data["trial_data"][this.trial_index]["prediction_shape"] = trialObj.prediction;
    this.data["trial_data"][this.trial_index]["prediction_catches_fish"] = trialObj.prediction_outcome;

    // Update html for this prediction trial
    var that = this;
    $("#exp-container").empty(); // TODO consider making a separate function to clear stuff out, we call this a lot...
    $("#next-exp").hide();
    $("#exp-container").load(this.prediction_htmlpath, function() {
        evidenceShape.drawLure(canvasId = "prediction-shape-canvas", sizeConfig = "prediction"); // TODO store this ID somewhere sensible

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // Process whether they clicked everything here (prevent from clicking if they didn't click stuff)
            var radio_resp = $("input[name='prediction']:checked").val();
            var slider_resp = $("#prediction-slider").val();
            if (radio_resp === undefined) {
                alert("Please select an answer for both the checkbox and the slider!");
            } else {
                // Add checkbox and slider values to experiment data object
                that.data["trial_data"][that.trial_index]["input_prediction_catches_fish"] = parseInt(radio_resp);
                that.data["trial_data"][that.trial_index]["input_prediction_conf"] = parseInt(slider_resp);
                // Add timestamp for end of prediction and end of individual trial
                var pred_end_ts = new Date().getTime();
                that.data["trial_data"][that.trial_index]["prediction_end_ts"] = pred_end_ts;
                that.data["trial_data"][that.trial_index]["trial_n_end_ts"] = pred_end_ts;
                // Process whether to show next trial or continue to rule generation task
                that.trial_index += 1;
                if (that.trial_index >= that.trial_array.length) {
                    console.log("Completed all trials.");
                    that.data["trial_end_ts"] = new Date().getTime();
                    that.showRuleGeneration();
                } else {
                    that.showEvidence();
                }
            }
        });
    });

};


Experiment.prototype.showRuleGeneration = function() {
    console.log("Collecting rule generation.");
    // Instantiate relevant fields in data object
    this.data["generation_data"] = {"input_free_response": ""};
    this.data["generation_data"]["generation_start_ts"] = new Date().getTime();
    // Update html for rule generation
    var that = this;
    $("#obs-container").hide(); // TODO make separate function to clear out full trial stuff (observations etc.)
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.generate_htmlpath, function() {
        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // process whether they wrote anything here (prevent from clicking if they didn't write anything)
            var resp = $("#generate-response").val();
            if (resp === "") {
                alert("Please respond to the prompt!");
            } else {
                // Add what they wrote to experiment data object and track time on task
                that.data["generation_data"]["input_free_response"] = resp;
                that.data["generation_data"]["generation_end_ts"] = new Date().getTime();
                that.showJudgmentTask();
            }
        });
    });
};


Experiment.prototype.showJudgmentTask = function() {
    console.log("Collecting rule judgments.");
    // Instantiate relevant fields in data object
    this.data["generation_data"]["judgment_task"] = [];
    this.data["generation_data"]["judgment_start_ts"] = new Date().getTime();
    // Update html for rule judgment task
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.judgment_htmlpath, function() {
        for (jIndex = 1; jIndex <= that.judgment_array.length; jIndex++) {
            var jItem = that.judgment_array[jIndex - 1];
            var shapeInfo = jItem.probe;
            var jShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
                shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);
            // write shape info to data object
            that.data["generation_data"]["judgment_task"].push({"judgment_index": jIndex,
                                                                "judgment_shape": shapeInfo,
                                                                "judgment_catches_fish": jItem.catches_fish});
            jShape.drawLure(canvasId = "generate-item-canvas-" + jIndex, sizeConfig = "generate"); // TODO store this ID somewhere sensible

            // Set image for radio buttons (catch fish, no fish)
            // TODO store these IDs somewhere sensible
            $("#checkbox-fish-generate-" + jIndex).attr("src", that.fish_img_path_small);
            $("#checkbox-no_fish-generate-" + jIndex).attr("src", that.no_fish_img_path_small);
        }

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // Process whether they clicked anything here (prevent from clicking next if they didn't)
            var radio_responses = [];
            var slider_responses = [];
            for (j = 1; j <= that.judgment_array.length; j++) {
                var radio_resp = $("input[name='generate-" + j + "']:checked").val();
                var slider_resp = $("#prediction-slider-" + j).val();
                if (radio_resp !== undefined) {
                    radio_responses.push(parseInt(radio_resp));
                    slider_responses.push(parseInt(slider_resp));
                }
            }
            // check that participant responded to all radio buttons
            if (radio_responses.length < that.judgment_array.length) {
                alert("Please select an answer for all the checkboxes and sliders!");
            } else {
                // Add selected answers to experiment data object
                for (j = 0; j < that.data["generation_data"]["judgment_task"].length; j++) {
                    that.data["generation_data"]["judgment_task"][j]["input_judgment"] = radio_responses[j];
                    that.data["generation_data"]["judgment_task"][j]["input_judgment_conf"] = slider_responses[j];
                }
                that.data["generation_data"]["judgment_end_ts"] = new Date().getTime();
                that.showEvaluationTask();
            }
        });
    });
};


Experiment.prototype.showEvaluationTask = function() {
    console.log("Showing rule evaluation for rule: ", this.eval_index + 1);
    var eval_n_start_ts = new Date().getTime();
    // Instantiate relevant fields in data object
    if (!("evaluation_data" in this.data)) {
        this.data["evaluation_data"] = {"eval_ratings": []};
        this.data["evaluation_data"]["evaluation_start_ts"] = new Date().getTime();
    }

    var ruleEval = this.evalArray[this.eval_index];
    // Update data with this eval object
    this.data["evaluation_data"]["eval_ratings"].push({"eval_index": this.eval_index + 1,
                                                        "eval_n_start_ts": eval_n_start_ts,
                                                        "rule_text": ruleEval.rule_text,
                                                        "is_target_rule": ruleEval.is_target});
    // Update html for evaluation task
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").load(this.eval_htmlpath, function() {
        $("#obs-container").show(); // show observed evidence during evaluation task
        $("#eval-rule").text(ruleEval.rule_text);

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // Add what user selected to experiment data object and timestamp end of evaluation
            var slider_resp = $("#eval-slider").val();
            that.data["evaluation_data"]["eval_ratings"][that.eval_index]["input_rule_rating"] = parseInt(slider_resp);
            that.data["evaluation_data"]["eval_ratings"][that.eval_index]["eval_n_end_ts"] = new Date().getTime();
            // Check whether eval task is complete and either proceed to memory task or complete next eval item
            that.eval_index += 1;
            if (that.eval_index >= that.evalArray.length) {
                // Add end timestamp to data
                that.data["evaluation_data"]["evaluation_end_ts"] = new Date().getTime();
                that.showMemoryTask();
            } else {
                that.showEvaluationTask();
            }
        });
    });
};


Experiment.prototype.showMemoryTask = function() {
    console.log("Showing memory task.");
    // Instantiate relevant fields in data object
    this.data["memory_data"] = {"memory_responses": []};
    this.data["memory_data"]["memory_start_ts"] = new Date().getTime();

    // Update html for memory task
    var that = this;
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#obs-container").hide(); // TODO make separate function to clear out full trial stuff (observations etc.)
    $("#exp-container").load(this.memory_htmlpath, function() {
        for (memIndex = 1; memIndex <= that.memory_array.length; memIndex++) {
            var memItem = that.memory_array[memIndex - 1];
            var shapeInfo = memItem.probe;
            var memShape = new Lure(shapeInfo.top_shape, shapeInfo.bottom_shape,
                shapeInfo.top_color, shapeInfo.bottom_color, shapeInfo.top_texture, shapeInfo.bottom_texture);
            // Update data with this memory object
            that.data["memory_data"]["memory_responses"].push({"memory_shape": shapeInfo,
                                                                "memory_shape_in_expt": memItem.in_expt,
                                                                "memory_shape_catches_fish": memItem.catches_fish});
            memShape.drawLure(canvasId = "memory-item-canvas-" + memIndex, sizeConfig = "memory"); // TODO store this ID somewhere sensible
        }

        // Update button response
        $("#next-exp").show();
        $("#next-exp").unbind().click(function() {
            // Process whether user clicked anything here (prevent from clicking next if they didn't)
            var mem_responses = [];
            for (m = 1; m <= that.memory_array.length; m++) {
                var radio_resp = $("input[name='memory-" + m + "']:checked").val();
                if (radio_resp !== undefined) {
                    mem_responses.push(parseInt(radio_resp));
                }
            }
            if (mem_responses.length < that.data["memory_data"]["memory_responses"].length) {
                alert("Please select an answer for all the checkboxes!");
            } else {
                // Add what user selected to experiment data object
                for (m = 0; m < that.data["memory_data"]["memory_responses"].length; m++) {
                    that.data["memory_data"]["memory_responses"][m]["input_shape_in_expt"] = mem_responses[m];
                }
                that.data["memory_data"]["memory_end_ts"] = new Date().getTime();
                that.endExperiment();
            }
        });
    });
};


Experiment.prototype.endExperiment = function() {
    console.log("End of experiment!");
    // Write end of experiment ts to data object
    this.data["expt_end_ts"] = new Date().getTime();

    // update html to reflect end of experiment
    $("#exp-container").empty();
    $("#next-exp").hide();
    $("#exp-container").html("<h1>" + this.end_game_msg + "</h1>");

    // write data to json
    this.writeData();
};


Experiment.prototype.writeData = function() {
    console.log("Writing experiment data to json");
    var expt = {
        "exptname": "go_fish", // name of experiment for easy reference
        "exptversion": "1" // version number for the broader experiment run (in case we modify subsequently)
    };
    var client_trials = {
        "test": this.istest,
        "sid": this.exptid
    };
    // Write trial data
    this.ajaxWrite({"expt": expt,
                    "client": client_trials,
                    "trials": this.data});
};


Experiment.prototype.ajaxWrite = function(jsondata) {
    var results = JSON.stringify(jsondata);
    console.log(results);
    $.ajax({
        dataType: "json",
        type: "POST",
        url: WRITE_ENDPOINT,
        data: {data: results},
        success: function(data){console.log("Success saving data!");},
        error: function(xhr, status, error) {
            console.log("Failure saving data. \n" +
                        "Response: " + xhr.responseText + "\n" +
                        "Status: " + status + "\n" +
                        "Error: " + error);
        }
    });
};

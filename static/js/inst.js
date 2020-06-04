/*
 * Library for running through instructions before returning to experiment flow
 */


Instructions = function(htmlpath, instructionSet) {
    this.htmlpath = htmlpath; // Path to html page to load for instructions
    this.instructionSet = instructionSet; // Array of instruction text elements to display
    this.instructionsComplete = false; // Bool to flip when all instructions have been shown
    this.instructionsIndex = 0; // Index to keep track of how many instructions have been processed
};

/*
 * Main function for running instructions, called when Instructions object is instantiated
 */
Instructions.prototype.run = function(experimentCallback) {
    console.log("Starting instructions");
    this.callback = experimentCallback; // Function to call when instructions complete

    // Load html for displaying instructions
    var that = this;
    console.log("loading: ", that.htmlpath);
    $("body").load(that.htmlpath, function() {
        that.populateInstruction()
        $("#next-inst").unbind().click(function() {that.buttonNext();});
    });
};

/*
 * Function called by button click, used for moving through instruction flow
 */
Instructions.prototype.buttonNext = function() {
    if (this.instructionsIndex >= this.instructionSet.length) {
        console.log("End of instructions");
        this.callback();
    } else {
        this.populateInstruction();
    }
};

/*
 * Function to populate instruction html elements with appropriate text/images
 * during each phase of instructions
*/
Instructions.prototype.populateInstruction = function() {
    instructionElem = this.instructionSet[this.instructionsIndex];
    // Remove any existing images in the canvas
    $(".instruction-img").remove();
    // Add top text
    $("#text-top").text(instructionElem.top_text);
    // Add bottom text
    $("#text-bottom").text(instructionElem.bottom_text);
    // Add and format image
    if (instructionElem.canvas_img != "") {
        $("#canvas-mid").prepend("<img class='instruction-img' src='" + instructionElem.canvas_img + "' />");
    }

    this.instructionsIndex++;
    return
};

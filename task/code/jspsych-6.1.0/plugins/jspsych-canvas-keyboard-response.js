/**
 * jspsych-canvas-keyboard-response
 * Noa
 *
 * plugin for displaying a stimulus on a canvas, manipulating its colour and getting a keyboard response
 *
 * based on image-keyboard-response
 *
 **/


jsPsych.plugins["canvas-keyboard-response"] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('canvas-keyboard-response', 'stimulus', 'image');

  plugin.info = {
    name: 'canvas-keyboard-response',
    description: '',
    parameters: {
      stimulus: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The image to be displayed. If array will display images next to each other.'
      },
      stimulus_height: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Image height',
        default: null,
        description: 'Set the image height in pixels'
      },
      stimulus_width: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Image width',
        default: null,
        description: 'Set the image width in pixels'
      },
      maintain_aspect_ratio: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Maintain aspect ratio',
        default: true,
        description: 'Maintain the aspect ratio after setting width or height'
      },
      stimulus_color: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Shape color",
        default: null,
        description: "The color of the shape [R, G, B, A]. If stimulus is an array, this must be an array as well."
      },
      stimulus_background_color: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Stimulus background color",
        default: null,
        description: "The color of the stimulus background (everything that isn't shape) [R, G, B, A]"
      },
      choices: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        array: true,
        pretty_name: 'Choices',
        default: jsPsych.ALL_KEYS,
        description: 'The keys the subject is allowed to press to respond to the stimulus.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed above the stimulus.'
      },
      label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Label',
        default: null,
        description: 'Any content here will be displayed below the stimulus.'
      },
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show trial before it ends.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, trial will end when subject makes a response.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {
    // Build DOM Start ----------
    var html = '';

    // add prompt
    if (trial.prompt !== null){
      html += trial.prompt;
    };

    // Canvas object to display image
    html += '<div id="jspsych-canvas-keyboard-response-div">';
    if (typeof trial.stimulus == 'object') { //if more than one stimulus shown
      for (var i = 0; i < trial.stimulus.length; i++) {
        html += '<canvas id="jspsych-canvas-keyboard-response-stimulus-'+i+'"></canvas>';
      }
    } else { // if only one stimulus
      html += '<canvas id="jspsych-canvas-keyboard-response-stimulus"></canvas>';
    }
    html += '</div>';

    // add prompt
    if (trial.label !== null){
      html += trial.label;
    };

    // render
    display_element.innerHTML = html;
    // Build DOM End ----------

    // Draw Image Start ----------
    function getColorIndicesForCoord(x, y, width) {
      var red = y * (width * 4) + x * 4;
      return [red, red + 1, red + 2, red + 3];
    }
    var shape_threshold = 200;

    if (typeof trial.stimulus == 'object') { //if more than one stimulus shown
      for (var i = 0; i < trial.stimulus.length; i++) {
        // Create image object to draw on canvas ---
        var image = new Image();
        image.src = trial.stimulus[i];
        image.id = 'jspsych-canvas-keyboard-response-image-'+i;

        // Draw image onto canvas ---
        var canvas = document.getElementById('jspsych-canvas-keyboard-response-stimulus-'+i);
        canvas.width  = trial.stimulus_width;
        canvas.height = trial.stimulus_height;
        var ctx = canvas.getContext("2d");

        ctx.drawImage(image, 0, 0, trial.stimulus_width, trial.stimulus_height);

        // Change Shape Colour ---
        var imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
        var data = imageData.data;

        if (trial.stimulus_color[i] !== null) { //  black pixels = shape, white = background
          for (var y = 0; y < canvas.height; y++) {
            for (var x = 0; x < canvas.width; x++) {
              var colorIndices = getColorIndicesForCoord(x, y, canvas.width); // get RGBA indices for each pixel
              if (data[colorIndices[0]] < shape_threshold && data[colorIndices[1]] < shape_threshold && data[colorIndices[2]] < shape_threshold ) { //check if pixel part of shape
                // change colour to shape colour
                data[colorIndices[0]] = trial.stimulus_color[i][0];
                data[colorIndices[1]] = trial.stimulus_color[i][1];
                data[colorIndices[2]] = trial.stimulus_color[i][2];
                data[colorIndices[3]] = trial.stimulus_color[i][3];
              }
            }
          }
        };
        // Change Stimulus Background Colour (everything that isn't shape) - assumes single background colour for all stimuli
        if (trial.stimulus_background_color !== null) {
          for (var y = 0; y < canvas.height; y++) {
            for (var x = 0; x < canvas.width; x++) {
              var colorIndices = getColorIndicesForCoord(x, y, canvas.width); //get RGBA indices for each pixel
              if (data[colorIndices[0]] >= shape_threshold && data[colorIndices[1]] >= shape_threshold && data[colorIndices[2]] >= shape_threshold ) {
                data[colorIndices[0]] = trial.stimulus_background_color[0];
                data[colorIndices[1]] = trial.stimulus_background_color[1];
                data[colorIndices[2]] = trial.stimulus_background_color[2];
                data[colorIndices[3]] = trial.stimulus_background_color[3];
              }
            }
          }
        };

        // Display new colour values ---
        ctx.putImageData(imageData, 0, 0);
      }
    } else { // if only one stimulus
      // Create image object to draw on canvas ---
      var image = new Image();
      image.src = trial.stimulus;
      image.id = 'jspsych-canvas-keyboard-response-image';

      // Draw image onto canvas ---
      var canvas = document.getElementById('jspsych-canvas-keyboard-response-stimulus');
      canvas.width  = trial.stimulus_width;
      canvas.height = trial.stimulus_height;
      var ctx = canvas.getContext("2d");

      ctx.drawImage(image, 0, 0, trial.stimulus_width, trial.stimulus_height);

      // Change Shape Colour ---
      var imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
      var data = imageData.data;

      if (trial.stimulus_color !== null) { //  black pixels = shape, white = background
        for (var y = 0; y < canvas.height; y++) {
          for (var x = 0; x < canvas.width; x++) {
            var colorIndices = getColorIndicesForCoord(x, y, canvas.width); //get RGBA indices for each pixel
            if (data[colorIndices[0]] < shape_threshold && data[colorIndices[1]] < shape_threshold && data[colorIndices[2]] < shape_threshold ) { //check if pixel part of shape
              // change colour to shape colour
              data[colorIndices[0]] = trial.stimulus_color[0];
              data[colorIndices[1]] = trial.stimulus_color[1];
              data[colorIndices[2]] = trial.stimulus_color[2];
              data[colorIndices[3]] = trial.stimulus_color[3];
            }
          }
        }
      };

      // Change Stimulus Background Colour (everything that isn't shape)
      if (trial.stimulus_background_color !== null) {
        for (var y = 0; y < canvas.height; y++) {
          for (var x = 0; x < canvas.width; x++) {
            var colorIndices = getColorIndicesForCoord(x, y, canvas.width); //get RGBA indices for each pixel
            if (data[colorIndices[0]] >= shape_threshold && data[colorIndices[1]] >= shape_threshold && data[colorIndices[2]] >= shape_threshold ) {
              data[colorIndices[0]] = trial.stimulus_background_color[0];
              data[colorIndices[1]] = trial.stimulus_background_color[1];
              data[colorIndices[2]] = trial.stimulus_background_color[2];
              data[colorIndices[3]] = trial.stimulus_background_color[3];
            }
          }
        }
      };

      // Display new colour values ---
      ctx.putImageData(imageData, 0, 0);
    };
    // Draw Image End ----------

    // store response
    var response = {
      rt: null,
      key: null
    };

    // function to end trial when it is time
    var end_trial = function() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // kill keyboard listeners
      if (typeof keyboardListener !== 'undefined') {
        jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);
      }

      // gather the data to store for the trial
      var trial_data = {
        "rt": response.rt,
        "stimulus": trial.stimulus,
        "key_press": response.key,
        "stimulus_color": trial.stimulus_color,
        "stimulus_background_color": trial.stimulus_background_color,
        "stimulus_height": trial.stimulus_height,
        "stimulus_width": trial.stimulus_width
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };

    // function to handle responses by the subject
    var after_response = function(info) {

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-canvas-keyboard-response-div').className += ' responded';

      // only record the first response
      if (response.key == null) {
        response = info;
      }

      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // start the response listener
    if (trial.choices != jsPsych.NO_KEYS) {
      var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: after_response,
        valid_responses: trial.choices,
        rt_method: 'performance',
        persist: false,
        allow_held_key: false
      });
    }

    // hide stimulus if stimulus_duration is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-canvas-keyboard-response-div').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if trial_duration is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();

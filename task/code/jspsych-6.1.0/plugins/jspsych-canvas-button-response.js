/**
 * jspsych-canvas-button-response
 * Noa
 *
 * adapted from image-button-response plugin
 * custom plugin for displaying a stimulus on a canvas and getting a button response
 *
 *
 **/

jsPsych.plugins["canvas-button-response"] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('canvas-button-response', 'stimulus', 'image');

  plugin.info = {
    name: 'canvas-button-response',
    description: '',
    parameters: {
      stimulus: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The image to be displayed'
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
        description: "The color of the shape [R, G, B, A]"
      },
      stimulus_background_color: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: "Stimulus background color",
        default: null,
        description: "The color of the stimulus background (everything that isn't shape) [R, G, B, A]"
      },
      choices: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Choices',
        default: undefined,
        array: true,
        description: 'The labels for the buttons.'
      },
      button_html: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button HTML',
        default: '<button class="jspsych-btn">%choice%</button>',
        array: true,
        description: 'The html of the button. Can create own style.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed under the button.'
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
        description: 'How long to show the trial.'
      },
      margin_vertical: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin vertical',
        default: '0px',
        description: 'The vertical margin of the button.'
      },
      margin_horizontal: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin horizontal',
        default: '8px',
        description: 'The horizontal margin of the button.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, then trial will end when user responds.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {
    // Build DOM Start ----------
    // Canvas object to display image
    var html = '<canvas id="jspsych-canvas-button-response-stimulus"></canvas>';

    //display buttons
    var buttons = [];
    if (Array.isArray(trial.button_html)) {
      if (trial.button_html.length == trial.choices.length) {
        buttons = trial.button_html;
      } else {
        console.error('Error in canvas-button-response plugin. The length of the button_html array does not equal the length of the choices array');
      }
    } else {
      for (var i = 0; i < trial.choices.length; i++) {
        buttons.push(trial.button_html);
      }
    }
    html += '<div id="jspsych-canvas-button-response-btngroup">';

    for (var i = 0; i < trial.choices.length; i++) {
      var str = buttons[i].replace(/%choice%/g, trial.choices[i]);
      html += '<div class="jspsych-canvas-button-response-button" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'" id="jspsych-canvas-button-response-button-' + i +'" data-choice="'+i+'">'+str+'</div>';
    }
    html += '</div>';

    //show prompt if there is one
    if (trial.prompt !== null) {
      html += trial.prompt;
    }

    display_element.innerHTML = html;
    // Build DOM End ----------

    // Draw Image Start ----------
    // Create image object to draw on canvas
    var image = new Image();
    image.src = trial.stimulus;
    image.id = 'jspsych-canvas-button-response-image';
    // configure image size - TODO: CHECK IF SIZING CORRECT
    if(trial.stimulus_width == null && trial.maintain_aspect_ratio){
      trial.stimulus_width = Math.floor((trial.stimulus_height/image.naturalHeight) * image.naturalWidth);
    } else if(trial.stimulus_height == null && trial.maintain_aspect_ratio){
      trial.stimulus_height = Math.floor((trial.stimulus_width/image.naturalWith) * image.naturalHeight);
    }
    image.style.height = trial.stimulus_height+'px';
    image.style.width = trial.stimulus_width+'px';
    // if(trial.stimulus_height !== null){
    //   image.style.height = trial.stimulus_height+'px';
    //   if(trial.stimulus_width == null && trial.maintain_aspect_ratio){
    //     image.style.width = 'auto';
    //   }
    // }
    // if(trial.stimulus_width !== null){
    //   image.style.width = trial.stimulus_width+'px';
    //   if(trial.stimulus_height == null && trial.maintain_aspect_ratio){
    //     image.style.height = 'auto';
    //   }
    // }

    // Draw image onto canvas
    var canvas = document.getElementById('jspsych-canvas-button-response-stimulus');
    canvas.width  = image.width; //TO DO: check that is correct
    canvas.height = image.height;
    var ctx = canvas.getContext("2d");
    ctx.drawImage(image, 0, 0);

    // Change Shape Colour
    var imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    var data = imageData.data;

    function getColorIndicesForCoord(x, y, width) {
      var red = y * (width * 4) + x * 4;
      return [red, red + 1, red + 2, red + 3];
    }

    var shape_threshold = 200;
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

    // display new colour values
    ctx.putImageData(imageData, 0, 0);

    // Draw Image End ----------

    // start timing
    var start_time = performance.now();

    for (var i = 0; i < trial.choices.length; i++) {
      display_element.querySelector('#jspsych-canvas-button-response-button-' + i).addEventListener('click', function(e){
        var choice = e.currentTarget.getAttribute('data-choice'); // don't use dataset for jsdom compatibility
        after_response(choice);
      });
    }

    // store response
    var response = {
      rt: null,
      button: null
    };

    // function to handle responses by the subject
    function after_response(choice) {

      // measure rt
      var end_time = performance.now();
      var rt = end_time - start_time;
      response.button = choice;
      response.rt = rt;

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-canvas-button-response-stimulus').className += ' responded';

      // disable all the buttons after a response
      var btns = document.querySelectorAll('.jspsych-canvas-button-response-button button');
      for(var i=0; i<btns.length; i++){
        //btns[i].removeEventListener('click');
        btns[i].setAttribute('disabled', 'disabled');
      }

      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // function to end trial when it is time
    function end_trial() {

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        "rt": response.rt,
        "stimulus": trial.stimulus,
        "stimulus_color": trial.stimulus_color,
        "stimulus_background_color": trial.stimulus_background_color,
        "stimulus_height": trial.stimulus_height,
        "stimulus_width": trial.stimulus_width,
        "button_pressed": response.button
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };



    // hide image if timing is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-canvas-button-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if time limit is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();

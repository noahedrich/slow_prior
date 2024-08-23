/* ######################### ATTENTION-CHECKS - TEXT ######################### */
var questions = {
  0: {
    prompt: '<h2>What are the two response keys?</h2>'+
    '<p>Please select your answer based on the task instructions you just read.</p>',
    options: ["e and i", "f and i", "f and j", "e and j"],
    correct_answer: 'f and j'},
  1: {
    prompt: '<h2>What is the colour of a ripe lemon?</h2>'+
    '<p>We are asking for the color of a ripe lemon, and not the colour of a lime.</p>'+
    '<p>We care about you reading the instructions carefully.</p>' +
    '<p>In order to demonstrate that, make sure to select green as your answer below.</p>',
    options: ["blue", "yellow", "green", "red"],
    correct_answer: 'green'},
  2: {
    prompt: '<h2>What does a mouse prefer to eat?</h2>'+
    '<p>According to popular belief, mice supposedly love cheese.</p>'+
    '<p>However, researchers found that mice do not like cheese at all. Instead they prefer sugary foods, such as fruit.</p>',
    options: ["cheese", "insects", "bacon", "fruit"],
    correct_answer: 'fruit'},
  3: {
    prompt: '<h2>Can cats see in complete darkness?</h2>'+
    '<p>Cats are able to see in very low light which makes them excellent night time hunters.</p>'+
    '<p> Even though it is often believed that cats can see in complete darkness, it is not correct.</p>'+
    '<p>They require at least some source of light to be able to see.</p>',
    options: ["yes", "no"],
    correct_answer: 'no'},
  4: {
    prompt: '<h2>Based on the text below, how many siblings does Lisa have?</h2>'+
    '<p>Lisa has a brother and a sister. </p>'+
    '<p>But we don’t really care about how many siblings Lisa has. '+
    'We care more about whether you are paying careful attention to the instructions. '+
    'Make sure to select four as your answer in order to demonstrate that you are paying attention. </p>',
    options: ["1", "2", "3", "4"],
    correct_answer: '4'},
  5: {
    prompt: '<h2>Based on the text below, what is the first month of the season?</h2>'+
    '<p>The month we are looking for is also the coldest month of the year and considered their least favourite by many people. </p>'+
    '<p>Make sure to select July as your answer below in order to demonstrate that you are paying attention to the instructions. </p>',
    options: ["January", "July", "September", "December"],
    correct_answer: 'July'}
};

var attention_checks = [];

for (var x = 0; x < Object.keys(questions).length; x++) {
  attention_checks[x] = {
    type: 'survey-multi-choice',
    questions: [{
      prompt: questions[x].prompt,
      name: x,
      options: questions[x].options,
      required: true }],
      data: {exp_part: 'attention_check', correct_answer: questions[x].correct_answer, name: x},
      on_finish: function(data) {
        var correct_answer = data.correct_answer;
        if (JSON.parse(data.responses)[data.name] == correct_answer) {
          data.correct = true;
        } else {
          data.correct = false;
        }
      }
    };
};

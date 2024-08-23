var consent_text = ['<h2>Study Information and Statement of Informed Consent</h2>'+
'<p>Welcome to our study! Thank you for your interest in participating. '+
'Please read the following information carefully. </p>'+
'<h3>1. Aim of the study</h3>'+
'<p>In this study we want to understand how humans learn which features of the environment are important. '+
'You are eligible for this study if you are fluent in English, 18-40 years old and are not currently receiving psychiatric treatment or taking medication for mental illness. </p>'+
'<h3>2. Procedure and content of the study</h3>'+
'<p>In this study, you will perform a learning and decision-making task. '+
'You will be shown objects on-screen and must decide whether to accept or reject these objects to earn coins. '+
'Different objects are worth different amounts of coins, so you need to learn which objects are worth accepting, and which are worth rejecting, to earn as much reward as possible. </p>'+
'<p>We will first show you some instructions and check to make sure you understand them (about 2 mins). '+
'If your answers to these questions are incorrect, the experiment will end without payment. '+
'We will explain in detail what you have to do. '+
'At the end, we will ask for basic demographic information. '+
'Please only participate from a computer (no mobiles, no tablets) and if your screen is at least 13-inch (33 cm) diagonal. '+
'Also know that you will need remain in full screen mode for the entire experiment. '+
'If you exit full screen mode, there is no guarantee we can compensate you. </p>',
'<h3>3. What will happen to the collected data?</h3>'+
'<p>This study is a research project of the Max Planck Institute for Human Development, Berlin. '+
'The data collected will be used for research purposes only. '+
'Your Prolific ID will be stored separately from the study data. '+
'The study data will be stored under an individual code number. '+
'This code number is stored on a separate code list in a secure location, and can only be accessed by a limited number of project members. '+
'The code list is the only link between your Prolific ID and your study data. '+
'After payment, the code list will be deleted, and it will no longer be possible to link the study data to your Prolific ID. '+
'The study data (but not Prolific ID) may be shared with cooperation partners and made publicly accessible via research data bases or scientific publications (typically via the Internet). '+
'The study data may also be used for new research questions going beyond the purposes of this particular study. '+
'Study data are only transferred or made publicly accessible without Prolific ID or any data in which persons are identifiable.</p>'+
'<h3>4. Participation is voluntary</h3>'+
'<p>Participation in this study is voluntary. '+
'You may withdraw from the study or withdraw your consent to data processing and usage any time before payment. '+
'To do this, please contact neuroexperiments@mpib-berlin.mpg.de. '+
'Please note that after payment, it is no longer possible to link your Prolific ID to your study data. </p>',
'<h3>5. Consent</h3>'+
'<p>The study will take 45 minutes. '+
'You will receive a base compensation of 5.00 GBP and a bonus of up to 2.50 GBP depending on your performance on the task. '+
"If you don't consent to the conditions outlined on the last two pages, please close your browser window now to abort the experiment. "+
'Otherwise, press next to continue to the consent page.</p>'];

var consent_statement = 'I have read and understood the conditions outlined above. <br>I consent to participate in the study and agree to the collection, storage, and use of my data as described above.';

var instructions_text = ['<p>Dear participant,<br>'+
'Thank you very much for participating in our experiment!<br><br>'+
'In this game you are a gem stone hunter, travelling from forest to forest looking for valuable gems to sell at markets. '+
'You will visit eight different forests and spend some time in each before moving on to the next one. '+
'For each new forest, there is a different nearby village and you must learn which gems sell well at the market there. </p>'+
'<p>When you get to a new forest you will first have an audience with the elders of the village. '+
'The elders are very knowledgeable about the gems of their forest and will show you a few of the gems you may find there. ' +
'They will not show you the gems for very long, so pay close attention. </p>' +
'<p>Following your meeting with the elders, you will spend some time gathering gems in the forest. ' +
'<b>Each time you find a gem you need to decide whether to carry it back to the market to sell. </b><br>'+
'On the screen you will see a gem with the words "Accept" and "Reject" displayed on either side of the screen below it. '+
'You can make your decision by pressing <kbd>F</kbd> for the left option and <kbd>J</kbd> for the right option. '+
'Please keep your left and right index fingers on the respective keys throughout the task. <br>'+
'Pay close attention to the displayed labels, as they can switch sides throughout the game. </p>'+
'<p>After making your decision, <b>the number of coins you received as a result will be displayed on the screen.</b> '+
'If you accept the gem, you will see how much money you got for selling it. '+
'A gem can be worth anything between <b>0 and 100 coins</b>. <br>'+
'If you reject it, you instead gather other forest goods that you sell at the market, for which you will always get <b>50 coins</b>. <br>'+
'You have 4 seconds to make your decision, otherwise you will receive 0 coins. </p>',
'<p>Your objective will be to gain as many coins as possible throughout the game. '+
'Importantly, different gems are worth a different number of coins. '+
'<b>Learning how to recognise valuable gems will help you decide whether you should accept or reject a given gem.</b> </p>'+
'<p>Different villages will pay different amounts for the gem stones, so you will need to <b>learn from scratch which gem stones are valuable in each village.</b> '+
'During your time in one forest, the value of gem stones will not change, so your learning can be applied to your entire experience in that forest. </p>'+
'<p>At the end of your time gathering gems in one forest, the village elders will test your knowledge of the value of gems. '+
'They will will show you pairs of gems and ask you to choose the one that you think would bring you more coins, based on what you have learnt in that forest. </p>'+
'<p>The <b>coins you earn at the market and your accuracy in choosing the correct gem in the elders’ test will be converted to a bonus payment</b> at the end of the study. </p>'+
'<p>Moving to a new forest will be announced and you will have opportunities for breaks in between. </p>',
'<p>Before starting the experiment, we will check whether you are a human, comprehend English and your understanding of the instructions with three questions. '+
'This is a step we must take to protect data quality. Some questions are slightly tricky and require you to pay very close attention to all texts and to answer accordingly. '+
'You must answer correctly to be able to take part in the experiment. Otherwise you will be returned to Prolific and we cannot reimburse you. </p>'+
'<p>Please press next to continue to the questions.</p>'];

var debrief_text = '<p>Thank you very much for your participation!</p>'+
'<p>In this study, we investigate how humans learn to identify relevant features in the environment. <br>'+
'The shape and colour of the gems in the task changed either a little or a lot from trial to trial. '+
'For example, if shape was changing gradually, then colour would change quickly (or vice versa). '+
'We expect that participants learn to identify the relevant feature more quickly when it is changing slowly compared to when it is changing quickly. </p>'+
'<p>It is no problem if you did not notice the speed of feature change and it says nothing about your intelligence. '+
'We intentionally designed the task in a way that participants are unlikely to notice the changes. </p>'+
'<p>If you have any further questions about the study, please contact us. '+
'Press space to end and get back to Prolific. '+
'<b>It might take a few moments to redirect, during which you will see a blank screen, please do not close this window</b>! ' +
'(You will be redirected automatically after one minute.)</p>';

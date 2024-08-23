var postData = function(data, url, callback){
$.ajax({
    url: url,
    type: 'post',
    headers: {
        'Access-Control-Allow-Origin': '*'
    },
	data : data,
    processData: false,
    contentType: "application/json; charset=utf-8",
    dataType: 'JSON',
    success: function (data) {
      console.info(data);
       callback()
    },
    error: function (error) {
      console.info(JSON.stringify(error));
       callback()
    }
   });
  }
$(function(){
    window.enable_logging = true;
    //////////////////////
    // Create namespace //
    //////////////////////
    var chatApp = {
	user_code: '',
	user_nick: ''};
    
    window.chatApp = chatApp;

    var chatClient = window.chatClient;

    ////////////
    // Models //
    ////////////
    var User = Backbone.Model.extend({});

    var Comment = Backbone.Model.extend({});

    var MessageCollection = Backbone.Collection.extend({
	model: Comment
    });

    var UserCollection = Backbone.Collection.extend({
	model: User
    });

    ////////////
    // Router //
    ////////////
    chatApp.Router = Backbone.Router.extend({
	routes: {
	    '': 'init',
	    ':room_code': 'start_room'},
	init: function(){
	    this.start_room('');
	},
	start_room: function(room_code){
	    // Wait for dom and bullet to initialize
	    setTimeout(function(){
		chatClient.connect_to_room(room_code,
					   chatApp.user_code,
					   chatApp.user_nick);},
		       1000);
	}
    });
    chatApp.router = new chatApp.Router();
    Backbone.history.start();


    //connect to server events
    chatClient.on('onopen',
		  function(){$('#status').text('online');},
		  chatClient);
    chatClient.on('ondisconnect',
		  function(){$('#status').text('offline');},
		  chatClient);
    chatClient.on('onmessage',
		  function(data){
		      if(enable_logging && console)
			  console.log('response', data);
		      switch(data.type){
			  case 'heartbeat':
			  break;
			  case 'connected_to_room':
			  chatApp.room_code = data.room_code;
			  chatApp.user_code = data.user_code;
			  chatApp.user_nick = data.user_nick;
			  $("#text").text(chatApp.user_code + ' ' + chatApp.user_nick);
			  
			  break;
		      };
		  },
		  chatClient);
    
    chatClient.on('onheartbeat',
		  function(){this.send_message({type:'heartbeat', value:'ping'});},
		  chatClient);

});
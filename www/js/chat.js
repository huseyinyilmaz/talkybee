
$(function(){
    console.log('chat.js');
    //////////////////////
    // Create namespace //
    //////////////////////
    var chatApp = {};
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
	    console.log('router.init');
	},
	start_room: function(room_code){
	    console.log('start_room ' + room_code);
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
		  function(e){console.log('onmessage = ' + e.data);
			      if (e.data != 'pong'){
				  $('#time').text(e.data);}},
		  chatClient);
    chatClient.on('onheartbeat',
		  function(){console.log('ping');
			     this.bullet.send('ping');},
		  chatClient);

});
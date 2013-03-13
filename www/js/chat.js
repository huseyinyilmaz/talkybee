
$(function(){
    console.log('chat.js');
    //////////////////////
    // Create namespace //
    //////////////////////
    var chatApp = {};
    window.chatApp = chatApp;

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



    
    ////////////////////
    // Server adapter //
    ////////////////////
    var bullet = $.bullet('ws://localhost:8080/bullet');
    chatApp.bullet = bullet;
    bullet.onopen = function(){
	console.log('onopen');
	$('#status').text('online');
    };
    bullet.ondisconnect = function(){
	console.log('ondisconnect');
	$('#status').text('offline');
    };
    bullet.onmessage = function(e){
	console.log('onmessage');
	if (e.data != 'pong'){
	    $('#time').text(e.data);
	}
    };
    bullet.onheartbeat = function(){
	console.log('ping');
	bullet.send('ping');
    };


});
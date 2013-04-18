$(function(){
    // Create chat client 
    var chatClient = {
	connect_to_room: function(room_code,
				  user_code,
				  user_nick){
	    
	    this.send_message({type: 'connect_to_room',
			       room_code: room_code,
			       user_code: user_code,
			       user_nick: user_nick});
	},
	send_message: function(obj){
	    if(enable_logging && console)
		console.log('request', obj);
	    var json_string = JSON.stringify(obj);
	    this.bullet.send(json_string);
	}
    };
    //Make chatClient an event Handler
    chatClient = _.extend(chatClient, Backbone.Events);

    window.chatClient = chatClient;
    var host = location.host;
    var bullet = $.bullet('ws://' + host + '/bullet');
    chatClient.bullet = bullet;

    // Bind bullet events to chatClient events
    bullet.onopen = function(){chatClient.trigger('onopen');};
    bullet.ondisconnect = function(){chatClient.trigger('disconnect');};
    bullet.onmessage = function(e){
	_.each(JSON.parse(e.data),
	       function(msg){
		   chatClient.trigger('onmessage', msg);
	       });};
    bullet.onheartbeat = function(){chatClient.trigger('onheartbeat');};
});

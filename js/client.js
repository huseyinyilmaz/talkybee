$(function(){
    // Create chat client 
    var chatClient = {
	client: null,
	connect_to_server: function(session_id){
	    this.client = publicator.get_client(session_id);
	    console.log('Got Client', this.client);
	    this.client.onopen(function(){chatClient.trigger('onopen');});
	    this.client.ondisconnect(function(){chatClient.trigger('disconnect');});
            this.client.onmessage(function(msg){chatClient.trigger('onmessage', msg);});
	    this.client.onheartbeat(function(){chatClient.trigger('onheartbeat');});
	},
	connect_to_room: function(room_code,
				  user_code,
				  user_nick){
	    this.client.subscribe(room_code, 'all');
	    this.client.publish(room_code,
				{'type': 'user_data',
				 'user_code': user_code,
				 'user_nick': user_nick});
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
    //Call session
    publicator.get_session_id(_.bind(chatClient.connect_to_server, chatClient));

    //initialization completed. Add chatClient to global namespace
    window.chatClient = chatClient;


});

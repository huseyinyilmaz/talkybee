$(function(){
    
    // Create chat client 
    var chatClient = {
	init_room: function(){},
	send_message: function(user_code, msg){
	   
	},
	trigger_onopen: function(){
	    $('#status').text('online');
	},
	trigger_ondisconnect: function(){
	    $('#status').text('offline');
	},
	trigger_onmessage: function(e){
	    if (e.data != 'pong'){
		$('#time').text(e.data);
	    }
	},
	trigger_onheartbeat: function(){
	    console.log('ping');
	    self.bullet.send('ping');
	}
	
    };
    window.chatClient = chatClient;
    
    var bullet = $.bullet('ws://localhost:8080/bullet');
    chatClient.bullet = bullet;
    
    bullet.onopen = function(){
	chatClient.trigger_onopen();
    };
    bullet.ondisconnect = function(){
	chatClient.trigger_disconnect();
    };
    bullet.onmessage = function(e){
	chatClient.trigger_onmessage();
    };
    bullet.onheartbeat = function(){
	chatClient.trigger_onheartbeat();
    };
});


$(function(){
    // Create chat client 
    var chatClient = {
	send_message: function(obj){
	    console.log("sending="+JSON.stringify(obj));
	    this.bullet.send(JSON.stringify(obj));
	}
    };
    //Make chatClient an event Handler
    chatClient = _.extend(chatClient, Backbone.Events);

    window.chatClient = chatClient;
    
    var bullet = $.bullet('ws://localhost:8080/bullet');
    chatClient.bullet = bullet;

    // Bind bullet events to chatClient events
    bullet.onopen = function(){chatClient.trigger('onopen');};
    bullet.ondisconnect = function(){chatClient.trigger('disconnect');};
    bullet.onmessage = function(e){chatClient.trigger('onmessage', e);};
    bullet.onheartbeat = function(){chatClient.trigger('onheartbeat');};
});

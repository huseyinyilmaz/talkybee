$(function(){
    window.enable_logging = true;
    //////////////////////
    // Create namespace //
    //////////////////////
    var chatApp = {
	user_code: '',
	user_nick: '',
	add_user: function(code,nick){
	    chatApp.users.add({id:code, nick:nick});
	},
	remove_user:function(code){
	    chatApp.users.remove(chatApp.users.get(code));
	}
    };
    
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
    chatApp.users = new UserCollection();
    chatApp.messages = new MessageCollection();

    ///////////
    // Views //
    ///////////

    chatApp.UsersView = Backbone.View.extend({
	initialize: function(options){
	    _.bindAll(this);
	    this.collection.bind('add', this.render);
	    this.collection.bind('remove', this.render);
	    this.template_text = $('#users_template').html();
	},
	render: function(){
	    this.$el.html(
		Mustache.render(this.template_text,
				{collection:this.collection,
				 code:function(){return this.get('id');},
				 nick:function(){return this.get('nick');}
				})
	    );
	}
    });

    chatApp.usersView = new chatApp.UsersView({
	collection: chatApp.users,
	el: '#users_container'
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

    ////////////////////
    // Event handlers //
    ////////////////////
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
			  chatApp.add_user(data.user_code, data.user_nick);
			  chatApp.router.navigate(data.room_code);
			  $("#text").text(chatApp.user_code + ' ' + chatApp.user_nick);
			  break;
			  
			  case 'user_data':
			  chatApp.add_user(data.code, data.nick);
			  break;
			  case 'user_removed':
			  chatApp.remove_user(data.code);
			  break;
		      };
		  },
		  chatClient);
    
    chatClient.on('onheartbeat',
		  function(){this.send_message({type:'heartbeat', value:'ping'});},
		  chatClient);

});
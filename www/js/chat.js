$(function(){
    window.enable_logging = true;
    //////////////////////
    // Create namespace //
    //////////////////////
    var chatApp = {
	user_code: '',
	user_nick: '',
	add_user: function(code,nick){
	    chatApp.users.add({id:code, nick:nick},{merge:true});
	},
	add_message: function(code,message, type){
	    var user = chatApp.users.get(code);
	    var nick = user.get('nick');
	    chatApp.messages.add({code:code,
				  nick:nick,
				  message:message,
				  type:type});
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
	    this.collection.bind('change', this.render);
	    this.template_text = $('#users_template').html();
	},
	render: function(){
	    this.$el.html(
		Mustache.render(this.template_text,
				{collection:this.collection,
				 code:function(){return this.get('id');},
				 nick:function(){return this.get('nick');}
				}));
	}});
    
    chatApp.MessageView = Backbone.View.extend({
	tagName: 'li',
	className: 'message',
	template_text: $('#message_template').html(),
	initialize: function(){
	    _.bindAll(this);
	},
	render: function(){
	    this.$el.html(
		Mustache.render(
		    this.template_text,
		    this.model.toJSON()
		)
	    );
	    return this;
	}
    });

    chatApp.MessagesView = Backbone.View.extend({
	initialize: function(){
	    _.bindAll(this);
	    this.collection.bind('add', this.addMessage);
	},
	addMessage: function(model){
	    var messageView = new chatApp.MessageView({model:model});
	    messageView.render();
	    this.$el.append(messageView.$el);
	}
    });
    
    chatApp.usersView = new chatApp.UsersView({
	collection: chatApp.users,
	el: '#users_container'
    });
    
    chatApp.messagesView = new chatApp.MessagesView({
	collection: chatApp.messages,
	el: '#messages_container'
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
		  function(){console.log('online');},
		  chatClient);
    chatClient.on('ondisconnect',
		  function(){console.log('offline');},
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
			  break;
			  
			  case 'user_data':
			  chatApp.add_user(data.code, data.nick);
			  break;
			  case 'user_removed':
			  chatApp.remove_user(data.code);
			  break;
			  case 'message':
			  chatApp.add_message(data.code, data.message, 'message');
			  break;
		      };
		  },
		  chatClient);
    
    chatClient.on('onheartbeat',
		  function(){this.send_message({type:'heartbeat', value:'ping'});},
		  chatClient);
    
    $('#send_button').click(function(){
	chatClient.send_message({type: 'message',
				 value: $('#main_input').val()});});
    $('#rename_button').click(function(){
	chatClient.send_message({type: 'rename',
				 value: $('#rename_input').val()});});

});
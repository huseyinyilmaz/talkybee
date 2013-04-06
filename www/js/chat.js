$(function(){
    window.enable_logging = true;
    //////////////////////
    // Create namespace //
    //////////////////////
    var chatApp = {
	room_code: '',
	user_code: '',
	user_nick: '',
	is_locked: false,
	add_user: function(code,nick){
	    chatApp.users.add({id:code, nick:nick},{merge:true});
	},
	update_room: function(code, is_locked){
	    // if lock value did not changed do
	    // not do anything
	    if(is_locked !== this.is_locked){
		this.is_locked = is_locked;
		$('#lock_button').text(is_locked ? 'Unlock room' : 'Lock room');
	    }
	    console.log(code,is_locked);
	},
	add_message: function(code,message){
	    var user = chatApp.users.get(code);
	    var nick = user.get('nick');
	    chatApp.messages.add({code:code,
				  nick:nick,
				  message:message,
				  log:false});
	},
	
	log: function(message){
	    chatApp.messages.add({code:'',
				  nick:'LOGGER',
				  message:message,
				  log: true});
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
		  function(){chatApp.log('Connected to main server');
			     if(enable_logging && console)
			     	 console.log('online');},
		  chatClient);
    chatClient.on('ondisconnect',
		  function(){chatApp.log('Connection lost with main server');
			     if(enable_logging && console)
				 console.log('offline');},
		  chatClient);
    chatClient.on('onmessage',
		  function(data){
		      if(enable_logging && console)
			  console.log('response', data);
		      switch(data.type){
			  case 'heartbeat':
			  break;
			  case 'connected_to_room':
			  chatApp.log('Connected to room ' + data.room_code);
			  chatApp.log('Current user data: ' + data.user_code + ' - ' + data.user_nick);
			  chatApp.room_code = data.room_code;
			  chatApp.user_code = data.user_code;
			  chatApp.user_nick = data.user_nick;
			  chatApp.add_user(data.user_code, data.user_nick);
			  chatApp.router.navigate(data.room_code);
			  break;
			  
			  case 'user_data':
			  chatApp.log('New user data: ' + data.code + ' - ' + data.nick);
			  chatApp.add_user(data.code, data.nick);
			  break;
			  case 'room_data':
			  chatApp.log('New room data: ' +
				      data.code +
				      ' Is room locked(' + data.is_locked + ')' );
			  chatApp.update_room(data.code, data.is_locked)
			  
			  case 'user_removed':
			  chatApp.log('Remove user: ' + data.code);
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

    $('#lock_button').click(function(){
	chatClient.send_message({type: chatApp.is_locked?'unlock_room' : 'lock_room',
				 value: chatApp.room_code});});
    
});

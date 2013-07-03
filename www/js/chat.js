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
		$('#lock_button').toggleClass('btn-success').toggleClass('btn-danger');

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
	    
            // keep the scroll to bottom
            $("body").stop().animate({
		scrollTop: $("body")[0].scrollHeight
            }, 800);
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

    chatApp.CurrentUserView = Backbone.View.extend({
	initialize: function(options){
	    _.bindAll(this);
	    this.model.bind('add', this.render);
	    this.model.bind('remove', this.render);
	    this.model.bind('change', this.render);
	    this.standard_template_text = $('#user_nick_template').html();
	    this.edit_template_text = $('#user_nick_edit_template').html();
	    this.render();
	},
	render: function(){
	    this.$el.html(
		Mustache.render(this.standard_template_text, {nick:this.model.get('nick')}));

	    $("#current_user_nick_button").click(_.bind(this.render_edit, this));
			      
	    $("#main_input").focus();
	    
	},
	edit_nick: function(){
	    var nick = $("#edit_nick_input").val();
	    chatClient.send_message({type: 'rename',
				     value: nick});
	    this.render();
	},
	render_edit: function(){
	    this.$el.html(
		Mustache.render(this.edit_template_text, {nick:this.model.get('nick')}));
	    $("#edit_nick_cancel_button").click(_.bind(this.render, this));
	    $("#edit_nick_ok_button").click(_.bind(this.edit_nick, this));
	    
	    $('#edit_nick_input').focus()
		.keypress(_.bind(function(e){
		    var k = e.which || e.keyCode;
		    if(e.type=='keypress' && k==13)
			this.edit_nick();
		}, this));

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
    
    chatApp.currentUserView = 
    
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
			  chatApp.users.reset();
			  chatApp.add_user(data.user_code, data.user_nick);
			  
			  //Change user nick on ui
			  chatApp.currentUserView = new chatApp.CurrentUserView({
			      model: chatApp.users.get(chatApp.user_code),
			      el: '#current_user_nick_button'});
			  
			  chatApp.router.navigate(data.room_code);
			  $('#main_input').focus()
			  break;
			  
			  case 'user_data':
			  chatApp.log('New user data: ' + data.code + ' - ' + data.nick);
			  if (data.code == chatApp.user_code){
			      chatApp.user_code = data.code;
			      chatApp.user_nick = data.nick;
			      //Change user nick on ui
			      $("#current_user_nick").text(chatApp.user_nick);
			  }
			      
			  chatApp.add_user(data.code, data.nick);
			  break;

			  case 'room_data':
			  chatApp.log('New room data: ' +
				      data.code +
				      ' Is room locked(' + data.is_locked + ')' );
			  chatApp.update_room(data.code, data.is_locked)
			  break;

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
    function send_message(){
	var main_input = $('#main_input');
	chatClient.send_message({type: 'message',
				 value:main_input.val()});
	main_input.val('');
	
    }    
    $('#send_button').click(send_message);
    $('#main_input').keypress(function(e){
	var k = e.which || e.keyCode;
	if(e.type=='keypress' && k==13)
	    send_message();
    });

    $('#lock_button').click(function(){
	chatClient.send_message({type: chatApp.is_locked?'unlock_room' : 'lock_room',
				 value: chatApp.room_code});});

    $('#help_button').click(function(){introJs().start();});
    
});

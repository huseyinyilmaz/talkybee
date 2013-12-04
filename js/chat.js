(function(){
    "use strict";

    $(function(){
	window.enable_logging = true;
	//////////////////////
	// Create namespace //
	//////////////////////
	var logger = {
            log:function(msg1,msg2){
		if(enable_logging && console)
                    console.log('chatApp: ', msg1, msg2);
            }
	};
    
    var chatApp = {
	room_code: '',
	user_code: '',
	user_nick: '',
	is_locked: false,
	add_user: function(code, nick){
            // Existing users will be ignored
            chatApp.users.add(
                {id: code,
                 nick: nick,
                 is_current_user: code==this.user_code},
                {merge: true});
            if(code==this.user_code){
                var user_list = chatApp.users.where({is_current_user: true, id: code});
                if(user_list){
                    var user = user_list[0];
                    if(chatApp.currentUserView===undefined){
                        chatApp.currentUserView = new chatApp.CurrentUserView({
                            model: user,
                            el:'#current_user_nick_button_container'});
                    }else{
                    }
                }
            }
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
            // keep the scroll to bottom
            this.$el.stop().animate({
		scrollTop: this.$el[0].scrollHeight
            }, 800);
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
            chatApp.client.rename(nick);
            //XXX: this must be done after data came back
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
    
    ////////////////////
    // Event handlers //
    ////////////////////
    function init(room_code){
	chatApp.client = publicatorChat.get_client(room_code);
	chatApp.client.onopen(
            _.bind(
		function(res){
                    chatApp.log('Connected to room ' + res.room_code);
                    this.room_code = res.room_code;
                    this.user_code = res.user_code;
                    this.user_nick = res.user_nick;
                },
		chatApp));
        chatApp.client.oninfo(
            _.bind(
		function(data){
                    switch(data.type){
                    case 'user_change':
                        _.each(chatApp.client.users,
                               function(user){
                                   chatApp.add_user(user.code, user.nick);
                               });
                        break;
                    default:
                        console.log('chat-Invalid info case', data);
                    }
                    
                    chatApp.log('info:' + data.type);
                    
                },
		chatApp));
        chatApp.client.onmessage(
            _.bind(
		function(data){
                    chatApp.add_message(data.code, data.data);
                },
		chatApp));
        
    }
    function send_message(){
	var main_input = $('#main_input');
	chatApp.client.send_message(main_input.val());
	main_input.val('');}
    
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


    ////////////
    // Router //
    ////////////
    chatApp.Router = Backbone.Router.extend({
	routes: {
            '': 'init',
            ':room_code': 'start_room'},
	init: function(){
            var room_code = Math.random().toString(36).substring(7);
            chatApp.log("No room code!, New room code: " + room_code);
            chatApp.router.navigate(room_code);
	},
	start_room: init});
    chatApp.router = new chatApp.Router();
    Backbone.history.start();
    
});
}());

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
	update_user: function(code, nick){
            // Existing users will not be duplicated
            chatApp.users.add(
                {id: code,
                 nick: nick,
                 is_current_user: code==this.user_code},
                {merge: true});
            if(code==this.user_code){
                var user = chatApp.users.get(code);
                if(user && chatApp.currentUserView===undefined){
                    chatApp.currentUserView = new chatApp.CurrentUserView({
                        model: user,
                        el:'#current_user_nick_button_container'});
                }else{
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
                                  type:'message'});
	},

	
	log: function(message){
            chatApp.messages.add({code:'',
                                  nick:'LOGGER',
                                  message:message,
                                  type:'log'});
        },
	error: function(message){
            chatApp.messages.add({code:'',
                                  nick:'LOGGER',
                                  message:message,
                                  type: 'error'});
            
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
            var msg = this.model.toJSON();
            this.$el.html(
		Mustache.render(
                    this.template_text,
                    {code:msg.code,
                     nick:msg.nick,
                     message:msg.message,
                     type:msg.type,
                     is_message:function(){return this.type=='message';},
                     is_log:function(){return this.type=='log';},
                     is_error:function(){return this.type=='error';}
                    }
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
        // publicator.set_host('localhost:8766');
        publicator.set_host('www.talkybee.com:8766');
	chatApp.client = publicator.chat.get_client(room_code);
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
		function(data){
                    switch(data.type){
                    case 'user_change':
                        _.each(chatApp.client.users,
                               function(user, user_code){
                                   chatApp.update_user(user.code, user.nick);
                               });
                        var removed_users = chatApp.users.filter(function(model){
                            return chatApp.client.users[model.id] == undefined;
                        });
                        chatApp.users.remove(removed_users);
                        break;
                    default:
                        console.log('chat-Invalid info case', data);
                    }
                    
                    console.log('info:' + data.type);
                    
                });
        chatApp.client.onmessage(
		function(data){
                    chatApp.add_message(data.code, data.data);
                });
        chatApp.client.onerror(
		function(data){
                    //XXX disable everything
                    if(data.data==='invalid_channel_code'){
                        chatApp.error('Current room name is invalid. Channel codes must be consist of consist of only numbers, leters and underscore \'_\' character.');
                    }else{
                        console.error(data);
                        alert('There was a problem with your session.' +
                              ' Please refresh your page.');
                    }
                });
        
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
            chatApp.router.navigate(room_code, {trigger: true, replace: true});
	},
	start_room: init});
    chatApp.router = new chatApp.Router();
    Backbone.history.start();
    
});
}());

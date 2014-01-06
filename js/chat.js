(function(){
    "use strict";
    // var _PUBLICATOR_SERVER = 'localhost:8766';
    var _PUBLICATOR_SERVER = 'www.talkybee.com:8766';
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

        function get_time_string(){
            var date = new Date();
            var hour = date.getHours();
            var minute = date.getMinutes();
            return (hour<10 ? '0':'') + hour + ':' + (minute<10?'0':'') + minute;
            
        }

        function linkify(string, includeW3, target){
            var url_regex = /\(?(?:(http|https|ftp):\/\/)?(?:((?:[^\W\s]|\.|-|[:]{1})+)@{1})?((?:www.)?(?:[^\W\s]|\.|-)+[\.][^\W\s]{2,4}|localhost(?=\/)|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})(?::(\d*))?([\/]?[^\s\?]*[\/]{1})*(?:\/?([^\s\n\?\[\]\{\}\#]*(?:(?=\.)){1}|[^\s\n\?\[\]\{\}\.\#]*)?([\.]{1}[^\s\?\#]*)?)?(?:\?{1}([^\s\n\#\[\]]*))?([\#][^\s\n]*)?\)?/gi;
            /*source: https://github.com/uudashr/jquery-linkify/blob/v2.1/jquery.linkify.js*/
            string = string.replace(url_regex, function(captured) {
                var uri;
                if (!((captured.toLowerCase().indexOf("http://") === 0)
                    ||(captured.toLowerCase().indexOf("https://") === 0)
                    ||(captured.toLowerCase().indexOf("ftp://") === 0))){
              uri = "http://" + captured;
            } else {
              uri = captured;
            }
            return "<a href=\"" + uri+ "\" target=\"_blank\">" + captured + "</a>";
          });
          
          return string;
        }

    var chatApp = {
        nick_regex: /^[a-z0-9_]+$/i,
        nick_size: 20,
	room_code: '',
	user_code: '',
	user_nick: '',
	is_locked: false,

	update_user: function(code, nick){
            var previous_user = chatApp.users.get(code);
            if(previous_user && previous_user.get('nick') !== nick){
                chatApp.log(previous_user.get('id') + ' changed his nick to ' + nick);
            }else if(!previous_user){
                chatApp.log('New user joined chat (' + nick + ')');
            }
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
	add_message: function(code,message){
            var user = chatApp.users.get(code);
            var nick = user.get('nick');
            chatApp.messages.add({code:code,
                                  nick:nick,
                                  message:message,
                                  time: get_time_string(),
                                  type:'message'
                                 });
            $.titleAlert('######################', {
                requireBlur:true,
                stopOnFocus:true,
                duration:0,
                interval:500});
	},

	
	log: function(message){
            chatApp.messages.add({code:'',
                                  nick:'LOGGER',
                                  message:message,
                                  time: get_time_string(),
                                  type:'log'});
        },
	error: function(message){
            chatApp.messages.add({code:'',
                                  nick:'LOGGER',
                                  message:message,
                                  time: get_time_string(),
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
	tagName: 'tr',
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
                     time:msg.time,
                     is_current_user: msg.code === chatApp.user_code,
                     is_message:function(){return this.type=='message';},
                     is_log:function(){return this.type=='log';},
                     is_error:function(){return this.type=='error';},
                     linkify: function (){
                         return function (text, render){
                             return linkify(render(text),{punct_regexp: null});
                         };
                     }                     
                    }
		)
            );
            return this;
	}
    });

    chatApp.MessagesView = Backbone.View.extend({
	initialize: function(options){
            this.$scroll_el = $(options.scroll_el);
            _.bindAll(this);
            this.collection.bind('add', this.addMessage);
	},
        scrollToBottom: function(){
            // keep the scroll to bottom
            this.$scroll_el.stop().animate({scrollTop: this.$scroll_el[0].scrollHeight}, 800);
        },
	addMessage: function(model){
            var messageView = new chatApp.MessageView({model:model});
            messageView.render();
            this.$el.append(messageView.$el);
            this.scrollToBottom();
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
            if(nick.match(chatApp.nick_regex) &&
               nick.length <= chatApp.nick_size &&
               nick.length > 0){
                chatApp.client.rename(nick);
                $.cookie('talkybee_nick', nick);
                $('.msg-nick-' + chatApp.user_code).text(nick);
            }else{
                chatApp.error("Error - Nick should be consist of numbers, letters or _ character." +
                             "Also its length should be smaller than " + chatApp.nick_size);
            }
            
            //XXX: this must be done after data came back
            // this.render();

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
            el: '#messages_container',
            scroll_el: '#messages_scroll_container'
        });

        var $writing_message_container = $("#writing_message_container");
        var writing_message_template_text = $("#writing_message_template").html();

        function update_writing_message(user_list){
            var value = '';
            if(user_list.length > 0){
                value = Mustache.render(
                    writing_message_template_text,
                    {name_list_str: _.pluck(user_list,
                                            'nick').join(', ')});
                
            }
            $writing_message_container.html(value);
            
            chatApp.messagesView.scrollToBottom();
        }
    ////////////////////
    // Event handlers //
    ////////////////////
    function init(room_code){
        publicator.set_host(_PUBLICATOR_SERVER);
	chatApp.client = publicator.chat.get_client(room_code);
	chatApp.client.onopen(
            _.bind(
		function(res){
                    chatApp.log('Connected to room ' + res.room_code);
                    this.room_code = res.room_code;
                    this.user_code = res.user_code;
                    this.user_nick = res.user_nick;
                    var cookie_nick = $.cookie('talkybee_nick');
                    if(cookie_nick){
                        chatApp.client.rename(cookie_nick);
                    }
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
                            return chatApp.client.users[model.id] === undefined;
                        });
                        chatApp.users.remove(removed_users);
                        break;
                    case 'writing_change':
                        console.log('writing change');
                        var current_user_code = chatApp.user_code;
                        var writing_user_list = _.filter(chatApp.client.users,
                                                         function(user, user_code){
                                                             return user.is_writing && current_user_code !== user_code;
                                                         });
                        update_writing_message(writing_user_list);
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
        var msg = main_input.val();
        if(msg)
            chatApp.client.send_message(msg);
	main_input.val('');}
    
    $('#send_button').click(send_message);
    $('#main_input').keypress(function(e){
	var k = e.which || e.keyCode;
	if(e.type=='keypress' && k==13){
            send_message();
        }else{
            chatApp.client.start_writing();
        }
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

$('.ui.form')
  .form({
    username: {
      identifier  : 'user-name',
      rules: [
        {
          type   : 'empty',
          prompt : 'Please enter your user name'
        }
      ]
    },
    password: {
      identifier : 'password',
      rules: [
        {
          type   : 'empty',
          prompt : 'Please enter your password'
        }
      ]
    }
  }).


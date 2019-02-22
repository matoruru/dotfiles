# To remove the message at fish login
set fish_greeting

# To use 'npm i -g' wituout sudo
if test -d ~/.npm-global
   set PATH ~/.npm-global/bin/ $PATH
end

if test -d ~/miniconda3
   source ~/miniconda3/etc/fish/conf.d/conda.fish
end

# Set default editor and options
set default_editor 'vim'
set editor_options '-p'
alias  e="$default_editor $editor_options"

# Set alias
alias mocp='mocp -T mytheme'
alias showtodo="fish ~/repositories/matoruru/gentoo-tools/showtodo.fish"
alias edittodo="vim ~/todolist.txt;showtodo"
alias su="sudo su - -m"
alias scrot="scrot -q 100"
alias n="sudo n"

# funny commnad
alias えぃｔ="exit"
alias ゔぃｍ="vim"

# Set tab step
tabs 3

# Set yimmy theme
. ~/repositories/theme-yimmy/fish_prompt.fish

# switch apatche server on/off
function httpon
   sudo /etc/init.d/apache2 start
end

function httpoff
   sudo /etc/init.d/apache2 stop
end

# switch docker on/off
function dockeron
   sudo rc-service docker start
end

function dockeroff
   sudo rc-service docker stop
end

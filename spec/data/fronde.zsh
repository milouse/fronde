#compdef fronde
#autoload


(( $+functions[__fronde_new] )) ||
__fronde_new(){
    _arguments \
        '(-a --author)'{-a,--author}':AUTHOR:' \
        '(-l --lang)'{-l,--lang}':LOCALE:' \
        '(-o --output)'{-o,--output}':FORMAT:' \
        '(-t --title)'{-t,--title}':TITLE:' \
        '(-v --verbose)'{-v,--verbose}
}

(( $+functions[__fronde_open] )) ||
__fronde_open(){
    _arguments \
        '(-a --author)'{-a,--author}':AUTHOR:' \
        '(-l --lang)'{-l,--lang}':LOCALE:' \
        '(-t --title)'{-t,--title}':TITLE:' \
        '(-v --verbose)'{-v,--verbose} \
        '1:file:_files -g \*.org'
}

(( $+functions[__fronde_build] )) ||
__fronde_build(){
    _arguments \
        '(-f --force)'{-f,--force}
}


(( $+functions[__fronde_help] )) ||
__fronde_help(){
    _arguments \
        "1:command:((new\:'Initialize your Fronde instance (you just need to do it once).' update\:'Update Fronde dependency (to be run once in a while).' preview\:'Start a test web server to preview your website on http://127.0.0.1:5000' open\:'Open or create an org file.' build\:'Compile your org files to HTML or gemtext.' publish\:'Push local changes to your public web server.' help\:'Alias for the -h switch.' ))"
}

local state

_arguments -C \
    '(-)-h[help]' \
    '(-)-V[version]' \
    "1:command:((new\:'Initialize your Fronde instance (you just need to do it once).' update\:'Update Fronde dependency (to be run once in a while).' preview\:'Start a test web server to preview your website on http://127.0.0.1:5000' open\:'Open or create an org file.' build\:'Compile your org files to HTML or gemtext.' publish\:'Push local changes to your public web server.' help\:'Alias for the -h switch.' ))" \
    '*::arg:->args'

if [ "$state" = args ]; then
    _call_function ret __fronde_${words[1]}
fi

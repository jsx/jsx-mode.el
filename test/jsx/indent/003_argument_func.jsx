document.addEventListener("DOMContentLoaded", function(e : Event) : void {  // 0
    if (true) {  // 4
        log "true";  // 8
    }  // 4
});  // 0

document.addEventListener(  // 0
    "DOMContentLoaded",  // 4
    function(e : Event) : void {  // 4
        if (true) {  // 8
            log "true";  // 12
        }  // 8
    });  // 4

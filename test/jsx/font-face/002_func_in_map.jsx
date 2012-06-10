/*
    var: font-lock-keyword-face
    obj: font-lock-variable-name-face

    a: font-lock-function-name-face
    function: font-lock-keyword-face
    void: font-lock-type-face
    log: font-lock-builtin-face
    "a": font-lock-string-face

    b: font-lock-function-name-face
    function: font-lock-keyword-face
    void: font-lock-type-face
    log: font-lock-builtin-face
    "}": font-lock-string-face

    null: font-lock-constant-face
*/

     var obj = {
         a: function() : void {
             log "a";
         },
         b: function() : void { log "}"; }, c: func,
         d: null
     };

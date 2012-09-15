/*
    function: font-lock-keyword-face
    foo: font-lock-function-name-face
    a: font-lock-variable-name-face
    int: font-lock-type-face
    b: font-lock-variable-name-face
    Map: font-lock-type-face
    string: font-lock-type-face
    function: font-lock-keyword-face
    int: font-lock-type-face
    string: font-lock-type-face
    void: font-lock-type-face

    return: font-lock-keyword-face
    function: font-lock-keyword-face
    a: font-lock-variable-name-face
    int: font-lock-type-face
    b: font-lock-variable-name-face
    string: font-lock-type-face
    void: font-lock-type-face
    log: font-lock-builtin-face
    "foo": font-lock-string-face

    override: font-lock-keyword-face
    function: font-lock-keyword-face
    bar: font-lock-function-name-face
    a: font-lock-variable-name-face
    int: font-lock-type-face
    b: font-lock-variable-name-face
    string: font-lock-type-face
    function: font-lock-keyword-face
    int: font-lock-type-face
    string: font-lock-type-face
    void: font-lock-type-face

    return: font-lock-keyword-face
    function: font-lock-keyword-face
    a: font-lock-variable-name-face
    int: font-lock-type-face
    b: font-lock-variable-name-face
    string: font-lock-type-face
    void: font-lock-type-face
    log: font-lock-builtin-face

    var: font-lock-keyword-face
    func: font-lock-variable-name-face
    function: font-lock-keyword-face
    void: font-lock-type-face
*/


    function foo(a : int,
        b : Map.<string>) : function(: int,
    : string) : void {
        return function(a : int, b : string) : void { log "foo"; };
    }

    override function bar(a : int, b : string) : function(:int, :string) : void {
        return function(a : int, b : string) : void {
            log b;
        };
    }

    var func : function() : void;

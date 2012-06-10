/*
    var: font-lock-keyword-face
    foo: font-lock-variable-name-face
    Foo: font-lock-type-face

    var: font-lock-keyword-face
    array: font-lock-variable-name-face
    string: font-lock-type-face

    var: font-lock-keyword-face
    func: font-lock-variable-name-face
    function: font-lock-keyword-face
    void: font-lock-type-face

    var: font-lock-keyword-face
    strMap: font-lock-variable-name-face
    Map: font-lock-type-face
    string: font-lock-type-face

    var: font-lock-keyword-face
    fooMap: font-lock-variable-name-face
    Map: font-lock-type-face
    Foo: font-lock-type-face

    var: font-lock-keyword-face
    funcMap: font-lock-variable-name-face
    Map: font-lock-type-face
    function: font-lock-keyword-face
    void: font-lock-type-face
*/

    var foo: Foo;
    var array = [] : string[];
    var func = function() : void {};
    var strMap = {} : Map.< string >;
    var fooMap = {} : Map.<Foo>;
    var funcMap = {} : Map.<function() : void>;

function foo1(a : string,  // 0
              b : string) : void {  // 14
    log a + b;  // 4
}  // 0

function foo2(  // 0
    a : string,  // 4
    b : string   // 4
) : void {  // 0
    log a + b;  // 4
}  // 0

function bar1(a : string,  // 0
              b : string) : function (:string,  // 14
                                      :string) : void {  // 38
    return function(c : string, d : string) : void {  // 4
        log a + b + c + d;  // 8
    };  // 4
}  // 0

function bar2(  // 0
    a : string,  // 4
    b : string  // 4
) : function (  // 0
    :string,  // 4
    :string  // 4
) : void {  // 0
    return function(c : string, d : string) : void {  // 4
        log a + b + c + d;  // 8
    };  // 4
}  // 0

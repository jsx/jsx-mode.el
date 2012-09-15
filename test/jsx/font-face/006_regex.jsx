/*
    //comment: font-lock-comment-face
    /: font-lock-string-face
    fo\/o: font-lock-string-face
    /: font-lock-string-face
    igm: font-lock-string-face
    /: font-lock-string-face
    ": font-lock-string-face
    /: font-lock-string-face
    var: font-lock-keyword-face
    regex: font-lock-variable-name-face
    /: font-lock-string-face
    a: font-lock-string-face
    /: font-lock-string-face
    /: font-lock-string-face
    [/*]: font-lock-string-face
    /: font-lock-string-face
    /* /);: font-lock-comment-face
*/

    //comment
    str.match(/fo\/o/igm);
    str.match(/fo\\
              o/igm);
    str.match(/"/);
    var regex = /a/;
    /[/*]/;
    str.match(/* /);

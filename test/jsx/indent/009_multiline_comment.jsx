/*  // 0
 * comment1  // 1
 /*  // 1
 * comment2  // 1
 */  // 1

/*  // 0
  /* // 2
  comment  // 2
*/  // 0

/*  // 0
 *  // 1
 comment  // 1
*/  // 0

if (true) {  // 0
    /*  // 4
     * comment1  // 5
     /*  // 5
     * comment2  // 5
     */  // 5

    /*  // 4
      /* // 6
      comment  // 6
    */  // 4

    /*  // 4
     *  // 5
     comment  // 5
    */  // 4
}  // 0

switch (0) {
case 0:
    break;
    // case 1:  // 4
    //     break;  // 4
}

if (true)  // 0
    log "inner-if";  // 4
else
    log "inner-else";  // 4
log "outer-if"; // 0

if (true)
    // 4
    log "if";  // 4

for (var i = 0; i < 3; i++)  // 0
    log i;  // 4

while (true)  // 0
    log "while";  // 4

do  // 0
    log "do-while";  // 4
while (i-- > 0);  // 0

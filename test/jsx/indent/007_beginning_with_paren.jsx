(function() {  // 0
	(function() { log "function"; })();  // 4
})();  // 0

if (true) {  // 0
	(function() { log "if"; })();  // 4
}  // 0

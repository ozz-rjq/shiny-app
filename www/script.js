var popup = document.querySelector(".modal-content");
var popupOpen = document.querySelector(".js-popup-open");

var screenBlackout = document.querySelector(".screen-blackout");

popupOpen.addEventListener("click", function(event){
	event.preventDefault();
	if(!popup.classList.contains("show")){
		popup.classList.add("show");
		screenBlackout.classList.add("show");
	}
});

screenBlackout.addEventListener("click", function(event){
	event.preventDefault();
	if(screenBlackout.classList.contains("show"))
		screenBlackout.classList.remove("show");

	if(popup.classList.contains("show"))
		popup.classList.remove("show");
});

window.addEventListener("keydown", function(event){
	if(event.keyCode === 27){
		if(screenBlackout.classList.contains("show"))
			screenBlackout.classList.remove("show");

		if(popup.classList.contains("show"))
			popup.classList.remove("show");
	}
});
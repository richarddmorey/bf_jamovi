function check_answer(parent, precision)
{
	var ans = parent.querySelector(".bfx-answer").innerHTML;
	var resp = parent.querySelector(".bfx-response").value;
	var err = Math.abs(ans - resp);
	var feedback;
	if(err < precision){
		feedback = "Correct!"
	}else{
		feedback = "Incorrect. Try again."
	}

	parent.querySelector(".bfx-feedback").innerHTML = feedback;
}
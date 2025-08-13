/**
* Implementation of the Diamond exercise
*/
component {
	
	/**
	* @returns An array of strings
	*/
	array function rows( letter ) {
		if (letter == "A") return ["A"];
		
		// find starting index of given letter and create middle row
		var offset = asc("A");
		var start = asc(letter) - offset;
		var index = start;
		var padding = "";
		var center = repeatString(" ", start * 2 - 1);
		var output = [letter & center & letter];

		// add additional rows for previous letters until reaching "A"
		while (index > 1) {
			index = index - 1;
			letter = chr(offset + index);
			padding = repeatString(" ", start - index);
			center = repeatString(" ", index * 2 - 1);
			enhance(output, padding & letter & center & letter & padding);
		}

		// add the first and last "A" rows to the final output
		padding = repeatString(" ", start);
		enhance(output, padding & "A" & padding);
		return output;
	}
	
	function enhance(existing, newRow) {
		existing.prepend(newRow);
		existing.append(newRow);
	}
}